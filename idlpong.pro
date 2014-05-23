; docformat = 'rst'

;+
;   The purpose of the programs is to implement a simple Pong game with
; IDL direct graphics.
;
;   Select a difficulty level to start the game at the main menu. 
; Left click to serve the ball. Move the cursor inside the game window
; to move the bat. Right click to pause. Middle button or pressing both
; left and right buttons to return to the main menu. Alternatively, use
; arrow keys to move left/right. Hit space bar to serve the ball, though
; the keyboard control is not smooth.
;
; :Author:
;   ywang
;
; :Examples:
; ::
;
;   idlpong
;
; :History:
;   Created by:    ywang @ Fri 12 Nov 2010
;   Last modified: ywang @ Mon 29 Nov 2010
;-

;+
; Main loop of the game
;   
; :Params:
;   info : in, required, type=struct
;       The game info struct variable
;   idxsel : in, required, type=int
;       The user selection of the difficulty id
;-
PRO IDLPong_Main_Loop, info, idxsel

    ; Set up the difficulty level
    difflvl = (idxsel+1)*2.0
    ;
    ; Ball state
    BALL_STATIONARY_ON_PLAYER = 0
    BALL_HIT_BY_PLAYER = 1
    BALL_HIT_BY_COMPUTER = 2
    ;
    ; A.I related variables and settings
    ; difflvl is from 20 to 100
    weight_hit = (difflvl*100000.)^(1.0/3.0)/130.0
    weight_ball = 1.0 - weight_hit
    ;
    ; maximum of the ball velocity
    ball_vx_max = 1.0 + difflvl*0.1
    ball_vy_max = 0.5 + difflvl*0.1
    ; Aggressive attack by taking a random directional movement when the ball
    ; is touching the computer's bat
    strategy_done = 0
    end_pos_factor = -1.0
    ini_pos_factor = 1.0
    dist_when_move_min = 0.02
    dist_when_move_max = 0.5-difflvl*0.04
    dist_when_move = dist_when_move_max
    aggressive_chance = 0.5 + difflvl*0.05
    scale_factor_base = 2.0
    scale_factor_range = 0.5 - difflvl*0.05
    power_factor_base = 1.5-difflvl*0.1
    ;
    ; The struct variables of the bats and ball
    ; bats
    pbat = {IDLPONG_BAT, 	$
        name : 'player', 	$
        score : 0LL,        $
        x : 0.5, 			$
        y : 0.1, 			$
        xsize : 0.1, 		$
        ysize : 0.01, 		$
        vx : 0.0, 			$
        vy : 0.0,			$
        boost : 0.008		$
        }

    cbat = {IDLPONG_BAT, 	$
        name : 'computer', 	$
        score : 0LL,        $
        x : 0.5, 			$
        y : 0.9, 			$
        xsize : 0.1, 		$
        ysize : 0.01, 		$
        vx : 0.0, 			$
        vy : 0.0,			$
        boost : 0.008		$
        }

    ; the pong
    ; Define the symbol as a filled circle
    phi = Findgen(36) * (!PI * 2 / 36.)
    phi = [ phi, phi(0) ]
    UserSym, Cos(phi), Sin(phi), /Fill
    ; Vars of the ball
    ball = {IDLPONG_BALL, 	$
        x : 0.5, 			$
        y : 0.5,			$
        scale : 1.0,		$
        r : 1.0,			$
        vx : 0.0,			$
        vy : 0.0,			$
        state : 0			$
        }
    ; The radius is half of the char size, divided by ysize to normalize it
    ball.r = ball.scale*!D.X_Ch_Size/2.0/info.ysize
    ; The ball's initial y position is center on top of the player's bat
    ball.y = pbat.y + pbat.ysize/2.0 + ball.r

    ;
    ; ######################################################################
    ; Main loop
    ; ######################################################################
    ;
    ; Time interval
    dt = 0.0
    ;
    REPEAT BEGIN
        ;
        ; Record the start time
        t = systime(1)

        ; -----------------------------------------------------------------------
        ; Check mouse status
        ; -----------------------------------------------------------------------
        Cursor, xx, yy, /NoWait, /Normal

        ; -----------------------------------------------------------------------
        ; Check keyboard input
        ; -----------------------------------------------------------------------
        keyins = Get_KBrd(0, /Key_Name)

        IF info.keyboard THEN BEGIN
            ;
            ; Process the keyboard input
            IF keyins EQ 'LEFT' THEN BEGIN
                pbat.x = pbat.x - 0.02 > 0.0
                pbat.vx = dt EQ 0.0?0.0:0.02/dt
            ENDIF ELSE IF keyins EQ 'RIGHT' THEN BEGIN
                pbat.x = pbat.x + 0.02 < 1.0
                pbat.vx = dt EQ 0.0?0.0:0.02/dt
            ENDIF
            ;
        ENDIF ELSE BEGIN
            ;
            ; Process the cursor position
            If xx GE 0 AND xx LE 1 THEN BEGIN
                ; If cursor is within the game window
                ; Calculate player speed
                dx = xx-pbat.x
                pbat.vx = dt EQ 0.0?0.0:dx/dt
                pbat.x = xx
            ENDIF ELSE BEGIN
                ; If cursor is out of the game window, 
                ; put the paddle at either edges according to the last velocity
                IF pbat.vx GT 0 THEN pbat.x=1.0 ELSE IF pbat.vx LT 0 THEN pbat.x=0.0
            ENDELSE
            ;
        ENDELSE


        ; -----------------------------------------------------------------------
        ; Process the mouse button input
        ; -----------------------------------------------------------------------
        ; Middle button or left and right button together for quit
        IF !Mouse.Button EQ 2 OR !Mouse.Button EQ 5 THEN isOver=1 ELSE isOver=0

        ; Right Button Boss Key
        IF !Mouse.Button EQ 4 THEN BEGIN
            WSet, info.wid
            Erase
            Contour, Dist(40,50), nlevel=10, Position=[0.1,0.1,0.9,0.9], $
                YTitle='left click to continue...'
            Cursor, xx, yy, /DOWN
            t = systime(1)
        ENDIF
        ;
        ; -----------------------------------------------------------------------
        ; Calculate the ball's position of the next frame (after dt)
        ; -----------------------------------------------------------------------
        ;
        ; If ball state is stationary, ball is staying with the player's bat
        IF ball.state EQ BALL_STATIONARY_ON_PLAYER THEN BEGIN
            ball.x = pbat.x
            ; Start the ball if player pressed left button
            IF !Mouse.Button EQ 1 OR keyins EQ ' ' THEN BEGIN
                ball.vy = pbat.boost*100.
                ball.vx = 0.0
                ball.state = BALL_HIT_BY_PLAYER
            ENDIF
            hit_x_computer = ball.x
        ENDIF 

        ; Save the old position of the ball
        ball_old_x = ball.x
        ball_old_y = ball.y
        ;
        ; If ball is moving, process the new position according to its speed
        IF ball.state NE BALL_STATIONARY_ON_PLAYER THEN BEGIN
            ; New candidate position of the ball
            new_x = ball.x + dt*ball.vx
            new_y = ball.y + dt*ball.vy
            ;
            ; Calculate the ball's trajectory
            ball_A = new_y - ball.y
            ball_B = ball.x - new_x
            ball_C = ball_A*ball.x + ball_B*ball.y
            ;
            ; -------------------------------------------------------------------
            ; Process the ball's trajectory for checking if it hits any bats or walls 
            ; -------------------------------------------------------------------
            ;
            ; If player bat's y is within the ball's y trajectory 
            hit_x_player = (ball_C-ball_B*pbat.y)/ball_A
            miny = new_y < ball.y < pbat.y
            maxy = new_y > ball.y > pbat.y
            IF miny NE pbat.y AND maxy NE pbat.y THEN BEGIN
                ; player bat trajectory
                ; If hit_x_player is within the player bat range, its a hit
                IF hit_x_player GE pbat.x-pbat.xsize/2.0 AND hit_x_player LE pbat.x+pbat.xsize/2.0 THEN BEGIN
                    ball.vy = -ball.vy
                    ; The ball gains x direction velocity from the bats's movement
                    ball.vx = ball.vx + (pbat.vx GT 0.0?1.0:-1.0)*(1.0-EXP(-ABS(pbat.vx)))*ball_vx_max
                    ; We cap the velocity so it is playable by human
                    ball.vx = -ball_vx_max > ball.vx < ball_vx_max
                    ; Process the bouncing of the hit
                    bounce_y = ABS((ball.y-new_y) - (ball.y-pbat.y))
                    new_y = pbat.y + (new_y LT pbat.y?bounce_y:-bounce_y)
                    ball.state = BALL_HIT_BY_PLAYER
                    ; The computer needs to prepare its strategy after a ball is hit by player
                    strategy_done = 0
                ENDIF
            ENDIF

            ; Computer bat trajectory
            ; If computer bat's y is within the ball's y trajectory 
            hit_x_computer = (ball_C-ball_B*cbat.y)/ball_A
            miny = new_y < ball.y < cbat.y
            maxy = new_y > ball.y > cbat.y
            IF miny NE cbat.y AND maxy NE cbat.y THEN BEGIN
                ; computer bat trajectory
                ; If hit_x_computer is within the computer bat range, its a hit
                IF hit_x_computer GE cbat.x-cbat.xsize/2.0 AND hit_x_computer LE cbat.x+cbat.xsize/2.0 THEN BEGIN
                    ball.vy = -ball.vy
                    ball.vx = ball.vx + (cbat.vx GT 0.0?1.0:-1.0)*(1.0-EXP(-ABS(cbat.vx)))*ball_vx_max
                    IF ball.vx GT ball_vx_max THEN ball.vx = ball_vx_max ELSE IF $
                        ball.vx LT -ball_vx_max THEN ball.vx = -ball_vx_max
                    bounce_y = ABS((ball.y-new_y) - (ball.y-cbat.y))
                    new_y = cbat.y + (new_y LT cbat.y?bounce_y:-bounce_y)
                    ball.state = BALL_HIT_BY_COMPUTER
                ENDIF
            ENDIF

            ; A score is gained if the ball goes out of the horizontal boundaries
            ; Reset some variables after a score 
            IF new_y GE 1.5 THEN BEGIN
                pbat.score++
                ball.state = BALL_STATIONARY_ON_PLAYER
                new_x = pbat.x
                new_y = pbat.y + pbat.ysize/2.0 + ball.r
                ball.vx = 0.0
                ball.vy = 0.0
                strategy_done = 0
            ENDIF
            IF new_y LE -0.5 THEN BEGIN
                cbat.score++
                ball.state = BALL_STATIONARY_ON_PLAYER
                new_x = pbat.x
                new_y = pbat.y + pbat.ysize/2.0 + ball.r
                ball.vx = 0.0
                ball.vy = 0.0
                strategy_done = 0
            ENDIF
            ;
            ; Bouncing off the vertical walls
            IF new_x GE 1.0 THEN BEGIN 
                ball.vx = -ball.vx
                bounce_x = ABS((ball.x-new_x) - (ball.x-1.0))
                new_x = 1.0 - bounce_x
            ENDIF
            IF new_x LE 0.0 THEN BEGIN
                ball.vx = -ball.vx
                bounce_x = ABS((ball.x-new_x) - (ball.x-0.0))
                new_x = 0.0 + bounce_x
            ENDIF

            ; Update the ball position
            ball.x = new_x
            ball.y = new_y
            ;
        ENDIF ; end of ball trajectory processing

        ;
        ; -----------------------------------------------------------------------
        ; Computer bat logic
        ; -----------------------------------------------------------------------
        ; Prepare strategy against the player
        IF strategy_done EQ 0 THEN BEGIN
            ;
            rnd_num = RANDOMU(seed,5)

            ; The computer is always aggressive if the ball has no x velocity.
            ; Otherwise the game would be boring.
            IF ball.vx EQ 0 OR rnd_num[0] LE aggressive_chance THEN isAggressive=1 ELSE isAggressive=0

            ; Aggressive move means add x velcoity to the ball, i.e. move the bat to either 
            ; sides when the ball is touching the bat
            IF isAggressive THEN BEGIN
                ; Which direction to move
                IF rnd_num[1] GE 0.5 THEN BEGIN
                    tmp = end_pos_factor
                    end_pos_factor = ini_pos_factor
                    ini_pos_factor = tmp
                ENDIF

                ; When to move
                dist_when_move = rnd_num[2]*(dist_when_move_max-dist_when_move_min)+dist_when_move_min

                ; Solve the linear equation constants for the aggressive move
                aggressive_k = (ini_pos_factor-end_pos_factor)/dist_when_move
                aggressive_b = end_pos_factor
            ENDIF

            ; Decide the response speed, which is the main driver of the AI
            scale_factor = scale_factor_base + rnd_num[3]*scale_factor_range
            power_factor = power_factor_base + rnd_num[4]

            ; The strategy is done for this round
            strategy_done = 1
            ;
        ENDIF

        ; -----------------------------------------------------------------------
        ; Position adjustment for aggressive moves
        ; -----------------------------------------------------------------------
        dy_to_ball = cbat.y - ball.y
        IF isAggressive AND dy_to_ball LE dist_when_move AND dy_to_ball GT 0.0 THEN BEGIN
            pos_adjust_factor = aggressive_k*dy_to_ball+aggressive_b
            ; Position ajustment ranges from +/- 40% bat xsize
            pos_adjust = cbat.xsize*0.4*pos_adjust_factor
        ENDIF ELSE $
            pos_adjust = 0.0

        ; -----------------------------------------------------------------------
        ; Calculate computer bat's new position
        ; -----------------------------------------------------------------------
        ;IF ball.state EQ BALL_HIT_BY_PLAYER THEN BEGIN
        IF hit_x_computer GE 0.0 AND hit_x_computer LE 1.0 THEN BEGIN
            chase_x = hit_x_computer 
        ENDIF ELSE BEGIN 
            turns = Fix(ABS(hit_x_computer))
            IF hit_x_computer LT 0.0 THEN BEGIN 
                IF turns MOD 2 EQ 0 THEN chase_x = ABS(hit_x_computer MOD 1.0) $
                    ELSE chase_x = 1.0 - ABS(hit_x_computer MOD 1.0)
            ENDIF ELSE BEGIN
                IF turns MOD 2 EQ 1 THEN chase_x = 1.0 - (hit_x_computer MOD 1.0) $
                    ELSE chase_x = hit_x_computer MOD 1.0
            ENDELSE
        ENDELSE
        ;ENDIF ELSE $ 
        ;    chase_x = ball.x

        ; Balance the chase position based on difficulty level
        chase_x = weight_hit*chase_x + weight_ball*ball.x

        response = ((0.0>ABS((ball.y-pbat.y)/(cbat.y-cbat.ysize/2.0-ball.r-pbat.y))<1.0)/scale_factor)^ $
            power_factor
        new_x = cbat.x + (chase_x-cbat.x + pos_adjust) * response

        ; Bat moving speed
        cbat.vx = (new_x-cbat.x)/dt
        ; Update bat position
        cbat.x = new_x


        ; -----------------------------------------------------------------------
        ; Rendering the frame
        ; -----------------------------------------------------------------------
        ; Plotting on buffer frame
        WSet, info.pwid
        Erase
        IDLPong_Draw_Bat, pbat
        IDLPong_Draw_Bat, cbat
        IDLPong_Draw_Ball, ball
        IDLPong_Draw_Stats, pbat=pbat, cbat=cbat, ball=ball, $
            debugmode=info.debugmode

        ; Copy to the display window
        WSet, info.wid
        Device, Copy=[0,0,info.xsize,info.ysize,0,0,info.pwid]

        ; We want the program to run on 32 fps
        second_per_frame = 0.03125 - (systime(1)-t) 
        Wait, second_per_frame GT 0.0?second_per_frame:0.0
        ; Real time spend on this iteration
        dt = SysTime(1)-t < 0.03125
        ;
    ENDREP UNTIL isOver

END

;+
; Cleanup for the game (deleting game window)
;
; :Params:
;   info : in, required, type=struct
;       The game info struct variable
;-
PRO IDLPong_Cleanup, info

    WDelete, info.wid
    WDelete, info.pwid

END

;+
; The purpose of this routine is to plot the stats on the screen, including scores
; and various debug informations.
;
; :Keywords:
;   pbat : in, required, type=struct
;       The player bat struct variable
;   cbat : in, required, type=struct
;       The computer bat struct variable
;   debugmode : in, optional, type=boolean
;       If set, debug information will be displayed on the game window
;- 
PRO IDLPong_Draw_Stats, $
    pbat=pbat, cbat=cbat, ball=ball, $
    debugmode=debugmode

On_Error, 2

IF KEYWORD_SET(debugmode) THEN debugmode=1 ELSE debugmode=0

; Draw dividor
PLOTS, [0,1],[0.5,0.5], LineStyle=2, /Normal

; The score
XYOutS, 0.9, 0.6, StrTrim(cbat.score,2), CharThick=2.0, CharSize=2.0, Alignment=0.5, /Normal
XYOutS, 0.9, 0.4, StrTrim(pbat.score,2), CharThick=2.0, CharSize=2.0, Alignment=0.5, /Normal

IF debugmode THEN BEGIN 
    XYOutS, 0.0, 0.01, 'pbat.vx='+String(pbat.vx,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.2, 0.01, 'cbat.vx='+String(cbat.vx,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.4, 0.01, 'ball.vx='+String(ball.vx,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.6, 0.01, 'ball.vy='+String(ball.vy,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.4, 0.03, 'ball.x='+String(ball.x,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.6, 0.03, 'ball.y='+String(ball.y,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.0, 0.03, 'cbat.x='+String(cbat.x,FORMAT='(F0.3)'), /Normal
    XYOutS, 0.2, 0.03, 'cbat.y='+String(cbat.y,FORMAT='(F0.3)'), /Normal
ENDIF

END

;+
; Routine for plotting the ball
;-
PRO IDLPong_Draw_Ball, ball

    On_Error, 2

    PLOTS, $
        ball.x, ball.y, PSym=8, SymSize=ball.scale, /Normal

END

;+
; Routine for plotting the bats.
; 
; :Params: 
;   thebat : in, required, type=struct
;       The bat struct variable
;-
PRO IDLPong_Draw_Bat, thebat

    On_Error, 2

    hx = thebat.xsize/2.0
    hy = thebat.ysize/2.0

    PolyFill, $
        [thebat.x-hx,thebat.x+hx,thebat.x+hx,thebat.x-hx,thebat.x-hx], $
        [thebat.y-hy,thebat.y-hy,thebat.y+hy,thebat.y+hy,thebat.y-hy], $
        /Normal

END

;+
; Plot the main menu frame of the game for selecting difficulty level.
;
; :Returns:
;   The selection of the difficulty level
;
; :Params:
;   info : in, required, type=struct
;       The game information struct varaible
;-
FUNCTION IDLPong_Draw_Menu, info

    WSet, info.wid

    y_chsz = !D.Y_Ch_Size*1.0/info.ysize
    select_panel_ypos = [0.38,0.33,0.28,0.23,0.18,0.08]
    select_name = ['Effortless','Easy','Normal','Hard','Insane','Quit']

    ; Wait for user to select difficult level and start
    REPEAT BEGIN
        ;
        ; Set the display screen to be the main frame
        Cursor, xx, yy, /NoWait, /Normal
        ;
        WSet, info.pwid
        Erase
        ;
        ; Draw the dividor
        PLOTS, [0,1],[0.5,0.5], LineStyle=2, /Normal
        ;
        ; Game title
        XYOutS, 0.5, 0.75, info.gametitle, CharSize=3.0, CharThick=2.0, $
            Alignment=0.5, /Normal
        ;
        ; Respond to user mouse movement
        chsz = Replicate(1.5,6)
        chtk = Replicate(1.0,6)
        IF xx GE 0.4 AND xx LE 0.6 THEN BEGIN
            FOR ii=0,5 DO BEGIN
                IF yy GE select_panel_ypos[ii] AND yy LE select_panel_ypos[ii]+y_chsz THEN BEGIN
                    chsz[ii] = 1.5
                    chtk[ii] = 2.0
                ENDIF ELSE BEGIN
                    chsz[ii] = 1.5
                    chtk[ii] = 1.0
                ENDELSE
            ENDFOR
        ENDIF

        FOR ii=0,5 DO $
            XYOutS, 0.5, select_panel_ypos[ii], select_name[ii], CharSize=chsz[ii], CharThick=chtk[ii], $
            Alignment=0.5, /Normal

        ; Copy to the display window
        WSet, info.wid
        Device, Copy=[0,0,info.xsize,info.ysize,0,0,info.pwid]

        ; If left clicked, return the selected option
        IF !Mouse.Button EQ 1 THEN idxsel = Where(chtk EQ 2.0) ELSE idxsel = -1

    ENDREP UNTIL idxsel NE -1

    ; If the selection is not 'Quit', add some flashing effects for selection
    IF idxsel NE 5 THEN BEGIN
        FOR ii=0,4 DO BEGIN
            ; Buffer frame
            WSet, info.pwid
            Erase
            ; Draw the dividor
            PLOTS, [0,1],[0.5,0.5], LineStyle=2, /Normal

            XYOutS, 0.5, 0.75, info.gametitle, CharSize=3.0, CharThick=2.0, $
                Alignment=0.5, /Normal

            IF ii MOD 2 EQ 0 THEN $
                XYOutS, 0.5, select_panel_ypos[idxsel], select_name[idxsel], CharSize=chsz[idxsel], $
                CharThick=chtk[idxsel], Alignment=0.5, /Normal

            ; Copy to the display window
            WSet, info.wid
            Device, Copy=[0,0,info.xsize,info.ysize,0,0,info.pwid]

            Wait, 0.2
        ENDFOR
    ENDIF

    RETURN, idxsel
    ;
END

;+
; Main program of the Pong game. The game window is set to be displayed at the
; center of the screen by default.
;
; :Keywords:
;   XSize : in, optional, type=int, default=400
;       The X size of the main window
;   YSize : in, optional, type=int, default=500
;       The Y size of the main window
;   XOffset : in, optional, type=int, default=0
;       The X offset to display the main window
;   YOffset : in, optional, type=int, default=0
;       The Y offset to display the main window
;   DebugMode : in, optional, type=boolean
;       If set, some debug information will be shown in the main window
;-
PRO IDLPong, $
    XSize=xsize, YSize=ysize, $
    XOffset=xoffset, YOffset=yoffset, $
    Keyboard=keyboard, $
    DebugMode=debugmode

; Process input
IF N_ELEMENTS(xoffset) EQ 0 THEN xoffset=0
IF N_ELEMENTS(yoffset) EQ 0 THEN yoffset=0
IF N_ELEMENTS(xsize) EQ 0 THEN xsize=400
IF N_ELEMENTS(ysize) EQ 0 THEN ysize=500
IF KEYWORD_SET(keyboard) THEN keyboard=1 ELSE keyboard=0
IF KEYWORD_SET(debugmode) THEN debugmode=1 ELSE debugmode=0

;  Define and Initialize Main Variables for the program
;
; Get the screen size to center the display
Device, Get_Screen_Size=sz
; Calculated the main window position. 
; If no offset is set, the window is centered
xpos = sz[0]/2-xsize/2+xoffset
ypos = sz[1]/2-ysize/2+yoffset
;
; Main Screen and buffer frame for display
;
; The main display window
Window, Retain=2, /Free, XSize=xsize, YSize=ysize, XPos=xpos, YPos=ypos
wid = !D.Window

; The buffer frame
Window, /Free, /Pixmap, XSize=xsize,  YSize=ysize
pwid = !D.Window

; Pack them into a struct
info = { $
    xsize: xsize,           $
    ysize: ysize,           $
    xpos: xpos,             $
    ypos: ypos,             $
    wid : wid,              $
    pwid : pwid,            $
    gametitle : 'IDL PONG', $
    keyboard : keyboard,    $
    debugmode : debugmode   $
    }
;
; Start the game
REPEAT BEGIN
    ; Menu frame for selection
    idxsel = IDLPong_Draw_Menu(info)
    ; main loop of the game
    IF idxsel NE 5 THEN IDLPong_Main_Loop, info, idxsel
ENDREP UNTIL idxsel EQ 5

; Clean up 
IDLPong_Cleanup, info

END
