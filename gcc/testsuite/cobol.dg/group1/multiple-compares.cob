*> { dg-do run }
*> { dg-output {D9 is 002(\n|\r\n|\r)} }
*> { dg-output {B9 is 002(\n|\r\n|\r)} }
*> { dg-output {X1 is '2'(\n|\r\n|\r)} }
*> { dg-output {X2 is ' 2'(\n|\r\n|\r)} }
*> { dg-output {X3 is '  2'(\n|\r\n|\r)} }
*> { dg-output {X4 is '2'(\n|\r\n|\r)} }
*> { dg-output {X5 is '02'(\n|\r\n|\r)} }
*> { dg-output {X6 is '002'(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO D9          (\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO B9          (\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X1           NOT(\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X2           NOT(\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X3           NOT(\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X4           NOT(\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X5           NOT(\n|\r\n|\r)} }
*> { dg-output {D9 EQUAL TO X6          (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO D9          (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO B9          (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X1           NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X2           NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X3           NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X4           NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X5           NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO X6          (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO 2           (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO 002         (\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO '2'          NOT(\n|\r\n|\r)} }
*> { dg-output {B9 EQUAL TO '002'       (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 2  EQUAL TO B9         (\n|\r\n|\r)} }
*> { dg-output {'2' EQUAL TO B9          NOT(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 002  EQUAL TO B9       (\n|\r\n|\r)} }
*> { dg-output {'002' EQUAL TO B9       (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 2  EQUAL TO  2         (\n|\r\n|\r)} }
*> { dg-output { 2  EQUAL TO '2'        (\n|\r\n|\r)} }
*> { dg-output {'2' EQUAL TO  2         (\n|\r\n|\r)} }
*> { dg-output {'2' EQUAL TO '2'        (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 2  EQUAL TO  002       (\n|\r\n|\r)} }
*> { dg-output { 2  EQUAL TO '002'       NOT(\n|\r\n|\r)} }
*> { dg-output {'2' EQUAL TO  002        NOT(\n|\r\n|\r)} }
*> { dg-output {'2' EQUAL TO '002'       NOT(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 002  EQUAL TO  2       (\n|\r\n|\r)} }
*> { dg-output { 002  EQUAL TO '2'       NOT(\n|\r\n|\r)} }
*> { dg-output {'002' EQUAL TO  2        NOT(\n|\r\n|\r)} }
*> { dg-output {'002' EQUAL TO '2'       NOT(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 002  EQUAL TO  002     (\n|\r\n|\r)} }
*> { dg-output { 002  EQUAL TO '002'    (\n|\r\n|\r)} }
*> { dg-output {'002' EQUAL TO  002     (\n|\r\n|\r)} }
*> { dg-output {'002' EQUAL TO '002'    (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output { 1000 EQUAL TO 999PPP   (\n|\r\n|\r)} }
*> { dg-output { 0\.0001 EQUAL TO PPP999 } }
        IDENTIFICATION DIVISION.
        PROGRAM-ID. bigif.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 D9 PICTURE 999 . *>DISPLAY.
        01 B9 PICTURE 999 BINARY.
        01 X1 PICTURE X .
        01 X2 PICTURE XX .
        01 X3 PICTURE XXX .
        01 X4 PICTURE X .
        01 X5 PICTURE XX .
        01 X6 PICTURE XXX.
        01 AAA PICTURE 999.
        01 999PPP PIC 999PPP BINARY.
        01 PPP999 PIC PPP999 BINARY.
        01 MSG PIC X(24).
        PROCEDURE DIVISION.
        MOVE 2 TO D9
        MOVE 2 TO B9
        MOVE "2" TO X1
        MOVE " 2" TO X2
        MOVE "  2" TO X3
        MOVE "2" TO X4
        MOVE "02" TO X5
        MOVE "002" TO X6
        DISPLAY "D9 is " D9
        DISPLAY "B9 is " B9
        DISPLAY "X1 is '" X1 "'"
        DISPLAY "X2 is '" X2 "'"
        DISPLAY "X3 is '" X3 "'"
        DISPLAY "X4 is '" X4 "'"
        DISPLAY "X5 is '" X5 "'"
        DISPLAY "X6 is '" X6 "'"
        DISPLAY " "
        MOVE "D9 EQUAL TO D9" TO MSG
           IF D9 EQUAL TO D9 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO B9" TO MSG
           IF D9 EQUAL TO B9 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X1" TO MSG
           IF D9 EQUAL TO X1 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X2" TO MSG
           IF D9 EQUAL TO X2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X3" TO MSG
           IF D9 EQUAL TO X3 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X4" TO MSG
           IF D9 EQUAL TO X4 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X5" TO MSG
           IF D9 EQUAL TO X5 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "D9 EQUAL TO X6" TO MSG
           IF D9 EQUAL TO X6 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE "B9 EQUAL TO D9" TO MSG
           IF B9 EQUAL TO D9 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO B9" TO MSG
           IF B9 EQUAL TO B9 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X1" TO MSG
           IF B9 EQUAL TO X1 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X2" TO MSG
           IF B9 EQUAL TO X2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X3" TO MSG
           IF B9 EQUAL TO X3 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X4" TO MSG
           IF B9 EQUAL TO X4 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X5" TO MSG
           IF B9 EQUAL TO X5 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO X6" TO MSG
           IF B9 EQUAL TO X6 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE "B9 EQUAL TO 2" TO MSG
           IF B9 EQUAL TO 2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO 002" TO MSG
           IF B9 EQUAL TO 002 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO '2'" TO MSG
           IF B9 EQUAL TO '2' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "B9 EQUAL TO '002'" TO MSG
           IF B9 EQUAL TO '002' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 2  EQUAL TO B9"     TO MSG
           IF  2  EQUAL TO B9      THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'2' EQUAL TO B9"   TO MSG
           IF '2' EQUAL TO B9    THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 002  EQUAL TO B9"   TO MSG
           IF  002  EQUAL TO B9    THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'002' EQUAL TO B9"  TO MSG
           IF '002' EQUAL TO B9   THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 2  EQUAL TO  2" TO MSG
           IF  2  EQUAL TO  2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE " 2  EQUAL TO '2'" TO MSG
           IF  2  EQUAL TO '2' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'2' EQUAL TO  2" TO MSG
           IF '2' EQUAL TO  2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'2' EQUAL TO '2'" TO MSG
           IF '2' EQUAL TO '2' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 2  EQUAL TO  002" TO MSG
           IF  2  EQUAL TO  002 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE " 2  EQUAL TO '002'" TO MSG
           IF  2  EQUAL TO '002' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'2' EQUAL TO  002" TO MSG
           IF '2' EQUAL TO  002 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'2' EQUAL TO '002'" TO MSG
           IF '2' EQUAL TO '002' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 002  EQUAL TO  2" TO MSG
           IF  002  EQUAL TO  2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE " 002  EQUAL TO '2'" TO MSG
           IF  002  EQUAL TO '2' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'002' EQUAL TO  2" TO MSG
           IF '002' EQUAL TO  2 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'002' EQUAL TO '2'" TO MSG
           IF '002' EQUAL TO '2' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 002  EQUAL TO  002" TO MSG
           IF  002  EQUAL TO  002 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE " 002  EQUAL TO '002'" TO MSG
           IF  002  EQUAL TO '002' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'002' EQUAL TO  002" TO MSG
           IF '002' EQUAL TO  002 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE "'002' EQUAL TO '002'" TO MSG
           IF '002' EQUAL TO '002' THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        DISPLAY " "
        MOVE " 1000 EQUAL TO 999PPP" TO MSG
        MOVE 1000 TO 999PPP.
           IF  1000  EQUAL TO  999PPP THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        MOVE " 0.0001 EQUAL TO PPP999" TO MSG
        MOVE 0.0001 TO PPP999.
           IF  0.0001  EQUAL TO  PPP999 THEN DISPLAY MSG ELSE DISPLAY  MSG " NOT" END-IF
        STOP RUN.
        END PROGRAM bigif.
