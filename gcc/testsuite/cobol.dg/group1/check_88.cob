*> { dg-do run }
*> { dg-output {\-><\-(\n|\r\n|\r)} }
*> { dg-output {\->   <\-(\n|\r\n|\r)} }
*> { dg-output {\->"""<\-(\n|\r\n|\r)} }
*> { dg-output {\->000<\-(\n|\r\n|\r)} }
*> { dg-output {\->.*<\-(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {\-><\-(\n|\r\n|\r)} }
*> { dg-output {\->    <\-(\n|\r\n|\r)} }
*> { dg-output {\->""""<\-(\n|\r\n|\r)} }
*> { dg-output {\->0000<\-(\n|\r\n|\r)} }
*> { dg-output {\->.*<\-(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {There should be no garbage after character 32(\n|\r\n|\r)} }
*> { dg-output {\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\*\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-(\n|\r\n|\r)} }
*> { dg-output {.* Bundesstra.e                                (\n|\r\n|\r)} }
*> { dg-output {.* Bundesstra.e                                (\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {There should be no spaces before the final quote(\n|\r\n|\r)} }
*> { dg-output {".* Bundesstra.e"(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {   IsLow   ""(\n|\r\n|\r)} }
*> { dg-output {   IsZero  "000"(\n|\r\n|\r)} }
*> { dg-output {   IsHi    ".*"(\n|\r\n|\r)} }
*> { dg-output {   IsBob   "bob"(\n|\r\n|\r)} }
*> { dg-output {   IsQuote """""(\n|\r\n|\r)} }
*> { dg-output {   IsSpace "   "(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {CheckBinary Properly True(\n|\r\n|\r)} }
*> { dg-output {CheckBinary Properly False} }
        IDENTIFICATION DIVISION.
        PROGRAM-ID. check88.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 Check88 PIC XXX VALUE SPACE.
           88  CheckSpace  VALUE SPACE.
           88  CheckHi     VALUE HIGH-VALUES.
           88  CheckLo     VALUE LOW-VALUES.
           88  CheckZero   VALUE ZERO.
           88  CheckQuotes VALUE QUOTE.
           88  CheckBob    VALUE "bob".
           88  CheckBinary VALUE X"000102". *> { dg-warning embedded }
        01 000VARL PIC XXX VALUE LOW-VALUE.
        01 000VARS PIC XXX VALUE SPACE.
        01 000VARQ PIC XXX VALUE QUOTE.
        01 000VARZ PIC XXX VALUE ZERO.
        01 000VARH PIC XXX VALUE HIGH-VALUE.
        01 MOVE-TARGET PIC XXXX.
        01 VAR-UTF8 PIC X(64) VALUE "üüüüüüüüüüüüüüüüüüü Bundesstraße".
      *>  01 VAR20 PIC 9V9(20) value "1.1".
        01 VAR99   PIC 999 VALUE ZERO.
        PROCEDURE DIVISION.
            DISPLAY "->" 000VARL "<-"
            DISPLAY "->" 000VARS "<-"
            DISPLAY "->" 000VARQ "<-"
            DISPLAY "->" 000VARZ "<-"
            DISPLAY "->" 000VARH "<-"
            DISPLAY SPACE
            MOVE LOW-VALUE  TO MOVE-TARGET DISPLAY "->" MOVE-TARGET "<-"
            MOVE SPACE      TO MOVE-TARGET DISPLAY "->" MOVE-TARGET "<-"
            MOVE QUOTE      TO MOVE-TARGET DISPLAY "->" MOVE-TARGET "<-"
            MOVE ZERO       TO MOVE-TARGET DISPLAY "->" MOVE-TARGET "<-"
            MOVE HIGH-VALUE TO MOVE-TARGET DISPLAY "->" MOVE-TARGET "<-"
            DISPLAY SPACE
            DISPLAY "There should be no garbage after character 32"
            DISPLAY "-------------------------------*"
                    "--------------------------------"
            DISPLAY VAR-UTF8
            MOVE "üüüüüüüüüüüüüüüüüüü Bundesstraße" TO VAR-UTF8
            DISPLAY VAR-UTF8
            DISPLAY SPACE
            DISPLAY "There should be no spaces before the final quote"
            DISPLAY """" "üüüüüüüüüüüüüüüüüüü Bundesstraße" """"
            DISPLAY SPACE
            SET CheckLo     to TRUE PERFORM Checker DISPLAY """" Check88 """"
            SET CheckZero   to TRUE PERFORM Checker DISPLAY """" Check88 """"
            SET CheckHi     to TRUE PERFORM Checker DISPLAY """" Check88 """"
            SET CheckBob    to TRUE PERFORM Checker DISPLAY """" Check88 """"
            SET CheckQuotes to TRUE PERFORM Checker DISPLAY """" Check88 """"
            SET CheckSpace  to TRUE PERFORM Checker DISPLAY """" Check88 """"
            DISPLAY SPACE
            MOVE X"000102" TO Check88
            IF CheckBinary
                DISPLAY "CheckBinary Properly True"
            else 
                DISPLAY "CheckBinary IMPROPERLY False".
            MOVE X"030102" TO Check88
            IF CheckBinary
                DISPLAY "CheckBinary IMPROPERLY True"
            else
                DISPLAY "CheckBinary Properly False".
            STOP RUN.
        Checker.
            *>DISPLAY "Checking '" Check88 "'"
            IF CheckHi     DISPLAY "   IsHi    " NO ADVANCING END-IF
            IF CheckLo     DISPLAY "   IsLow   " NO ADVANCING END-IF
            IF CheckZero   DISPLAY "   IsZero  " NO ADVANCING END-IF
            IF CheckBob    DISPLAY "   IsBob   " NO ADVANCING END-IF
            IF CheckQuotes DISPLAY "   IsQuote " NO ADVANCING END-IF
            IF CheckSpace  DISPLAY "   IsSpace " NO ADVANCING END-IF
            .
