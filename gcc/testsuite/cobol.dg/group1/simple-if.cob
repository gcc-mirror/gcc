*> { dg-do run }
*> { dg-output {A_4 is 0005(\n|\r\n|\r)} }
*> { dg-output {B_4 is 0007(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {VALID: A_4  < B_4(\n|\r\n|\r)} }
*> { dg-output {VALID: A_4 <= B_4(\n|\r\n|\r)} }
*> { dg-output {VALID: A_4 <> B_4(\n|\r\n|\r)} }
*> { dg-output {VALID: A_4 NOT  = B_4(\n|\r\n|\r)} }
*> { dg-output {VALID: A_4 NOT  > B_4(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_TRUE: A_4  < B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_TRUE: A_4 <= B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_ELSE: A_4  = B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_TRUE: A_4 <> B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_ELSE: A_4 >= B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_ELSE: A_4  > B_4(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_ELSE: A_4 NOT  < B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_TRUE: A_4 NOT  = B_4(\n|\r\n|\r)} }
*> { dg-output {CORRECTLY_ELSE: A_4 NOT  > B_4(\n|\r\n|\r)} }
*> { dg-output { } }
        >>SOURCE FREE
        IDENTIFICATION DIVISION.
        PROGRAM-ID. test.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 A_4 PIC 9999 VALUE 5.
        01 B_4 PIC 9999 VALUE 7.
        PROCEDURE DIVISION.
        DISPLAY "A_4 is " A_4
        DISPLAY "B_4 is " B_4
        DISPLAY " "
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF A_4 < B_4 THEN
            DISPLAY "VALID: A_4  < B_4"
            END-IF
        IF A_4 <= B_4 THEN
            DISPLAY "VALID: A_4 <= B_4"
            END-IF
        IF A_4 = B_4 THEN
            DISPLAY "FALSE: A_4  = B_4"
            END-IF
        IF A_4 <> B_4 THEN
            DISPLAY "VALID: A_4 <> B_4"
            END-IF
        IF A_4 >= B_4 THEN
            DISPLAY "FALSE: A_4 >= B_4"
            END-IF
        IF A_4 > B_4 THEN
            DISPLAY "FALSE: A_4  > B_4"
            END-IF
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF A_4 NOT < B_4 THEN
            DISPLAY "FALSE: A_4 NOT  < B_4"
            END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT <= B_4 THEN
*          DISPLAY "FALSE: A_4 NOT <= B_4"
*          END-IF
        IF A_4 NOT = B_4 THEN
            DISPLAY "VALID: A_4 NOT  = B_4"
            END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT <> B_4 THEN
*          DISPLAY "FALSE: A_4 NOT <> B_4"
*          END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT >= B_4 THEN
*          DISPLAY "VALID: A_4 NOT >= B_4"
*          END-IF
        IF A_4 NOT > B_4 THEN
            DISPLAY "VALID: A_4 NOT  > B_4"
            END-IF
        DISPLAY " "
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF A_4 < B_4 THEN
            DISPLAY "CORRECTLY_TRUE: A_4  < B_4"
        ELSE
            DISPLAY "INCORRECT: A_4  < B_4"
            END-IF
        IF A_4 <= B_4 THEN
            DISPLAY "CORRECTLY_TRUE: A_4 <= B_4"
        ELSE
            DISPLAY "INCORRECT: A_4 <= B_4"
            END-IF
        IF A_4 = B_4 THEN
            DISPLAY "INCORRECT: A_4  = B_4"
        ELSE
            DISPLAY "CORRECTLY_ELSE: A_4  = B_4"
            END-IF
        IF A_4 <> B_4 THEN
            DISPLAY "CORRECTLY_TRUE: A_4 <> B_4"
        ELSE
            DISPLAY "INCORRECT: A_4 <> B_4"
            END-IF
        IF A_4 >= B_4 THEN
            DISPLAY "INCORRECT: A_4 >= B_4"
        ELSE
            DISPLAY "CORRECTLY_ELSE: A_4 >= B_4"
            END-IF
        IF A_4 > B_4 THEN
            DISPLAY "INCORRECT: A_4  > B_4"
        ELSE
            DISPLAY "CORRECTLY_ELSE: A_4  > B_4"
            END-IF
        DISPLAY " "
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        IF A_4 NOT < B_4 THEN
            DISPLAY "INCORRECT: A_4 NOT  < B_4"
        ELSE
            DISPLAY "CORRECTLY_ELSE: A_4 NOT  < B_4"
            END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT <= B_4 THEN
*          DISPLAY "INCORRECT: A_4 NOT <= B_4"
*      ELSE
*          DISPLAY "CORRECTLY_ELSE: A_4 NOT <= B_4"
*          END-IF
        IF A_4 NOT = B_4 THEN
            DISPLAY "CORRECTLY_TRUE: A_4 NOT  = B_4"
        ELSE
            DISPLAY "INCORRECT: A_4 NOT  = B_4"
            END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT <> B_4 THEN
*          DISPLAY "INCORRECT: A_4 NOT <> B_4"
*      ELSE
*          DISPLAY "CORRECTLY_ELSE: A_4 NOT <> B_4"
*          END-IF
*      This test works when compiled with GnuCOBOL
*      IF A_4 NOT >= B_4 THEN
*          DISPLAY "CORRECTLY_TRUE: A_4 NOT >= B_4"
*      ELSE
*          DISPLAY "INCORRECT: A_4 NOT >= B_4"
*          END-IF
        IF A_4 NOT > B_4 THEN
            DISPLAY "CORRECTLY_ELSE: A_4 NOT  > B_4"
        ELSE
            DISPLAY "INCORRECT: A_4 NOT  > B_4"
            END-IF
        DISPLAY " "
*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        STOP RUN.
