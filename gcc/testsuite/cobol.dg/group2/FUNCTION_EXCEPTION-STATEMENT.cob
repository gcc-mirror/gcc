       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_EXCEPTION-STATEMENT.out" }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        ENVIRONMENT      DIVISION.
        INPUT-OUTPUT     SECTION.
        FILE-CONTROL.
        SELECT TEST-FILE ASSIGN "NOTEXIST"
        FILE STATUS IS TEST-STATUS.
        DATA             DIVISION.
        FILE             SECTION.
            FD  TEST-FILE.
            01  TEST-REC      PIC X(4).
        WORKING-STORAGE SECTION.
            01  TEST-STATUS  PIC XX.
        PROCEDURE        DIVISION.
        DISPLAY "EXCEPTION-STATEMENT before bad OPEN: "
                """" FUNCTION EXCEPTION-STATEMENT """"
        OPEN INPUT TEST-FILE.
        DISPLAY "EXCEPTION-STATEMENT  after bad OPEN: "
                """" FUNCTION EXCEPTION-STATEMENT """"
        STOP RUN.

