       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_EXCEPTION-STATUS.out" }

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
           DISPLAY "EXCEPTION STATUS before bad open: "
                    """" FUNCTION EXCEPTION-STATUS """"
           OPEN INPUT TEST-FILE.
           DISPLAY "EXCEPTION STATUS  after bad open: "
                    """" FUNCTION EXCEPTION-STATUS """"
           STOP RUN.

