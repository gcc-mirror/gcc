       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_EXCEPTION-FILE.out" }

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
           DISPLAY FUNCTION EXCEPTION-FILE '|'
                   NO ADVANCING
           END-DISPLAY.
           OPEN INPUT TEST-FILE.
           DISPLAY FUNCTION EXCEPTION-FILE
                   NO ADVANCING
           END-DISPLAY.
           STOP RUN.

