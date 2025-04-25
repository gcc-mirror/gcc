       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE
              ASSIGN      "TESTFILE"
              ACCESS       DYNAMIC
              ORGANIZATION RELATIVE
              STATUS       TESTSTAT
              RELATIVE KEY TESTKEY
       .
       DATA             DIVISION.
       FILE             SECTION.
       FD  TEST-FILE    GLOBAL.
       01  TEST-REC     PIC X(4).
       WORKING-STORAGE  SECTION.
       01  GLOBVALS.
           03  TESTKEY  PIC 9(4).
           03  TESTSTAT PIC XX.
       PROCEDURE        DIVISION.
           MOVE "00"    TO TESTSTAT.
           CALL  "prog2"
           END-CALL.
           IF TESTSTAT = "00"
              DISPLAY "Not OK"
              END-DISPLAY
           END-IF.
           STOP RUN.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        PROCEDURE        DIVISION.
            OPEN  INPUT TEST-FILE.
            EXIT PROGRAM.
        END PROGRAM prog2.
       END PROGRAM prog.

