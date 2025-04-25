      *> { dg-do compile }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       INPUT-OUTPUT     SECTION.
       FILE-CONTROL.
       SELECT TEST-FILE
              ASSIGN      "TESTFILE"
              ACCESS       DYNAMIC
              ORGANIZATION INDEXED
              STATUS       TESTSTAT
              RECORD KEY   TESTKEY
       .
       DATA             DIVISION.
       FILE             SECTION.
       FD  TEST-FILE    GLOBAL.
       01  TEST-REC.
           03  TESTKEY  PIC X(4).
       WORKING-STORAGE  SECTION.
       01  GLOBVALS.
           03  TESTSTAT PIC XX.
       PROCEDURE        DIVISION.
           OPEN  INPUT TEST-FILE.
           CALL  "prog2"
           END-CALL.
           CLOSE TEST-FILE.
           STOP RUN.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        PROCEDURE        DIVISION.
            READ TEST-FILE
                 INVALID KEY
                 DISPLAY "NOK"
                 END-DISPLAY
            END-READ.
            EXIT PROGRAM.
        END PROGRAM prog2.
       END PROGRAM prog.

