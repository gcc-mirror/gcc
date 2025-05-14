       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_TEST-DAY-YYYYDDD__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC 999.
       PROCEDURE        DIVISION.
           MOVE FUNCTION TEST-DAY-YYYYDDD ( 2002400 ) TO RESULT
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

