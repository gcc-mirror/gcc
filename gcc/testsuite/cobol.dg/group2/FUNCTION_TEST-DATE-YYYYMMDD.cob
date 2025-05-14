       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_TEST-DATE-YYYYMMDD.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 RESULT PIC 999.
       PROCEDURE        DIVISION.
           MOVE FUNCTION TEST-DATE-YYYYMMDD ( 20020231 ) TO RESULT
           DISPLAY RESULT
           END-DISPLAY.
           STOP RUN.

