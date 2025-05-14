       *> { dg-do run }
       *> { dg-output-file "group2/POINTER__display.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 PTR           USAGE POINTER VALUE NULL.
       PROCEDURE        DIVISION.
           DISPLAY PTR
           END-DISPLAY.
           SET PTR UP BY 1
           DISPLAY PTR
           SET PTR DOWN BY 1
           DISPLAY PTR
           END-DISPLAY.
           STOP RUN.

