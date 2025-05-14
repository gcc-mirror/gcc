       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_arithmetic.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC 99 USAGE PACKED-DECIMAL VALUE 0.
       01 Y             PIC 99 USAGE PACKED-DECIMAL VALUE 9.
       PROCEDURE        DIVISION.
           COMPUTE X = 1
           END-COMPUTE.
           DISPLAY X
           END-DISPLAY.
           COMPUTE X = Y
           END-COMPUTE.
           DISPLAY X
           END-DISPLAY.
           COMPUTE X = X + Y
           END-COMPUTE.
           DISPLAY X
           END-DISPLAY.
           STOP RUN.

