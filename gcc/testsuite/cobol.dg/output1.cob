*> { dg-do run }
*> { dg-output {-0.00012(\n|\r\n|\r)} }
*> { dg-output {0.00012(\n|\r\n|\r)} }
*> { dg-output {1234.66(\n|\r\n|\r)} }
*> { dg-output {-99.8(\n|\r\n|\r)} }
IDENTIFICATION DIVISION.
PROGRAM-ID. output1.
ENVIRONMENT DIVISION.
PROCEDURE DIVISION.
    DISPLAY -0.00012
    DISPLAY 0.00012
    DISPLAY 1234.66
    DISPLAY -99.8 
    STOP RUN.
