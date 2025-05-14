       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_BYTE-LENGTH.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC      X(4).
       01  TEST-FLD     PIC S9(04)V9(08).
       PROCEDURE        DIVISION.
           MOVE FUNCTION BYTE-LENGTH ( TEST-FLD )   TO TEST-FLD.
           DISPLAY "BYTE-LENGTH of PIC S9(04)V9(08) is " TEST-FLD
           MOVE FUNCTION BYTE-LENGTH ( X )          TO TEST-FLD.
           DISPLAY "BYTE-LENGTH of PIC X(4) is "       TEST-FLD
           MOVE FUNCTION BYTE-LENGTH ( '00128' )    TO TEST-FLD
           DISPLAY "BYTE-LENGTH of PIC '00128' is "    TEST-FLD
           MOVE FUNCTION BYTE-LENGTH ( x'a0' )      TO TEST-FLD
           DISPLAY "BYTE-LENGTH of PIC x'a0' is "      TEST-FLD
           STOP RUN.

