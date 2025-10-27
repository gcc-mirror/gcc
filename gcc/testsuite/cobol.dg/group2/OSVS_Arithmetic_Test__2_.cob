       *> { dg-do run }
       *> { dg-output-file "group2/OSVS_Arithmetic_Test__2_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID. prog.
       ENVIRONMENT      DIVISION.
       DATA             DIVISION.
       WORKING-STORAGE SECTION.
       01  VAL                 PIC S9(7)V99 COMP-3 VALUE 20500.
       01  DIV1                PIC S9(7)V99 COMP-3 VALUE 0.9.
       01  DIV2                PIC S9(7)V99 COMP-3 VALUE 33.45.
       01  DIV3                PIC S9(7)V99 COMP-3 VALUE 9.
       01  MUL1                PIC S9(7)V99 COMP-3 VALUE 10.
       01  MUL2                PIC S9(7)V99 COMP-3 VALUE 5.
       01  MUL3                PIC S9(7)V99 COMP-3 VALUE 2.
       01  RES                 PIC S9(7)V99 COMP-3.
       PROCEDURE        DIVISION.
           COMPUTE RES = VAL / DIV1 / DIV2.
           DISPLAY 'RES = ' RES.
           COMPUTE RES ROUNDED = VAL / DIV1 / DIV2.
           DISPLAY 'RES ROUNDED = ' RES.
           COMPUTE RES = VAL * MUL1 / DIV3 / DIV2.
           DISPLAY 'RES MULT1 = ' RES.
           COMPUTE RES = VAL * MUL2 * MUL3 / DIV3 / DIV2.
           DISPLAY 'RES MULT2 = ' RES.
           COMPUTE RES = VAL / DIV1.
           DISPLAY 'RES 1 = ' RES.
           COMPUTE RES = RES / DIV2.
           DISPLAY 'RES F = ' RES.
           COMPUTE RES  =
                VAL / DIV1 / DIV2.
           DISPLAY 'RES NOT ROUNDED = ' RES.
           COMPUTE RES ROUNDED MODE NEAREST-AWAY-FROM-ZERO =
                VAL / DIV1 / DIV2.
           DISPLAY 'RES ROUNDED NEAREST-AWAY = ' RES.
           COMPUTE RES ROUNDED MODE AWAY-FROM-ZERO =
                VAL / DIV1 / DIV2.
           DISPLAY 'RES ROUNDED AWAY = ' RES.
           STOP RUN.

