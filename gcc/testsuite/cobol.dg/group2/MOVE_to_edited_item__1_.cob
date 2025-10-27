       *> { dg-do run }
       *> { dg-output-file "group2/MOVE_to_edited_item__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  SRC-1        PIC S99V99  VALUE   1.10.
       01  SRC-2        PIC S99V99  VALUE   0.02.
       01  SRC-3        PIC S99V99  VALUE  -0.03.
       01  SRC-4        PIC S99V99  VALUE  -0.04.
       01  SRC-5        PIC S99V99  VALUE  -0.05.
       01  EDT-1        PIC -(04)9.
       01  EDT-2        PIC -(04)9.
       01  EDT-3        PIC -(04)9.
       01  EDT-4        PIC +(04)9.
       01  EDT-5        PIC -(05).
       PROCEDURE        DIVISION.
           MOVE SRC-1   TO EDT-1.
           MOVE SRC-2   TO EDT-2.
           MOVE SRC-3   TO EDT-3.
           MOVE SRC-4   TO EDT-4.
           MOVE SRC-5   TO EDT-5.
           DISPLAY '>' EDT-1 '<'
           END-DISPLAY.
           DISPLAY '>' EDT-2 '<'
           END-DISPLAY.
           DISPLAY '>' EDT-3 '<'
           END-DISPLAY.
           DISPLAY '>' EDT-4 '<'
           END-DISPLAY.
           DISPLAY '>' EDT-5 '<'
           END-DISPLAY.
           STOP RUN.

