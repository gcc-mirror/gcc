       *> { dg-do run }
       *> { dg-output-file "group2/MOVE_to_JUSTIFIED_item.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  SRC-1        PIC S9(04)          VALUE  11.
       01  SRC-2        PIC S9(04) COMP     VALUE  22.
       01  SRC-3        PIC S9(04) COMP-5   VALUE  33.
       01  SRC-4        PIC S9(04)PP        VALUE  4400.
       01  SRC-5        PIC S9(04)PPPPP     VALUE  55500000.
       01  EDT-FLD      PIC X(07)           JUSTIFIED RIGHT.
       PROCEDURE        DIVISION.
           MOVE SRC-1   TO EDT-FLD.
           DISPLAY '>' EDT-FLD '<'
           END-DISPLAY.
           MOVE SRC-2   TO EDT-FLD.
           DISPLAY '>' EDT-FLD '<'
           END-DISPLAY.
           MOVE SRC-3   TO EDT-FLD.
           DISPLAY '>' EDT-FLD '<'
           END-DISPLAY.
           MOVE SRC-4   TO EDT-FLD.
           DISPLAY '>' EDT-FLD '<'
           END-DISPLAY.
           MOVE SRC-5   TO EDT-FLD.
           DISPLAY '>' EDT-FLD '<'
           END-DISPLAY.
           STOP RUN.

