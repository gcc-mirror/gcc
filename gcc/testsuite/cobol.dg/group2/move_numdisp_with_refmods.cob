      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/move_numdisp_with_refmods.out" }
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MOVEPROBE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 SOURCE-GROUP.
          05 SOURCE-FIELD PIC 9(9).
       01 SOURCE-RAW REDEFINES SOURCE-GROUP PIC X(9).
       01 TARGET-GROUP.
          05 TARGET-FIELD PIC 9(5).
       01 TARGET-RAW REDEFINES TARGET-GROUP PIC X(5).
       PROCEDURE DIVISION.
           MOVE "A0A1B0000" TO SOURCE-RAW
           MOVE ZERO TO TARGET-FIELD
           MOVE SOURCE-FIELD(1:5) TO TARGET-FIELD(1:5)
           DISPLAY "SOURCE=[" SOURCE-RAW "]"
           DISPLAY "TARGET=[" TARGET-RAW "]"
           STOP RUN.

