       *> { dg-do run }
       *> { dg-output-file "group2/EVALUATE_with_WHEN_using_condition-1.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 var-1 PIC 99V9.
           88 var-1-big VALUE 20 THRU 40.
           88 var-1-huge VALUE 40 THRU 99.
       PROCEDURE DIVISION.
           EVALUATE TRUE *> not: var-1
              WHEN var-1-big  DISPLAY "big"
              WHEN var-1-huge DISPLAY "huge"
              WHEN OTHER      DISPLAY "not"
              END-EVALUATE.
           END PROGRAM prog.

