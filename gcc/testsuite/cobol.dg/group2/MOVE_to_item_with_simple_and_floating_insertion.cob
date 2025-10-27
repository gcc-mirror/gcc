       *> { dg-do run }
       *> { dg-output-file "group2/MOVE_to_item_with_simple_and_floating_insertion.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  num-1 PIC -*B*99.
       01  num-2 PIC $BB**,***.**.
       01  num-3 PIC $BB--,---.--.

       PROCEDURE DIVISION.
           MOVE -123 TO num-1
           DISPLAY ">" num-1 "<"

           MOVE 1234.56 TO num-2
           DISPLAY ">" num-2 "<"

           MOVE 1234.56 TO num-3
           DISPLAY ">" num-3 "<"
           .

