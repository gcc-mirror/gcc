       *> { dg-do run }
       *> { dg-output-file "group2/SORT__table_sort__2_.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 K                 PIC 9(2).

       01 CNT1              PIC 9(9) COMP-5 VALUE 4.
       01 TAB1.
          05 ROW1 OCCURS 1 TO 4 DEPENDING CNT1
                                 DESCENDING TAB1-NR.
             10 TAB1-NR     PIC 99.

       01 TAB2.
          05 CNT2           PIC 9(9) COMP-5 VALUE 4.
          05 ROW2 OCCURS 1 TO 4 DEPENDING CNT2
                                 DESCENDING TAB2-NR.
             10 TAB2-NR PIC 99.

       01 TAB3.
          05 CNT3           PIC 9(9) COMP-5 VALUE 10.
          05 ROW3 OCCURS 1 TO 10 DEPENDING CNT3
                                  DESCENDING TAB3-NR
                                  ASCENDING TAB3-DATA.
             10 TAB3-NR     PIC 99.
             10 FILLER      PIC X(2).
             10 TAB3-DATA   PIC X(5).
             10 FILLER      PIC X(2).
             10 TAB3-DATA2  PIC X(5).


       PROCEDURE DIVISION.
       A.
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 4
             MOVE K TO TAB1-NR(K), TAB2-NR(K)
           END-PERFORM

           MOVE 1 TO TAB3-NR(1).
           MOVE 1 TO TAB3-NR(8).
           MOVE 1 TO TAB3-NR(4).
           MOVE 6 TO TAB3-NR(2).
           MOVE 5 TO TAB3-NR(3).
           MOVE 5 TO TAB3-NR(9).
           MOVE 2 TO TAB3-NR(5).
           MOVE 2 TO TAB3-NR(10).
           MOVE 4 TO TAB3-NR(6).
           MOVE 3 TO TAB3-NR(7).

           MOVE "abcde" TO TAB3-DATA(1).
           MOVE "AbCde" TO TAB3-DATA(2).
           MOVE "abcde" TO TAB3-DATA(3).
           MOVE "zyx" TO TAB3-DATA(4).
           MOVE "12345" TO TAB3-DATA(5).
           MOVE "zyx" TO TAB3-DATA(6).
           MOVE "abcde" TO TAB3-DATA(7).
           MOVE "AbCde" TO TAB3-DATA(8).
           MOVE "abc" TO TAB3-DATA(9).
           MOVE "12346" TO TAB3-DATA(10).

           MOVE "day" TO TAB3-DATA2(1).
           MOVE "The" TO TAB3-DATA2(2).
           MOVE "eats" TO TAB3-DATA2(3).
           MOVE "." TO TAB3-DATA2(4).
           MOVE "mooos" TO TAB3-DATA2(5).
           MOVE "grass" TO TAB3-DATA2(6).
           MOVE "and" TO TAB3-DATA2(7).
           MOVE "whole" TO TAB3-DATA2(8).
           MOVE "cow" TO TAB3-DATA2(9).
           MOVE "the" TO TAB3-DATA2(10).

           SORT ROW1 DESCENDING TAB1-NR
           SORT ROW2 DESCENDING TAB2-NR

           DISPLAY "SINGLE TABLE" END-DISPLAY
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 4
             DISPLAY TAB1-NR(K) END-DISPLAY
           END-PERFORM

           DISPLAY "LOWER LEVEL TABLE" END-DISPLAY
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 4
             DISPLAY  TAB2-NR(K) END-DISPLAY
           END-PERFORM

           SORT ROW3 DESCENDING TAB3-NR ASCENDING TAB3-DATA

           DISPLAY "MULTY KEY SORT" END-DISPLAY
           PERFORM VARYING K FROM 1 BY 1 UNTIL K > 10
             DISPLAY  FUNCTION TRIM(ROW3(K))
             END-DISPLAY
           END-PERFORM

           STOP RUN.

