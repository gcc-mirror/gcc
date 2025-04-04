       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   X(20).
       01  Z   PIC   X(20).
       PROCEDURE        DIVISION.
           MOVE "abc111444555defxxabc" TO Y.
           MOVE FUNCTION SUBSTITUTE
                   ( Y anycase "ABC" "zz"
                       anycase "55" "666" ) (2 : 9)
                TO Z.
           IF Z NOT = "z11144466"
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

