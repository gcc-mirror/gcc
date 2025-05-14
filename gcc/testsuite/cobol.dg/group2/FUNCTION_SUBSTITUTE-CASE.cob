       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   X(20).
       01  Z   PIC   X(20).
       PROCEDURE        DIVISION.
           MOVE "ABC111444555defxxabc" TO Y.
           MOVE FUNCTION SUBSTITUTE (Y anycase "abc" "zz"
                                       anycase "55" "666")
                TO Z.
           IF Z NOT = "zz1114446665defxxzz"
              DISPLAY Z
              END-DISPLAY
           END-IF.
           STOP RUN.

