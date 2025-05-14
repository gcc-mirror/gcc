       *> { dg-do run }
       *> { dg-output-file "group2/DISPLAY__Sign_ASCII__2_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X           PIC X(10).
         02 X-S99       REDEFINES X PIC S99.
         02 X-S9        REDEFINES X PIC S9 OCCURS 10.
       PROCEDURE        DIVISION.
           MOVE 0 TO X-S9(1).
           MOVE 1 TO X-S9(2).
           MOVE 2 TO X-S9(3).
           MOVE 3 TO X-S9(4).
           MOVE 4 TO X-S9(5).
           MOVE 5 TO X-S9(6).
           MOVE 6 TO X-S9(7).
           MOVE 7 TO X-S9(8).
           MOVE 8 TO X-S9(9).
           MOVE 9 TO X-S9(10).
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           MOVE -10 TO X-S99. MOVE X(2:1) TO X(1:1).
           MOVE -1 TO X-S9(2).
           MOVE -2 TO X-S9(3).
           MOVE -3 TO X-S9(4).
           MOVE -4 TO X-S9(5).
           MOVE -5 TO X-S9(6).
           MOVE -6 TO X-S9(7).
           MOVE -7 TO X-S9(8).
           MOVE -8 TO X-S9(9).
           MOVE -9 TO X-S9(10).
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           STOP RUN.

