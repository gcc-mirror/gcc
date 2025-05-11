       *> { dg-do run }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 str PIC X(20) VALUE ALL "X".

       PROCEDURE DIVISION.
           CALL "subprog" USING str
           move '   45'   to str
           CALL "subprog" USING str
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. subprog.

       DATA DIVISION.
       LINKAGE SECTION.
       01 str PIC X ANY LENGTH.

       PROCEDURE DIVISION USING str.
           IF str = 'X'
             DISPLAY 'X is X'
           END-IF
           IF str = space
             DISPLAY 'X is space'
           END-IF
           .
       END PROGRAM subprog.
       END PROGRAM prog.

