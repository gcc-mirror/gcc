       *> { dg-do run }
       *> { dg-output-file "group2/CALL_with_OCCURS_DEPENDING_ON.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog-main.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  parm.
           03  parm-size PIC S999 COMP.
           03  parm-str.
               05  parm-char PIC X OCCURS 0 TO 100 TIMES
                        DEPENDING ON parm-size.

       PROCEDURE DIVISION.
           MOVE 10 TO parm-size
           MOVE "Hi, there!" TO parm-str
           CALL "prog" USING parm
           .
       END PROGRAM prog-main.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       DATA DIVISION.
       LINKAGE SECTION.
       01  parm.
           03  parm-size PIC S999 COMP.
           03  parm-str.
               05  parm-char PIC X OCCURS 0 TO 100 TIMES
                        DEPENDING ON parm-size.

       PROCEDURE DIVISION USING parm.
           DISPLAY FUNCTION TRIM(parm-str) WITH NO ADVANCING
           .
       END PROGRAM prog.

