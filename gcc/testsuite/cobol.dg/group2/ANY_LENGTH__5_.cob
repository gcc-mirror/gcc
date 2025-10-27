       *> { dg-do run }
       *> { dg-output-file "group2/ANY_LENGTH__5_.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
       CALL "subprog"
       GOBACK.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. subprog.
       DATA DIVISION.
       LINKAGE SECTION.
       01 str1 PIC X ANY LENGTH.
       01 str2 PIC X ANY LENGTH.
       PROCEDURE DIVISION USING optional str1 optional str2.
       DISPLAY 'IN' WITH NO ADVANCING.
       END PROGRAM subprog.
       END PROGRAM prog.

