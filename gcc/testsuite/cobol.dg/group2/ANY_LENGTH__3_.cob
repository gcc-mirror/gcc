       *> { dg-do run }
       *> { dg-output-file "group2/ANY_LENGTH__3_.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 str PIC X(20) VALUE ALL "X".
       PROCEDURE DIVISION.
           CALL "subprog" USING str.
       IDENTIFICATION DIVISION.
       PROGRAM-ID. subprog.
       DATA DIVISION.
       LINKAGE SECTION.
       01 str PIC X ANY LENGTH.
       PROCEDURE DIVISION USING str.
           MOVE "abcd" TO str
           DISPLAY FUNCTION TRIM (str)
           MOVE "abcd" TO str (5:)
           DISPLAY FUNCTION TRIM (str)
           MOVE ALL "a" TO str
           DISPLAY FUNCTION TRIM (str).
       END PROGRAM subprog.
       END PROGRAM prog.

