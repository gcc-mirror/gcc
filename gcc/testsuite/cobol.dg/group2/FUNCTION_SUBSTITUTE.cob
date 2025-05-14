       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_SUBSTITUTE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y   PIC   X(24).
       PROCEDURE        DIVISION.
           MOVE "abc111444555defxxabc" TO Y
           DISPLAY FUNCTION TRIM (FUNCTION SUBSTITUTE ( Y "abc" "zz" "55" "666" ))

           MOVE "bobBobjimJimbobBobjimJim" TO Y
           DISPLAY FUNCTION SUBSTITUTE ( Y "bob" "FILLER" "jim" "Z")

           MOVE "bobBobjimJimbobBobjimJim" TO Y
           DISPLAY FUNCTION SUBSTITUTE ( Y FIRST "bob" "FILLER" "jim" "Z")

           MOVE "bobBobjimJimbobBobjimJim" TO Y
           DISPLAY FUNCTION SUBSTITUTE ( Y LAST "bob" "FILLER" "jim" "Z")

           MOVE "bobBobjimJimbobBobjimJim" TO Y
           DISPLAY FUNCTION SUBSTITUTE ( Y ANYCASE "bob" "FILLER" ANYCASE "jim" "Z")

