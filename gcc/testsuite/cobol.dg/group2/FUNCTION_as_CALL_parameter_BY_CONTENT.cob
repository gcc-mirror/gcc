       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_as_CALL_parameter_BY_CONTENT.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       PROCEDURE DIVISION.
       PROG-MAIN.
           CALL "subprog" USING BY CONTENT
                                FUNCTION CONCAT("Abc" "D")
           STOP RUN.

       *> *****************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. subprog.

       DATA DIVISION.
       LINKAGE SECTION.
       01 TESTING PIC X ANY LENGTH.

       PROCEDURE DIVISION USING TESTING.
       SUBPROG-MAIN.
           DISPLAY TESTING
           GOBACK.
       END PROGRAM subprog.
       END PROGRAM prog. *> bzzt

