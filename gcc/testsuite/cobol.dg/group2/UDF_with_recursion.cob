       *> { dg-do run }
       *> { dg-output-file "group2/UDF_with_recursion.out" }

       IDENTIFICATION DIVISION.
       FUNCTION-ID. foo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ttl  PIC 9 VALUE 1.

       LOCAL-STORAGE SECTION.
       01  num  PIC 9.

       LINKAGE SECTION.
       01  arg PIC 9.
       01  ret PIC 9.

       PROCEDURE DIVISION USING arg RETURNING ret.
           IF arg < 5
              ADD 1 TO arg GIVING num END-ADD
              MOVE FUNCTION foo (num) TO ret
           ELSE
              MOVE arg TO ret
           END-IF
           DISPLAY "Step: " ttl ", Arg: " arg ", Return: " ret
           END-DISPLAY
           ADD 1 to ttl END-ADD
           GOBACK.
       END FUNCTION foo.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION foo.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 num PIC 9 VALUE 1.

       PROCEDURE DIVISION.
           DISPLAY "Return value '" FUNCTION foo (num) "'"
             WITH NO ADVANCING
           END-DISPLAY
           GOBACK.
       END PROGRAM prog.

