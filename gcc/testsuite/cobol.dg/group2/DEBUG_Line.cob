       *> { dg-do run }
       *> { dg-output-file "group2/DEBUG_Line.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           Linux WITH DEBUGGING MODE.
       PROCEDURE DIVISION.
      *> Success is printing this message. If nothing comes out, the
      *> test fails.
      D    DISPLAY "DEBUG MESSAGE" NO ADVANCING.
       EXIT PROGRAM.
       END PROGRAM prog.

