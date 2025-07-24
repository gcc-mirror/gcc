       *> { dg-do run }
       *> { dg-options "-static" }
       *> { dg-prune-output {warning} }
       *> { dg-output {hello, world} }
       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       PROCEDURE DIVISION.
       DISPLAY "hello, world".
       end program prog.

