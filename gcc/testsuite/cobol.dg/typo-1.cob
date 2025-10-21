*> { dg-options "-fdiagnostics-show-caret" } 
*> { dg-do compile }

       identification division.
       porgram-id. hello. *> { dg-error "8: syntax error, unexpected NAME, expecting FUNCTION or PROGRAM-ID" }
       procedure division.
           display "Hello World!".
           stop run.

*<<
{ dg-begin-multiline-output "" }
        porgram-id. hello.
        ^~~~~~~~~~~
{ dg-end-multiline-output "" }
*>>
