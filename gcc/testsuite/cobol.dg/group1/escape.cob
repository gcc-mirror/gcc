*> { dg-do run }
*> { dg-output {Testing the testing(\n|\r|\r\n)} }
*> { dg-output {\.\^\$\*\+\-\?\(\)\[\]\{\}\\\|(\n|\r|\r\n)} }
*> { dg-output {"\.\^\$\*\+\-\?\(\)\[\]\{\}\\\|"} }
        identification division.
        program-id. escape.
        procedure division.
        display "Testing the testing"
        display ".^$*+-?()[]{}\|"  
        display '".^$*+-?()[]{}\|"'  .
        end program escape.
