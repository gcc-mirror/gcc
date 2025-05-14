       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/INSPECT_TRAILING.out" }

        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        01 the-text         pic x(30) value "    middle".
        01 counter          pic 9999.
        01 expected         pic 9999.
        01 should-be        pic zzz9.
        01 but-is           pic zzz9.
        01 msg              pic x(100).
        procedure           division.
        
        move "inspect for leading spaces" to msg
        move zero to counter
        inspect the-text tallying counter for leading spaces
        move 4 to expected
        perform result.
        
        move "inspect for trailing spaces with reverse" to msg
        move zero to counter
        inspect function reverse(the-text) tallying counter for leading spaces
        move 20 to expected
        perform result.

        move "inspect for trailing spaces with reversed variable" to msg
        move function reverse(the-text) to the-text
        move zero to counter
        inspect the-text tallying counter for leading spaces
        move 20 to expected
        perform result.

        move "inspect for trailing spaces with INSPECT TRAILING extension" to msg
        move function reverse(the-text) to the-text
        move zero to counter
        inspect the-text tallying counter for trailing spaces
        move 20 to expected
        perform result.

        inspect the-text replacing trailing space by "X"
        display the-text

        stop run.

        result.
        display function trim(msg) ": " with no advancing
        move expected to should-be
        if counter equal to expected
            display function trim(should-be)
        else
            move counter  to but-is
            display "should be " function trim(should-be) 
                    " but is " function trim(but-is)
        end-if.

