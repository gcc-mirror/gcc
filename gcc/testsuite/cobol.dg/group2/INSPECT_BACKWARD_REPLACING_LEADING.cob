       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_BACKWARD_REPLACING_LEADING.out" }
        identification  division.
        program-id.     caller.
        data            division.
        working-storage section.
        77 str          pic x(19) value "AAAAsomeABthingBBBB".
        procedure division.
        display "Starting with                                    " """" str """" "..."
        
        initialize str all value.
        inspect str replacing all "A" by "X"
        display "After inspect          replacing     ALL A by X: " """" str """" 

        initialize str all value.
        inspect str replacing leading "A" by "X"
        display "After inspect          replacing LEADING A by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing all "A" by "X"
        display "After inspect backward replacing     ALL A by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing leading "A" by "X"
        display "After inspect backward replacing LEADING A by X: " """" str """" 

        initialize str all value.
        inspect str replacing all "B" by "X"
        display "After inspect          replacing     ALL B by X: " """" str """" 

        initialize str all value.
        inspect str replacing leading "B" by "X"
        display "After inspect          replacing LEADING B by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing all "B" by "X"
        display "After inspect backward replacing     ALL B by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing leading "B" by "X"
        display "After inspect backward replacing LEADING B by X: " """" str """" 
        goback.

