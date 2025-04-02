       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/INSPECT_BACKWARD_REPLACING_TRAILING.out" }
        identification  division.
        program-id.     caller.
        data            division.
        working-storage section.
        77 str          pic x(19) value "AAAAsomeABthingBBBB".
        procedure division.
        display "Starting with                                     " """" str """" "..."
        
        initialize str all value.
        inspect str replacing all "A" by "X"
        display "After inspect          replacing      ALL A by X: " """" str """" 

        initialize str all value.
        inspect str replacing trailing "A" by "X"
        display "After inspect          replacing TRAILING A by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing all "A" by "X"
        display "After inspect backward replacing      ALL A by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing trailing "A" by "X"
        display "After inspect backward replacing TRAILING A by X: " """" str """" 

        initialize str all value.
        inspect str replacing all "B" by "X"
        display "After inspect          replacing      ALL B by X: " """" str """" 

        initialize str all value.
        inspect str replacing trailing "B" by "X"
        display "After inspect          replacing TRAILING B by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing all "B" by "X"
        display "After inspect backward replacing      ALL B by X: " """" str """" 

        initialize str all value.
        inspect backward str replacing trailing "B" by "X"
        display "After inspect backward replacing TRAILING B by X: " """" str """" 
        goback.

