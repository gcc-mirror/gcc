       *> { dg-do run }
       *> { dg-output-file "group2/EVALUATE_doubled_WHEN.out" }

        identification division.
        program-id. prog.
        data division.
        working-storage section.
        77 eval pic x(4).
        procedure division.
            move "open" to eval
            display "about to EVALUATE eval " """" eval """"
            evaluate true
            when eval = 'open'
            when eval = 'OPEN'
                display "Good: We got us an " """" eval """"
            when other
                display "BAD!!! It shoulda been " """" eval """"
            end-evaluate
            move "OPEN" to eval
            display "about to EVALUATE eval " """" eval """"
            evaluate true
            when eval = 'open'
            when eval = 'OPEN'
                display "Good:     We got us an " """" eval """"
            when other
                display "BAD!!! It shoulda been " """" eval """"
            end-evaluate
            goback.
        end program prog.

