       *> { dg-do run }
       *> { dg-output-file "group2/EVALUATE_WHEN_NEGATIVE.out" }

        identification division.
        program-id. prog.
        data division.
        working-storage section.
        77 num pic s9.
        procedure division.
        move -1  to num
        evaluate num
        when negative
            display "negative"
        end-evaluate.
        end program prog.

