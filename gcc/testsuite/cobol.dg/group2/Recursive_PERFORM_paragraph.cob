       *> { dg-do run }
       *> { dg-output-file "group2/Recursive_PERFORM_paragraph.out" }
        identification      division.
        program-id.         prog.
        data                division.
        working-storage     section.
        77 n binary-double unsigned.
        77 f binary-double unsigned.
        procedure           division.
        move 20 to n
        move 1 to f
        display "compute " n " factorial".
        fact.
            compute f = f * n
            subtract 1 from n
            if n not equal to zero then 
                perform fact
            end-if.
        end-fact.
        display f.
        end program         prog.

