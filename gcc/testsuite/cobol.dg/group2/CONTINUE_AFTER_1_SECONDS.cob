       *> { dg-do run }
       *> { dg-output-file "group2/CONTINUE_AFTER_1_SECONDS.out" }

        program-id. prog.
        data division.
        working-storage section.
        01 tod pic x(64).
        01 tstart pic 9999.
        01 tend   pic 9999.
        01 tspan  pic 9999.
        procedure division.
        accept tod from time
        move tod(5:) to tstart
        continue after 1.0 seconds.
        accept tod from time
        move tod(5:) to tend
        if tend < tstart 
            compute tend = tend + 6000
        end-if
        compute tspan = tend - tstart
        if tspan >= 75 and tspan <= 125
            display "Looks good"
        else
            display "Looks bad! " tstart space tend space tspan
        end-if
        goback.
        end program prog.

