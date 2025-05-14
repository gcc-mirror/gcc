       *> { dg-do run }
       *> { dg-output-file "group2/access_to_OPTIONAL_LINKAGE_item_not_passed.out" }

        identification   division.
        program-id.      caller.
        data             division.
        working-storage  section.
        01 x             pic x(4) value '9876'.
        procedure        division.
           call 'callee' using x
           end-call
           call 'callee' using omitted
           end-call
           stop run.
           end program caller.

        identification   division.
        program-id.      callee.
        data             division.
        working-storage  section.
        01 py pointer.
        linkage          section.
        01 x.
          05 y          pic x(4).
        procedure        division using optional x.
        set py to address of x.
        if py is not equal to zero
            display y
        else
            display "parameter omitted"
        end-if.
        goback.
        end program callee.

