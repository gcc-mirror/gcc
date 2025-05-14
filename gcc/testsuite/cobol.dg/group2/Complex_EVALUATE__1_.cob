       *> { dg-do run }
       *> { dg-output-file "group2/Complex_EVALUATE__1_.out" }

        identification      division.
        function-id.        bumper.
        data                division.
        working-storage     section.
        77 bump             pic 9999    value zero.
        linkage             section.
        77 bumped           pic 9999.
        procedure division returning bumped.
            add 1 to bump.
            move bump to bumped.
            goback.
        end function        bumper.

        identification      division.
        program-id.         prog.
        environment         division.
        configuration       section.
            repository.
            function         bumper.
        data division.
        working-storage     section.
        77 bump             pic 9999    value zero.
        77 bump1            pic 9999    value zero.
        77 bump2            pic 9999    value zero.
        77 bump3            pic 9999    value zero.
        procedure division.
            move function bumper to bump
            display bump
            move function bumper to bump
            display bump
            move function bumper to bump
            display bump
            evaluate function bumper also function bumper also function bumper
            when 4 also 5 also 6
                display "properly 4 also 5 also 6"
            when 7 also 8 also 9
                display "IMPROPERLY 6 then 7 then 8"
            when other
                display "we don't know what's going on"
            end-evaluate
            goback.
        end program prog.

