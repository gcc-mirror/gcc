       *> { dg-do run }
       *> { dg-output-file "group2/Complex_EVALUATE__2_.out" }

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
            display "            bumper is returning " bumped
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
        procedure division.
            display "            Prime the pump with three calls to bumper"
            move function bumper to bump
            move function bumper to bump
            move function bumper to bump
            display "            Three calls to BUMPER should follow"
            evaluate function bumper also function bumper also function bumper
            when 4 also 5 also 6
                display "properly 4 also 5 also 6"
            when 7 also 8 also 9
                display "IMPROPERLY 7 also 8 also 9"
            when other
                display "IMPROPERLY we don't know what's going on"
            end-evaluate
            display "            Three more calls to BUMPER should follow"
            evaluate function bumper also function bumper also function bumper
            when 4 also 5 also 6
                display "IMPROPERLY 4 also 5 also 6"
            when 7 also 8 also 9
                display "properly 7 also 8 also 9"
            when other
                display "IMPROPERLY we don't know what's going on"
            end-evaluate
            goback.
        end program prog.

