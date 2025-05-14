       *> { dg-do run }
       *> { dg-output-file "group2/Refmod__comparisons_inside_numeric-display.out" }
        identification division.
        program-id. prog.
        data division.
        working-storage section.
        01 n  pic 9(9).
        01 i  pic 99.
        procedure division.
            perform varying i from 1 by 1 until i > 8
            move 88888888 to n
            move "12" to n(i:2)
            display n
            if n(i:2) not equal to "12"
                display "Equality is flawed"
                end-if
            end-perform.
            goback.
         end program prog.

