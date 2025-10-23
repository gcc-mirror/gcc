       *> { dg-do run }
       *> { dg-output-file "group2/Recursive_subscripts.out" }

       identification division.
       program-id. pmain.
       data division.
       working-storage section.
       01 filler.
        02 tabl-values pic x(9) value "234567890".
        02 v redefines tabl-values occurs 9 pic 9.
       procedure division.
       display v(1)                       " should be 2"
       display v(v(1))                    " should be 3"
       display v(v(v(1)))                 " should be 4"
       display v(v(v(v(1))))              " should be 5"
       display v(v(v(v(v(1)))))           " should be 6"
       display v(v(v(v(v(v(1))))))        " should be 7"
       display v(v(v(v(v(v(v(1)))))))     " should be 8"
       display v(v(v(v(v(v(v(v(1))))))))  " should be 9"

       display v(v(v(v(v(v(v(v(v(1)))))))))  " should be 0"
       move 1 to v(v(v(v(v(v(v(v(v(1)))))))))
       display v(v(v(v(v(v(v(v(v(1)))))))))  " should be 1"

       goback.
       end program pmain.

