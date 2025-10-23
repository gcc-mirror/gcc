       *> { dg-do run }
       *> { dg-output-file "group2/Subscripted_refmods.out" }

       identification division.
       program-id. pmain.
       data division.
       working-storage section.
       01 filler.
        02 tabl-values pic x(9) value "123456789".
        02 v redefines tabl-values occurs 9 pic 9.
       procedure division.
       display tabl-values( 3:4 )           " should be 3456"
       display tabl-values( v(3):v(4) )     " should be 3456"
       goback.
       end program pmain.

