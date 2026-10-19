      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
[
       identification division.
       program-id. prog.
       data division.
       working-storage section.
       01 filler.
           03 filler            occurs 2.
             05 group1.
               07  filler        pic  x(10).

       78  cnt1               value 2.
       01  group2.
         05 element  pic x(10) occurs cnt1.
       77 expected-length binary-long.

       procedure division.
           compute expected-length = length of group2 / cnt1.
           if length of group1 <> expected-length
              display "expected length " expected-length
                ", got " length of group1
           end-if.
       end program prog.
]
