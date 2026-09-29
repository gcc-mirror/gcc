      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
[
        identification division.
        program-id. prog.
        data division.
        working-storage section.
        01 group1.
          03 member1 binary-long.
      * ensure 78-level entries between group items does not affect
      * parsing.
          78 cnt1 value 0.
          03 member2 binary-long.
        78 cnt2 value 0.
        78 cnt3 value 0.
        77 total-length binary-long.
        procedure division.
          compute total-length = length of member1 + length of member2.
          if length of group1 <> total-length
            display "length mismatch, expected " total-length
              ", got " length of group1
          end-if.
        end program prog.
]
