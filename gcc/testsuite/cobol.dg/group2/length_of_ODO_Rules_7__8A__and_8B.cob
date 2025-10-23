       *> { dg-do run }
       *> { dg-output-file "group2/length_of_ODO_Rules_7__8A__and_8B.out" }

        identification division.
        program-id. prog.
        procedure division.
        call "prog1"
        call "prog2"
        call "prog3"
        goback.
        end program prog.

        identification division.
        program-id. prog1.
        data division.
        working-storage section.
        01      depl        pic 9.
        01      digtab.
         05     digitgrp.
          10    digits      occurs 1 to 9 depending on depl pic x.
        procedure division.
        display "Demonstrates 13.18.38.4 OCCURS General rules 7)"
        display "depl is completely separate"
        display "output should be ""12345    """
        move 9              to depl
        move space          to digtab
        move 5              to depl
        move "123456789"    to digtab
        move 9              to depl
        display             """" digtab """"
        goback.
        end program prog1.

        identification division.
        program-id. prog2.
        data division.
        working-storage section.
        01      digtab.
         05     depl        pic 9.
         05     digitgrp.
          10    digits      occurs 1 to 9 depending on depl pic x.
        procedure division.
        display "Demonstrates 13.18.38.4 OCCURS General rules 8a)"
        display "depl is not subordinate to digitgrp"
        display "output should be ""12345    """
        move 9              to depl
        move space          to digtab
        move 5              to depl
        move "123456789"    to digitgrp
        move 9              to depl
        display             """" digitgrp """"
        goback.
        end program prog2.

        identification division.
        program-id. prog3.
        data division.
        working-storage section.
        01      digtab.
         05     depl        pic 9.
         05     digitgrp.
          10    digits      occurs 1 to 9 depending on depl pic x.
        procedure division.
        display "Demonstrates 13.18.38.4 OCCURS General rules 8b)"
        display "depl is subordinate to digtab"
        display "output should be ""123"" followed by ""123456789"""
        move 9              to depl
        move space          to digtab
        move 5              to depl
        move "3123456789"   to digtab
        display             """" digitgrp """"
        move 9              to depl
        display             """" digitgrp """"
        goback.
        end program prog3.

