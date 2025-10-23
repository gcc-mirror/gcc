       *> { dg-do run }
       *> { dg-xfail-run-if "" { *-*-* }  }
       *> { dg-output-file "group2/EC-BOUND-REF-MOD_checking_process_termination.out" }
        identification   division.
        program-id.      caller.
        data             division.
        working-storage  section.
        77 str pic x(4) value "abcd".
        procedure        division.
            display "sending str  " str
            call "prog1" using str.
            display "returned str " str
            call "prog2" using str.
            display "returned str " str
            goback.

        identification   division.
        program-id.      prog1.
        data             division.
        linkage  section.
        01 str pic x any length.
        procedure        division using str.
            move '4' to str(5:1)
            display "We should get here, because there is no checking"
            goback.
        end program       prog1.

        >>turn ec-all checking on
        identification   division.
        program-id.      prog2.
        data             division.
        linkage  section.
        01 str pic x any length.
        procedure        division using str.
            move '4' to str(5:1)
            display "I don't think we should get here?"
            goback.
        end program       prog2.

        end program       caller.

