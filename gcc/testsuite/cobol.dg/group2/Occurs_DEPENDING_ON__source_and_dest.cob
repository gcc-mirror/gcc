       *> { dg-do run }
       *> { dg-output-file "group2/Occurs_DEPENDING_ON__source_and_dest.out" }
        identification              division.
        program-id.                 prog.
        data                        division.
        working-storage             section.
        01 table1d value "1234567890".
          02 table1  pic x occurs 0 to 10 times depending on table1do.

        01 table2d value "1234567890".
          02 table2  pic x occurs 0 to 10 times depending on table2do.

        01 table3d.
          02 table3do pic 99.
          02 table3dd.
            03 table3  pic x occurs 0 to 10 times depending on table3do.

        77 table1do pic 99.
        77 table2do pic 99.
        77 n pic 99.
        procedure                   division.
            display "Test1: Demonstrate ODO limits:"
            perform varying n from 0 by 1 until n > 10
                move n to table1do
                display n space """"table1d""""
                end-perform

            display "Test2:  result should be ABC4567890"
            move 3 to table2do
            move "ABCDEFGHIJ" to table2d
            move 10 to table2do
            display "        result is        "table2d

            display "Test3A: result should be 05ABCDE"
            move "05ABCDEFGHIJ" to table3d
            display "        result is        "table3d
            move 10 to table3do
            display "Test3B: result should be 10ABCDEFGHIJ"
            display "        result is        "table3d
            
            display "Test4:  result should be 10lmnopqGHIJ"
            move 6 to table3do
            move "lmnopqrstu" to table3dd
            move 10 to table3do
            display "        result is        "table3d

            goback.

