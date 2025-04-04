       *> { dg-do run }
       *> { dg-options "-dialect ibm" }
       *> { dg-output-file "group2/FUNCTION_BIGGER-POINTER__2_.out" }

        identification   division.
        program-id.      prog.
        data             division.
        working-storage  section.
        01  n4                 pic     s9(8) comp-5 value 0.
        01  p4   redefines n4  pointer.
        01  n8                 pic     s9(16) comp-5 value 0.
        01  p8   redefines n8  pointer.
        procedure        division.
            move -1 to n8
            set     p4 to p8
            display "P4 and P8 before: " p4 space p8
            display "Increment N4 and N8"
            add 1 to n4 n8
            display "P4 and P8  after: " p4 space p8
            goback.
            end program prog.

