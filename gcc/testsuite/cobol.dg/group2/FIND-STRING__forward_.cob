      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-fexec-charset=ibm1140" }
       *> { dg-output-file "group2/FIND-STRING__forward_.out" }
        IDENTIFICATION  DIVISION.
        PROGRAM-ID.     prog.
        DATA            DIVISION.
        WORKING-STORAGE SECTION.
        01 foo pic x(55) value "bob01     bob11     bob21     bob31     bob41     bob51".
        01 nfound pic 99.
        PROCEDURE       DIVISION.
            move function find-string(foo, "bob")               to nfound
                display "A: " nfound
            move function find-string(foo, "bob" start after 0) to nfound
                display "B: " nfound
            move function find-string(foo, "bob" start after 1) to nfound
                display "C: " nfound
            move function find-string(foo, "bob" start after 2) to nfound
                display "D: " nfound
            move function find-string(foo, "bob" start after 3) to nfound
                display "E: " nfound
            move function find-string(foo, "bob" start after 4) to nfound
                display "F: " nfound
            move function find-string(foo, "bob" start after 5) to nfound
                display "G: " nfound
            move function find-string(foo, "bob" start after 6) to nfound
                display "H: " nfound

            move function find-string(foo, "BOB") to nfound
                display "I: " nfound
            
            move function find-string(foo, "BOB"anycase)               to nfound
                display "J: " nfound
            move function find-string(foo, "BOB" start after 0 anycase) to nfound
                display "K: " nfound
            move function find-string(foo, "BOB" start after 1 anycase) to nfound
                display "L: " nfound
            move function find-string(foo, "BOB" start after 2 anycase) to nfound
                display "M: " nfound
            move function find-string(foo, "BOB" start after 3 anycase) to nfound
                display "N: " nfound
            move function find-string(foo, "BOB" start after 4 anycase) to nfound
                display "O: " nfound
            move function find-string(foo, "BOB" start after 5 anycase) to nfound
                display "P: " nfound
            move function find-string(foo, "BOB" start after 6 anycase) to nfound
                display "Q: " nfound
        goback.

