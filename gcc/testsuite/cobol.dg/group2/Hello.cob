      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/Hello.out" }
        identification   division.
        program-id.      prog.
        data             division.
        working-storage  section.
        01 msg pic x(10) value "Hello".
        procedure        division.
            display """" msg """"
            display "Note the quotes. autoconf is allergic to trailing spaces."
            display "The alternative is to use FUNCTION TRIM, or autoconf quadrigraphs."
            display FUNCTION TRIM(msg)
            display msg
            goback.
        end program     prog.

