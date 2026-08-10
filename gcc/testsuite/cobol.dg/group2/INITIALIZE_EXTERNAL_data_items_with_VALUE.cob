      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/INITIALIZE_EXTERNAL_data_items_with_VALUE.out" }
        identification   division.
        program-id.      prog.
        data             division.
        working-storage  section.
            01 ext-var       pic x(6) external.
            01 ext-vari      pic x(6) external value "absurd".
        procedure        division.
            initialize ext-var
            display "ext-var is " """"ext-var""""
            initialize ext-var all to value
            display "ext-var is " """"ext-var""""
            initialize ext-vari
            display "ext-vari is " """"ext-vari""""
            initialize ext-vari all to value
            display "ext-vari is " """"ext-vari""""
            goback.
        end program prog.

