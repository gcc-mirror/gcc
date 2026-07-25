      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-output-file "group2/ACCEPT_FROM__date__with_subscripts_and_refmods.out" }
        identification              division.
        program-id.                 prog.
        data                        division.
        working-storage             section.
        01 dates.
        02 foo pic x(30) occurs 6 times.
        01 msg pic x(20) value "The year is xxxx CE." .
        procedure                   division.
            display "GCOBOL_CURRENT_DATE" upon environment-name
            display "19530227123456"      upon environment-value
            accept  foo(1) from time
            accept  foo(2) from date
            accept  foo(3) from date yyyymmdd
            accept  foo(4) from day
            accept  foo(5) from day yyyyddd
            accept  foo(6) from day-of-week
            display function trim(foo(1))
            display function trim(foo(2))
            display function trim(foo(3))
            display function trim(foo(4))
            display function trim(foo(5))
            display function trim(foo(6))
            accept msg(13:4) from date yyyymmdd
            display msg
            goback.
        end program                 prog.

