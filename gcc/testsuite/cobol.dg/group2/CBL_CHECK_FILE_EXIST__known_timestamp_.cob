      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }

        copy "cblproto.cpy".
        identification division.
        program-id. prog.
        data division.
        working-storage section.
        copy "cbltypes.cpy".
        77 filename pic x(5) value "file".
        01 buf type cblt-fileexist-buf.
        01 year constant 2026.
        01 month constant 9.
        01 cday constant 16.
        01 hours constant 15.
        01 minutes constant 18.
        01 seconds constant 0.
        01 status-code pic x(2) comp-5.
        01 status-bits redefines status-code.
          03 msb pic x.
          03 lsb pic x comp-x.
        procedure division.
          call "CBL_CHECK_FILE_EXIST" using filename buf
            returning status-code.
          if status-code <> 0
            display "CBL_CHECK_FILE_EXIST failed with "
              "[" msb ", " lsb "]"
          else if cblte-fe-year <> year
            display "expected year " year ", got " cblte-fe-year
          else if cblte-fe-month <> month
            display "expected month " month ", got " cblte-fe-month
          else if cblte-fe-day <> cday
            display "expected day " cday ", got " cblte-fe-day
          else if cblte-fe-hours <> hours
            display "expected hours " hours ", got " cblte-fe-hours
          else if cblte-fe-minutes <> minutes
            display "expected minutes " minutes
              ", got " cblte-fe-minutes
          else if cblte-fe-seconds <> seconds
            display "expected seconds " seconds
              ", got " cblte-fe-seconds
          end-if.
        end program prog.

