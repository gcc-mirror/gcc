      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/CBL_GET_PROGRAM_INFO__get_program_name_-_functions_0_and_2_.out" }

        copy "cblproto.cpy".
        identification division.
        program-id. prog.
        data division.
        working-storage section.
        copy "cbltypes.cpy".
        01 param-block type cblt-prog-info-params value zeros.
        77 fn pic x(4) comp-5.
        77 status-code pic x(2) comp-5.
        77 buf pic x(128).
        77 buflen pic x(4) comp-5.
        procedure division.
          move length of param-block to cblte-gpi-size.

      * allocate handle
          move 7 to cblte-gpi-flags.
          move 0 to fn.
          move length of buf to buflen.
          call "CBL_GET_PROGRAM_INFO" using
                                by value fn
                                by reference param-block
                                             buf
                                             buflen
                                returning status-code.

          if status-code <> 0
            display "CBL_GET_PROGRAM_INFO failed with " status-code
            goback
          end-if.

          display "buf is '" buf "'".

      * retrieve program name from handle
          move 6 to cblte-gpi-flags.
          move 2 to fn.
          move x"00" to buf.
          call "CBL_GET_PROGRAM_INFO" using
                                by value fn
                                by reference param-block
                                             buf
                                             buflen
                                returning status-code.

          if status-code <> 0
            display "CBL_GET_PROGRAM_INFO failed with " status-code
            goback
          end-if.

          display "buf from handle is '" buf "'".

      * close handle
          move 0 to cblte-gpi-flags.
          move 3 to fn.
          call "CBL_GET_PROGRAM_INFO" using
                                by value fn
                                by reference param-block
                                             buf
                                             buflen
                                returning status-code.

          if status-code <> 0
            display "CBL_GET_PROGRAM_INFO failed with " status-code
            goback
          end-if.
        end program prog.

