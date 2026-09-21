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
        01 param-block type cblt-prog-info-params.
        procedure division.
          display "calling a from prog".
          call "a" using param-block.
          display "calling c from prog".
          call "c" using param-block.
        end program prog.

        identification division.
        program-id. a.
        data division.
        linkage section.
        copy "cbltypes.cpy".
        01 param-block type cblt-prog-info-params.
        procedure division using param-block.
          display "calling b from a".
          call "b" using param-block.
        end program a.

        identification division.
        program-id. c.
        data division.
        working-storage section.
        77 fn pic x(4) comp-5.
        77 status-code pic x(2) comp-5 value 0.
        77 buf pic x(128).
        77 buflen pic x(4) comp-5.
        linkage section.
        copy "cbltypes.cpy".
        01 param-block type cblt-prog-info-params.
        procedure division using param-block.
          move length of buf to buflen.
          perform until status-code <> 0
      * retrieve program name from handle
            move 6 to cblte-gpi-flags
            move 2 to fn
            move x"00" to buf
            call "CBL_GET_PROGRAM_INFO" using
                                  by value fn
                                  by reference param-block
                                              buf
                                              buflen
                                  returning status-code

            if status-code = 0
              display "buf from c is '" buf "'"
            end-if
          end-perform.

          close-handle section.
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
        end program c.

        identification division.
        program-id. b.
        data division.
        working-storage section.
        copy "cbltypes.cpy".
        77 fn pic x(4) comp-5.
        77 status-code pic x(2) comp-5.
        77 buf pic x(128).
        77 buflen pic x(4) comp-5.
        linkage section.
        copy "cbltypes.cpy".
        01 param-block type cblt-prog-info-params.
        procedure division using param-block.
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

          display "buf from b is '" buf "'".
        end program b.

