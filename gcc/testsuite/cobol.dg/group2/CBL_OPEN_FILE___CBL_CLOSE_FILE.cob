      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/CBL_OPEN_FILE___CBL_CLOSE_FILE.out" }

        identification division.
        program-id. uat_cbl_open_file.

        environment division.
        configuration section.
          source-computer. Posix
        >>if debugging-mode is defined
          with debugging mode
        >>end-if
        .
          object-computer. Posix.

        >>define FILE_NAME as "thisfileshouldneverexist.txt"

        data division.
        working-storage section.
      *> Open as read-only.
        01  access-mode pic x comp-x value 1.
      *> Not supported by gcc-cobol or GnuCOBOL yet.
        01  deny-mode   pic x comp-x value 0.
      *> Reserved value.
        01  device      pic x comp-x value 0.
        01  file-handle pic x(4) comp-5.
        01  file-status pic x(2) comp-5.
        01  fs redefines file-status.
          03 msb pic x.
          03 lsb pic x comp-x.

        procedure division.
          perform open-ro.
          perform open-failed-ro.
      *> Return with explicit status code. Otherwise, return-code
      *> (which should be non-zero since open-failed-ro is expected
      *> to fail) will be returned as the status code.
          goback with normal status 0.

        open-ro.
          move 1 to access-mode.
          display "Opening /dev/null as read-only"
          call "CBL_OPEN_FILE" using Z"/dev/null"
                                     access-mode
                                     deny-mode
                                     device
                                     file-handle
          if return-code <> 0
            display "Failed to open " Z"/dev/null" " with " return-code
          else
            call "CBL_CLOSE_FILE" using file-handle
          end-if.

          exit paragraph.

        open-failed-ro.
      * Open as read-only.
          move 1 to access-mode.
      * Deny both read and write.
          move 0 to deny-mode.
          display "Opening " FILE_NAME " as read-only"
          call "CBL_OPEN_FILE" using FILE_NAME
                                     access-mode
                                     deny-mode
                                     device
                                     file-handle.
          if return-code <> 0
            move return-code to file-status
            display "Expected failure when opening " FILE_NAME
            display "File status MSB: " msb
            display "File status LSB: " lsb
          else
            display "CBL_OPEN_FILE was unexpectedly successful"
          end-if.

          exit paragraph.

        end program uat_cbl_open_file.

