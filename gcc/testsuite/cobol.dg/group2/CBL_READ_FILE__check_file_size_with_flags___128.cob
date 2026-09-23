      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }

        identification division.
        program-id. uat_file_size.

        environment division.
        configuration section.
          source-computer. Posix
        >>if debugging-mode is defined
          with debugging mode
        >>end-if
        .
          object-computer. Posix.

        data division.
        >>define filename as "test_file_size.cbl.txt"
        >>define buffer as "hi, this text is exactly 38 bytes long"
        working-storage section.
          01 file-handle pic x(4) comp-5.
          01 access-mode pic x comp-x.
          01 deny-mode pic x comp-x value 0.
          01 device pic x comp-x value 0.
          01 file-offset    	PIC X(8) COMP-x.
          01 byte-count     	pic x(4) comp-x.
          01 flags            pic x comp-x value 0.
          01 dummy            pic x.

        procedure division.
          perform write-file.
          perform check-file-size.
          move zero to return-code.
          call "CBL_DELETE_FILE" using filename
          goback.

        write-file section.
          *> Open as write-only.
          move 2 to access-mode.
          call "CBL_CREATE_FILE" using filename
                                       access-mode
                                       deny-mode
                                       device
                                       file-handle.

          if return-code <> 0
            display "CBL_CREATE_FILE failed with " return-code
            goback
          end-if.

          move 0 to file-offset.
          move function byte-length(buffer) to byte-count.

          call "CBL_WRITE_FILE" using file-handle
                                      file-offset
                                      byte-count
                                      flags
                                      buffer.

          if return-code <> 0
            display "CBL_WRITE_FILE failed with " return-code
            goback
          end-if.

          call "CBL_CLOSE_FILE" using file-handle.

          if return-code <> 0
            display "CBL_CLOSE_FILE failed with " return-code
          end-if.

          exit paragraph.

        check-file-size section.
          *> Open as read-only.
          move 1 to access-mode.
          call "CBL_OPEN_FILE" using filename
                                     access-mode
                                     deny-mode
                                     device
                                     file-handle.

          if return-code <> 0
            display "Failed to open file " filename
            display "CBL_OPEN_FILE failed with " return-code
            goback
          end-if.

          *> Obtain file size.
          move 128 to flags.

          call "CBL_READ_FILE" using file-handle
                                     file-offset
                                     byte-count
                                     flags
                                     dummy.

          if return-code <> 0
            display "CBL_READ_FILE failed with " return-code
            goback
          else if file-offset <> function byte-length(buffer)
            display "File size mismatch. "
              "Expected " function byte-length(buffer) " bytes, "
              "got " file-offset
            goback
          end-if.

          call "CBL_CLOSE_FILE" using file-handle.

          if return-code <> 0
            display "CBL_CLOSE_FILE failed with " return-code
          end-if.

          exit paragraph.

        end program uat_file_size.

