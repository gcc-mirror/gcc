      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/CBL_DELETE_FILE.out" }

        identification division.
        program-id. test_delete_file.
        data division.
        >>define filename as "test_delete_file.cbl.txt"
        >>define invalid-path as "thisfileshouldnotexist.txt"
        working-storage section.
        01 file-status pic x(2) comp-5.
        01 fs redefines file-status.
          03 MSB pic x.
          03 LSB pic x comp-x.
        01 deny-mode pic x comp-x value 0.
        01 access-mode pic x comp-x value 2.
        01 device pic x comp-x value 0.
        01 file-handle pic x(4) comp-5.

        procedure division.
          perform create-file.
          perform delete-file.
          perform delete-invalid-file.
          move zero to return-code.
          goback.

        create-file section.
          call "CBL_CREATE_FILE" using filename
                                       access-mode
                                       deny-mode
                                       device
                                       file-handle.

          if return-code <> 0
            display "CBL_CREATE_FILE failed with " return-code
            goback
          end-if.

          call "CBL_CLOSE_FILE" using file-handle.

          if return-code <> 0
            display "CBL_CLOSE_FILE failed with " return-code
            goback
          end-if.

          exit paragraph.

        delete-file section.
          call "CBL_DELETE_FILE" using filename returning file-status.

          if file-status <> 0
            display "CBL_DELETE_FILE failed with " file-status
          end-if.

          exit paragraph.

        delete-invalid-file section.
          call "CBL_DELETE_FILE" using invalid-path
                                 returning file-status.

          if file-status <> 0
            display "Expected failure when deleting " invalid-path
            display "File status MSB: " MSB
            display "File status LSB: " LSB
          end-if.

          exit paragraph.

        end program test_delete_file.

