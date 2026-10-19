      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/CBL_GET_PROGRAM_INFO__returned_buffer_length_.out" }

        COPY "cblproto.cpy".


        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROG2 PROTOTYPE.
        PROCEDURE DIVISION.
        END PROGRAM PROG2.

        ID DIVISION.
        PROGRAM-ID. PROG.
        PROCEDURE DIVISION.
          CALL "PROG2".
        END PROGRAM PROG.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. PROG2.
        DATA DIVISION.
        LOCAL-STORAGE SECTION.
        77  UNS-INT                PIC  9(09)       COMP-5 IS TYPEDEF.

        01 SA-FUNCTION             USAGE UNS-INT.
        01 SA-PARM-BLOCK.
            05 SA-P-B-SIZE          USAGE UNS-INT.
            05 SA-P-B-FLAGS         USAGE UNS-INT.
            05 SA-P-B-HANDLE        USAGE POINTER.
            05 SA-P-B-PROG-ID       USAGE POINTER.
            05 SA-P-B-ATTRS         USAGE UNS-INT.
        01 SA-NAME-BUF             PIC X(400).
        01 SA-NAME-LEN             USAGE UNS-INT VALUE 400.
        01 SA-STATUS-CODE          PIC 9(04) COMP-5.
        PROCEDURE DIVISION.
          MOVE ZERO TO SA-FUNCTION.
          MOVE 28   TO SA-P-B-SIZE.
          MOVE 7   TO SA-P-B-FLAGS.
          MOVE ZERO TO SA-P-B-ATTRS.
          DISPLAY "SA-NAME-LEN ON ENTRY IS " SA-NAME-LEN.
          CALL "CBL_GET_PROGRAM_INFO" USING BY VALUE SA-FUNCTION
                            BY REFERENCE   SA-PARM-BLOCK
                            BY REFERENCE   SA-NAME-BUF
                            BY REFERENCE   SA-NAME-LEN
                            RETURNING      SA-STATUS-CODE.

          DISPLAY "STATUS-CODE IS " SA-STATUS-CODE.
          DISPLAY "SA-NAME-LEN ON EXIT IS " SA-NAME-LEN.
          DISPLAY "SA-NAME-BUF ON EXIT IS " SA-NAME-BUF.

          PERFORM UNTIL SA-STATUS-CODE <> 0
            MOVE 2 TO SA-FUNCTION
            MOVE 28 TO SA-P-B-SIZE
            DISPLAY "SA-NAME-LEN ON ENTRY IS " SA-NAME-LEN
            CALL "CBL_GET_PROGRAM_INFO" USING BY VALUE SA-FUNCTION
                              BY REFERENCE   SA-PARM-BLOCK
                              BY REFERENCE   SA-NAME-BUF
                              BY REFERENCE   SA-NAME-LEN
                              RETURNING      SA-STATUS-CODE

            DISPLAY "STATUS-CODE IS " SA-STATUS-CODE

            IF SA-STATUS-CODE = 0
              DISPLAY "SA-NAME-LEN ON EXIT IS " SA-NAME-LEN
              DISPLAY "SA-NAME-BUF ON EXIT IS " SA-NAME-BUF
            END-IF
          END-PERFORM.

          IF SA-STATUS-CODE <> 500
            DISPLAY "CBL_GET_PROGRAM_INFO failed with " SA-STATUS-CODE
          END-IF.
        END PROGRAM PROG2.

