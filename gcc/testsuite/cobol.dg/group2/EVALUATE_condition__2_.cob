       *> { dg-do run }
       *> { dg-output-file "group2/EVALUATE_condition__2_.out" }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      prog.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
           01  XVAL PIC X VALUE '_'.
               88  UNDERSCORE  VALUE '_'.
        PROCEDURE        DIVISION.
           DISPLAY 'Next line should be "UNDERSCORE evaluates to TRUE"'
           EVALUATE TRUE
              WHEN NOT UNDERSCORE
                 DISPLAY
                     "***IMPROPERLY*** NOT UNDERSCORE evaluates to TRUE"
                 END-DISPLAY
           END-EVALUATE.
           EVALUATE TRUE
              WHEN UNDERSCORE
                 DISPLAY "UNDERSCORE evaluates to TRUE"
                 END-DISPLAY
           END-EVALUATE.

           DISPLAY
               'Next line should be "NOT UNDERSCORE evaluates to FALSE"'
           EVALUATE FALSE
              WHEN NOT UNDERSCORE
                 DISPLAY "NOT UNDERSCORE evaluates to FALSE"
                 END-DISPLAY
           END-EVALUATE.
           EVALUATE FALSE
              WHEN UNDERSCORE
                 DISPLAY
                        "***IMPROPERLY*** UNDERSCORE evaluates to FALSE"
                 END-DISPLAY
           END-EVALUATE.
           STOP RUN.

