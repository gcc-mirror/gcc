       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  Y            PIC   X(4).
       01  TEST-FLD.
           05  TEST-DATA  PIC X(14).
               88  VALID-DATA   VALUE 'defxabczz55666'.
           05  TEST-UNSET PIC X VALUE '_'.
               88  VALID-UNSET  VALUE '_'.
       PROCEDURE        DIVISION.
           MOVE "defx" TO Y.
           STRING FUNCTION CONCAT ( Y "abc" "zz" "55" "666" )
                  DELIMITED BY SIZE
                  INTO TEST-FLD
           END-STRING.
           EVALUATE TRUE
              WHEN NOT VALID-UNSET
                 DISPLAY "FUNCTION result too long"
                 END-DISPLAY
              WHEN TEST-DATA
                <> FUNCTION CONCAT ( Y "abc" "zz" "55" "666" )
                 DISPLAY "CONCAT issue, '" TEST-DATA
                     "' vs. '"
                     FUNCTION CONCAT ( Y "abc" "zz" "55" "666" ) "'"
                 END-DISPLAY
              WHEN VALID-DATA
                 CONTINUE
              WHEN OTHER
                 DISPLAY TEST-DATA
                 END-DISPLAY
           END-EVALUATE.
           STOP RUN.

