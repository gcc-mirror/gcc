       *> { dg-do run }
       *> { dg-output-file "group2/COMPUTE_multiplication_to_FIX4.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. onsize.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  FIX4DISPLAY                PIC 9(4) DISPLAY.
        01  FIX8DISPLAY                PIC 9(8) DISPLAY VALUE 12345678.
        01  FIX8BINARY                 PIC 9(8) BINARY  VALUE 12345678.
        01  FIX8PACKED                 PIC 9(8) PACKED-DECIMAL  VALUE 12345678.
        01  FIX8NUMEDT                 PIC 9(8).0       VALUE 12345678.
        01  FLOATSHORT                 FLOAT-SHORT      VALUE 12345678.
        01  FLOATLONG                  FLOAT-LONG       VALUE 12345678.
        01  FLOATEXT                   FLOAT-EXTENDED   VALUE 12345678.

        PROCEDURE       DIVISION.

      *> FIX8DISPLAY
            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8DISPLAY without SIZE ERROR"
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FIX8DISPLAY
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8DISPLAY with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8DISPLAY
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FIX8DISPLAY
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FIX8BINARY

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8BINARY without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8BINARY
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8BINARY with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8BINARY
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FIX8BINARY
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FIX8PACKED

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8PACKED without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8PACKED
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8PACKED with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8PACKED
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FIX8PACKED
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FIX8NUMEDT

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8NUMEDT without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8NUMEDT
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FIX8NUMEDT with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FIX8NUMEDT
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FIX8NUMEDT
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FLOATSHORT

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATSHORT without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATSHORT
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATSHORT with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATSHORT
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FLOATSHORT
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FLOATLONG

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATLONG without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATLONG
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATLONG with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATLONG
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FLOATLONG
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     "."

      *> FLOATEXT

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATEXT without SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATEXT
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 5678"
            DISPLAY     "."

            DISPLAY     "COMPUTE FIX4DISPLAY = FLOATEXT with SIZE ERROR"
            COMPUTE     FIX4DISPLAY = FLOATEXT
            MOVE        9876        TO FIX4DISPLAY
            COMPUTE     FIX4DISPLAY = FLOATEXT
                            ON SIZE ERROR Display "Proper size error"
                        NOT ON SIZE ERROR Display "Improper no error"
            END-COMPUTE
            DISPLAY     "FIX4DISPLAY is " FIX4DISPLAY
            DISPLAY     "Should      be 9876"
            DISPLAY     ".".

            STOP RUN.
        END PROGRAM onsize.

