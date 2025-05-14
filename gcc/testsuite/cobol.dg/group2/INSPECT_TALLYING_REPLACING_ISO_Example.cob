       *> { dg-do run }
       *> { dg-output-file "group2/INSPECT_TALLYING_REPLACING_ISO_Example.out" }

      *> Example from ISO/IEC 2023 page 1151
        IDENTIFICATION DIVISION.
        PROGRAM-ID. tests.
        PROCEDURE DIVISION.
        CALL "test1"
        CALL "test2"
        CALL "test3"
        goback.
        end program tests.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. test1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 ITEM PIC X(14) VALUE "EFABDBCGABEFGG".
        01 COUNT-0 PIC 99 VALUE 0.
        01 COUNT-1 PIC 99 VALUE 0.
        01 COUNT-2 PIC 99 VALUE 0.
        01 COUNT-3 PIC 99 VALUE 0.
        01 COUNT-4 PIC 99 VALUE 0.
        PROCEDURE DIVISION.
        INSPECT ITEM TALLYING
        COUNT-0 FOR ALL "AB", ALL "D"
        COUNT-1 FOR ALL "BC"
        COUNT-2 FOR LEADING "EF"
        COUNT-3 FOR LEADING "B"
        COUNT-4 FOR CHARACTERS;
        INSPECT ITEM REPLACING
        ALL "AB" BY "XY", "D" BY "X"
        ALL "BC" BY "VW"
        LEADING "EF" BY "TU"
        LEADING "B" BY "S"
        FIRST "G" BY "R"
        FIRST "G" BY "P"
        CHARACTERS BY "Z"
        DISPLAY "Counts are: "
                COUNT-0 SPACE 
                COUNT-1 SPACE
                COUNT-2 SPACE
                COUNT-3 SPACE
                COUNT-4
        DISPLAY "Should be:  "
                "03" SPACE 
                "01" SPACE
                "01" SPACE
                "00" SPACE
                "05"
        DISPLAY "Result is " """" ITEM """"
        MOVE "TUXYXVWRXYZZPZ" TO  ITEM
        DISPLAY "Should be " """" ITEM """"
        GOBACK.
        END PROGRAM test1.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. test2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 ITEM PIC X(6) VALUE "BABABC".
        01 COUNT-0 PIC 99 VALUE 0.
        01 COUNT-1 PIC 99 VALUE 0.
        01 COUNT-2 PIC 99 VALUE 0.
        01 COUNT-3 PIC 99 VALUE 0.
        01 COUNT-4 PIC 99 VALUE 0.
        PROCEDURE DIVISION.
        INSPECT ITEM TALLYING
        COUNT-0 FOR ALL "AB", ALL "D"
        COUNT-1 FOR ALL "BC"
        COUNT-2 FOR LEADING "EF"
        COUNT-3 FOR LEADING "B"
        COUNT-4 FOR CHARACTERS;
        INSPECT ITEM REPLACING
        ALL "AB" BY "XY", "D" BY "X"
        ALL "BC" BY "VW"
        LEADING "EF" BY "TU"
        LEADING "B" BY "S"
        FIRST "G" BY "R"
        FIRST "G" BY "P"
        CHARACTERS BY "Z"
        DISPLAY "Counts are: "
                COUNT-0 SPACE 
                COUNT-1 SPACE
                COUNT-2 SPACE
                COUNT-3 SPACE
                COUNT-4
        DISPLAY "Should be:  "
                "02" SPACE 
                "00" SPACE
                "00" SPACE
                "01" SPACE
                "01"
        DISPLAY "Result is " """" ITEM """"
        MOVE "SXYXYZ" TO  ITEM
        DISPLAY "Should be " """" ITEM """"
        GOBACK.
        END PROGRAM test2.

        IDENTIFICATION DIVISION.
        PROGRAM-ID. test3.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 ITEM PIC X(4) VALUE "BBBC".
        01 COUNT-0 PIC 99 VALUE 0.
        01 COUNT-1 PIC 99 VALUE 0.
        01 COUNT-2 PIC 99 VALUE 0.
        01 COUNT-3 PIC 99 VALUE 0.
        01 COUNT-4 PIC 99 VALUE 0.
        PROCEDURE DIVISION.
        INSPECT ITEM TALLYING
        COUNT-0 FOR ALL "AB", ALL "D"
        COUNT-1 FOR ALL "BC"
        COUNT-2 FOR LEADING "EF"
        COUNT-3 FOR LEADING "B"
        COUNT-4 FOR CHARACTERS;
        INSPECT ITEM REPLACING
        ALL "AB" BY "XY", "D" BY "X"
        ALL "BC" BY "VW"
        LEADING "EF" BY "TU"
        LEADING "B" BY "S"
        FIRST "G" BY "R"
        FIRST "G" BY "P"
        CHARACTERS BY "Z"
        DISPLAY "Counts are: "
                COUNT-0 SPACE 
                COUNT-1 SPACE
                COUNT-2 SPACE
                COUNT-3 SPACE
                COUNT-4
        DISPLAY "Should be:  "
                "00" SPACE 
                "01" SPACE
                "00" SPACE
                "02" SPACE
                "00"
        DISPLAY "Result is " """" ITEM """"
        MOVE "SSVW" TO  ITEM
        DISPLAY "Should be " """" ITEM """"
        GOBACK.
        END PROGRAM test3.

