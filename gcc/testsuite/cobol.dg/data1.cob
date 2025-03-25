*> { dg-do run }
*> { dg-output {1.2345678E\+07(\n|\r\n|\r)} }
*> { dg-output {1.2345678E\+07(\n|\r\n|\r)} }
        IDENTIFICATION DIVISION.
        PROGRAM-ID. data1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  FLOATLONG                  FLOAT-LONG       VALUE 12345678.
        01  FLOATEXT                   FLOAT-EXTENDED   VALUE 12345678.
        PROCEDURE       DIVISION.
            DISPLAY FLOATLONG
            DISPLAY FLOATEXT
            GOBACK.
        END PROGRAM data1.
