       *> { dg-do run }
       *> { dg-output-file "group2/SEARCH_ALL_with_OCCURS_DEPENDING_ON.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
       77  SCREEN-AKT         PIC 9(02) VALUE 0.
       01  SCREEN-TAB.
           03 SCREEN-ENTRY    OCCURS 0  TO  20
                              DEPENDING ON  SCREEN-AKT
                              ASCENDING KEY SCREEN-NAME
                              INDEXED   BY  SCREEN-IDX.
             05 SCREEN-NAME     PIC X(02).

       PROCEDURE DIVISION.

           SEARCH ALL SCREEN-ENTRY
              AT END
                 DISPLAY 'END'
              WHEN SCREEN-NAME (SCREEN-IDX) = 'AB'
                 DISPLAY 'FOUND'
           END-SEARCH
           MOVE 1 TO SCREEN-AKT
           MOVE 'AB' TO  SCREEN-NAME (1)
           SEARCH ALL SCREEN-ENTRY
              AT END
                 DISPLAY 'END'
              WHEN SCREEN-NAME (SCREEN-IDX) = 'AB'
                 DISPLAY 'FOUND'
           END-SEARCH
           MOVE 2 TO SCREEN-AKT
           MOVE 'CD' TO  SCREEN-NAME (2)
           SEARCH ALL SCREEN-ENTRY
              AT END
                 DISPLAY 'END'
              WHEN SCREEN-NAME (SCREEN-IDX) = 'CD'
                 DISPLAY 'FOUND'
           END-SEARCH
           EXIT PROGRAM.

