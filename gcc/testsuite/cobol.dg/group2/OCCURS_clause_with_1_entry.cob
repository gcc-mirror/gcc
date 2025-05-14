       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  D1.
           03  FILLER   OCCURS 1.
               05 D1-ENTRY   PIC X(03) value '123'.
       01  D2.
           03  D2-ENTRY   PIC X(03)  value 'ABC'  OCCURS 1.
       01  D1TOR.
           03  FILLER   PIC X(03) value '456'.
       01  D1-R         REDEFINES D1TOR.
           03  FILLER   OCCURS 1.
               05 D1-R-ENTRY   PIC X(03).
       01  D2TOR.
           03  FILLER   PIC X(03) value 'DEF'.
       01  D2-R         REDEFINES D2TOR.
           03  D2-R-ENTRY   PIC X(03)   OCCURS 1.

       PROCEDURE        DIVISION.
           IF D1-ENTRY (1) NOT = "123"
              DISPLAY D1-ENTRY (1)
              END-DISPLAY
           END-IF.
           IF D2-ENTRY (1) NOT = "ABC"
              DISPLAY D2-ENTRY (1)
              END-DISPLAY
           END-IF.
           IF D1-R-ENTRY (1) NOT = "456"
              DISPLAY D1-R-ENTRY (1)
              END-DISPLAY
           END-IF.
           IF D2-R-ENTRY (1) NOT = "DEF"
              DISPLAY D2-R-ENTRY (1)
              END-DISPLAY
           END-IF.
           STOP RUN.

