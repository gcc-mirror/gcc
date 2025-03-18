*> { dg-do run }
*> { dg-output {000000259(\n|\r\n|\r)} }
*> { dg-output {0x0000000000000103(\n|\r\n|\r)} }
*> { dg-output {Faith       (\n|\r\n|\r)} }
*> { dg-output {Hope        (\n|\r\n|\r)} }
*> { dg-output {Charity     (\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly equal    (\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly different(\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly different(\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly different(\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly different(\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly equal    (\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly equal    (\n|\r\n|\r)} }
*> { dg-output {Pointers are correctly equal    (\n|\r\n|\r)} }
*> { dg-output {NOT EQUAL is correctly FALSE    } }
       ID DIVISION.
       PROGRAM-ID. pointers.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-POINTER USAGE IS POINTER .
       01 WS-PVALUE REDEFINES WS-POINTER PIC 9(9) COMP-5.
       01 WS-POINTER2 USAGE IS POINTER .
       01 WS-PVALUE2 REDEFINES WS-POINTER2 PIC 9(9) COMP-5.
       01 VALUE-SOURCE1 PIC X(12).
       01 VALUE-SOURCE2 PIC X(12).
       01 VALUE-SOURCE3 PIC X(12).
       01 VALUE-DEST   PIC X(12).
       LINKAGE SECTION.
       01 DEREFERENCER PIC X(12).
       PROCEDURE DIVISION.
       MOVE 259 TO WS-PVALUE
       DISPLAY WS-PVALUE
       DISPLAY WS-POINTER
      *> Pointer manipulation: ADDRESS OF to ADDRESS OF
       MOVE "Faith" TO VALUE-SOURCE1
       SET ADDRESS OF DEREFERENCER TO ADDRESS OF VALUE-SOURCE1
       MOVE DEREFERENCER TO VALUE-DEST
       DISPLAY VALUE-DEST
      *> Pointer manipulation: POINTER to ADDRESS OF
      *>                       ADDRESS OF to POINTER
       MOVE "Hope" TO VALUE-SOURCE2
       SET WS-POINTER TO ADDRESS OF VALUE-SOURCE2
       SET ADDRESS OF DEREFERENCER TO WS-POINTER
       DISPLAY DEREFERENCER
      *> Pointer manipulation: Pointer to pointer:
       MOVE "Charity" TO VALUE-SOURCE3
       SET WS-POINTER2 TO ADDRESS OF VALUE-SOURCE3
       SET WS-POINTER TO WS-POINTER2
       SET ADDRESS OF DEREFERENCER TO WS-POINTER
       DISPLAY DEREFERENCER
       IF WS-POINTER EQUAL TO WS-POINTER2
           DISPLAY "Pointers are correctly equal    "
       ELSE
           DISPLAY "Pointers are incorrectly different".
       SET WS-POINTER2 TO ADDRESS OF VALUE-DEST
       IF WS-POINTER EQUAL TO WS-POINTER2
           DISPLAY "Pointers are incorrectly equal"
       ELSE
           DISPLAY "Pointers are correctly different"
       SET WS-POINTER TO NULL
       IF WS-POINTER EQUAL TO WS-POINTER2
           DISPLAY "Pointers are incorrectly equal"
       ELSE
           DISPLAY "Pointers are correctly different"
       IF NULL EQUAL TO WS-POINTER2
           DISPLAY "Pointers are incorrectly equal"
       ELSE
           DISPLAY "Pointers are correctly different"
       IF WS-POINTER2 EQUAL TO NULL
           DISPLAY "Pointers are incorrectly equal"
       ELSE
           DISPLAY "Pointers are correctly different"
       SET WS-POINTER2 TO NULL
       IF WS-POINTER EQUAL TO WS-POINTER2
           DISPLAY "Pointers are correctly equal    "
       ELSE
           DISPLAY "Pointers are incorrectly different".
       IF WS-POINTER EQUAL TO NULL
           DISPLAY "Pointers are correctly equal    "
       ELSE
           DISPLAY "Pointers are incorrectly different".
       IF WS-POINTER EQUAL TO NULL
           DISPLAY "Pointers are correctly equal    "
       ELSE
           DISPLAY "Pointers are incorrectly different".
       PERFORM one-last-dance
       STOP RUN.
       one-last-dance.
       IF WS-POINTER NOT EQUAL TO NULL
      *>Making sure comments don't cause trouble
           DISPLAY "Pointers are incorrectly EQUAL  "
       ELSE
      *>Making sure comments don't cause trouble
           DISPLAY "NOT EQUAL is correctly FALSE    "
       END-IF.
       one-last-dance-end.
           DISPLAY "We should never get here".
       END PROGRAM pointers.
