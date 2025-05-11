       *> { dg-do run }
       *> { dg-output-file "group2/Multi-target_MOVE_with_subscript_re-evaluation.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID.  mover.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 FILLER.
          02 ADATA VALUE "654321".
          02 A REDEFINES ADATA PIC 9 OCCURS 6 TIMES.
          02 B PIC 9.
          02 CDATA VALUE "999999".
          02 C REDEFINES CDATA PIC 9 OCCURS 6 TIMES.
        01 TEMP PIC 9.
        PROCEDURE DIVISION.
        INITIALIZE CDATA ALL TO VALUE
        MOVE 2 TO B
        MOVE A(B) TO B, C(B)
      *> That should pick up 5, move it to B, and then move 5 to C(5),
        IF CDATA NOT EQUAL TO "999959"
            DISPLAY CDATA " Should be ""999959"", but isn't"
        ELSE
            DISPLAY CDATA " Should be ""999959""".
      *> See 14.9.25.4 MOVE General Rules
        INITIALIZE CDATA ALL TO VALUE
        MOVE 2 TO B
        MOVE A(B) TO TEMP
        MOVE TEMP TO B
        MOVE TEMP TO C(B)
        IF CDATA NOT EQUAL TO "999959"
            DISPLAY CDATA " Should be ""999959"", but isn't"
        ELSE
            DISPLAY CDATA " Should be ""999959""".
        STOP RUN.

