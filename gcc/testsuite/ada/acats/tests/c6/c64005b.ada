-- C64005B.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT A SUBPROGRAM CAN BE CALLED RECURSIVELY AND THAT NON-LOCAL
-- VARIABLES AND CONSTANTS ARE PROPERLY ACCESSED FROM WITHIN RECURSIVE
-- INVOCATIONS.

-- CPP 7/2/84

WITH REPORT;  USE REPORT;
PROCEDURE C64005B IS

     COUNT : INTEGER := 0;
     TWENTY : CONSTANT INTEGER := 20;
     C1 : CONSTANT INTEGER := 1;
     G1, G2, G3 : INTEGER := 0;
     G4, G5 : INTEGER := 0;

     PROCEDURE R (A1 : INTEGER; A2 : IN OUT INTEGER; A3 : OUT INTEGER)
     IS
          C1 : CONSTANT INTEGER := 5;
          TEN : CONSTANT INTEGER := 10;
          J1, J2 : INTEGER := 1;
          J3 : INTEGER := 0;

          PROCEDURE RECURSE (P1 : INTEGER; P2 : IN OUT INTEGER) IS
               C1 : INTEGER := 2;
          BEGIN     -- RECURSE
               C1 := IDENT_INT (10);
               IF P1 < TWENTY THEN
                    RECURSE (P1 + C1, G2);
                    G1 := G1 + C64005B.C1;
                    G3 := G3 + P1;
                    P2 := P2 + IDENT_INT(2);
                    A2 := A2 + IDENT_INT(1);
                    J2 := J2 + R.C1;
               END IF;
          END RECURSE;

     BEGIN     -- R
          IF A2 < TEN THEN
               A2 := A2 + C1;
               RECURSE (0, J1);
               J3 := J3 + TEN;
               COUNT := COUNT + 1;
               COMMENT ("ON PASS # " & INTEGER'IMAGE(COUNT));
               COMMENT ("VALUE OF A2 IS " & INTEGER'IMAGE(A2));
               COMMENT ("VALUE OF J3 IS " & INTEGER'IMAGE(J3));
               R (0, A2, J3);
               J3 := J3 + A2;
          END IF;
          A3 := J1 + J3;
     END R;

BEGIN
     TEST("C64005B", "RECURSIVE SUBPROGRAMS WITH ALL KINDS " &
          "OF DATA ACCESS");

     R (0, G4, G5);

     IF (COUNT /=  2) OR (G1 /=  4) OR
        (G2    /=  4) OR (G3 /= 20) OR
        (G4    /= 14) OR (G5 /= 35) THEN
          FAILED ("RECURSIVE INVOCATIONS' DATA ACCESS IS NOT" &
                  " WORKING CORRECTLY");
     END IF;

     COMMENT ("VALUE OF COUNT IS " & INTEGER'IMAGE(COUNT));
     COMMENT ("VALUE OF G1 IS " & INTEGER'IMAGE(G1));
     COMMENT ("VALUE OF G2 IS " & INTEGER'IMAGE(G2));
     COMMENT ("VALUE OF G3 IS " & INTEGER'IMAGE(G3));
     COMMENT ("VALUE OF G4 IS " & INTEGER'IMAGE(G4));
     COMMENT ("VALUE OF G5 IS " & INTEGER'IMAGE(G5));

     RESULT;

EXCEPTION
     WHEN PROGRAM_ERROR =>
          FAILED ("PROGRAM_ERROR RAISED");
          COMMENT ("VALUE OF COUNT IS " & INTEGER'IMAGE(COUNT));
          COMMENT ("VALUE OF G1 IS " & INTEGER'IMAGE(G1));
          COMMENT ("VALUE OF G2 IS " & INTEGER'IMAGE(G2));
          COMMENT ("VALUE OF G3 IS " & INTEGER'IMAGE(G3));
          COMMENT ("VALUE OF G4 IS " & INTEGER'IMAGE(G4));
          COMMENT ("VALUE OF G5 IS " & INTEGER'IMAGE(G5));
          RESULT;

END C64005B;
