-- C37005A.ADA

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
-- CHECK THAT SCALAR RECORD COMPONENTS MAY HAVE NON-STATIC
-- RANGE CONSTRAINTS OR DEFAULT INITIAL VALUES.

-- DAT 3/6/81
-- JWC 6/28/85   RENAMED TO -AB
-- EDS 7/16/98   AVOID OPTIMIZATION

WITH REPORT;
PROCEDURE C37005A IS

     USE REPORT;

BEGIN
     TEST ("C37005A", "SCALAR RECORD COMPONENTS MAY HAVE NON-STATIC"
                    & " RANGE CONSTRAINTS OR DEFAULT INITIAL VALUES");

     DECLARE
          SUBTYPE DT IS INTEGER RANGE IDENT_INT (1) .. IDENT_INT (5);
          L : INTEGER := IDENT_INT (DT'FIRST);
          R : INTEGER := IDENT_INT (DT'LAST);
          SUBTYPE DT2 IS INTEGER RANGE L .. R;
          M : INTEGER := (L + R) / 2;

          TYPE REC IS
               RECORD
                    C1 : INTEGER := M;
                    C2 : DT2 := (L + R) / 2;
                    C3 : BOOLEAN RANGE (L < M) .. (R > M)
                         := IDENT_BOOL (TRUE);
                    C4 : INTEGER RANGE L .. R := DT'FIRST;
               END RECORD;

          R1, R2 : REC := ((L+R)/2, M, M IN DT, L);
          R3 : REC;
     BEGIN
          IF R3 /= R1
          THEN
               FAILED ("INCORRECT RECORD VALUES");
          END IF;

          R3 := (R2.C2, R2.C1, R3.C3, R);  -- CONSTRAINTS CHECKED BY :=
          IF EQUAL(IDENT_INT(1), 2) THEN
               FAILED("IMPOSSIBLE " & INTEGER'IMAGE(R3.C1));  --USE R3
          END IF;

          BEGIN
               R3 := (M, M, IDENT_BOOL (FALSE), M); -- RAISES CON_ERR.
               FAILED ("CONSTRAINT ERROR NOT RAISED " & INTEGER'IMAGE(R3.C1));
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION");
          END;

          FOR I IN DT LOOP
               R3 := (I, I, I /= 100, I);
               R1.C2 := I;
               IF EQUAL(IDENT_INT(1), 2) THEN
                    FAILED("IMPOSSIBLE " & 
                           INTEGER'IMAGE(R3.C1 + R1.C2));  --USE R3, R1
               END IF;
          END LOOP;

     EXCEPTION
          WHEN OTHERS => FAILED ("INVALID EXCEPTION");
     END;

     RESULT;
END C37005A;
