-- C43105B.ADA

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
-- OBJECTIVE:
--     IN A RECORD AGGREGATE (X => E, Y => E), WHERE E IS AN OVERLOADED
--     FUNCTION CALL, OVERLOADING RESOLUTION OCCURS SEPARATELY FOR THE
--     DIFFERENT OCCURRENCES OF E.

-- HISTORY:
--     DHH 09/07/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43105B IS
BEGIN
     TEST ("C43105B", "IN A RECORD AGGREGATE (X => E, Y => E), WHERE " &
                      "E IS AN OVERLOADED FUNCTION CALL, OVERLOADING " &
                      "RESOLUTION OCCURS SEPARATELY FOR THE " &
                      "DIFFERENT OCCURRENCES OF E");

     DECLARE
          TYPE COLOR IS (RED, YELLOW, GREEN);
          TYPE PALETTE IS (GREEN, YELLOW, RED);

          TYPE REC IS
               RECORD
                    X : COLOR;
                    Y : PALETTE;
               END RECORD;

          TYPE RECD IS
               RECORD
                    X : PALETTE;
                    Y : COLOR;
               END RECORD;

          REC1 : REC;
          REC2 : RECD;

          FUNCTION IDENT_C(C : COLOR) RETURN COLOR IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN C;
               ELSE
                    RETURN GREEN;
               END IF;
          END IDENT_C;

          FUNCTION IDENT_C(P : PALETTE) RETURN PALETTE IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN P;
               ELSE
                    RETURN RED;
               END IF;
          END IDENT_C;

     BEGIN
          REC1 := (X => IDENT_C(YELLOW), Y => IDENT_C(YELLOW));
          REC2 := (X => IDENT_C(YELLOW), Y => IDENT_C(YELLOW));

          IF REC1.X /= REC2.Y THEN
               FAILED("COLOR FUNCTION RESOLUTION FAILED");
          END IF;

          IF REC1.Y /= REC2.X THEN
               FAILED("PALETTE FUNCTION RESOLUTION FAILED");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED("EXCEPTION RAISED");
     END;
     RESULT;
END C43105B;
