-- C41206A.ADA

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
-- CHECK THAT A RANGE L..R, WHERE L=SUCC(R) CAN BE USED TO FORM
--   A NULL SLICE FROM AN ARRAY WHEN:
--        BOTH L AND R SATISFY THE INDEX CONSTRAINT;
--        L SATISFIES THE INDEX CONSTRAINT, R DOES NOT (BUT IT
--             BELONGS TO THE BASE TYPE OF THE INDEX);
--        L SATISFIES THE CONSTRAINT IMPOSED BY THE TYPE MARK OF
--             THE INDEX, BUT NOT THE CONSTRAINT ASSOCIATED WITH
--             THE INDEX;
--        THE ARRAY IS NULL, AND L IS IN THE RANGE OF THE INDEX SUBTYPE.

-- WKB 8/10/81

WITH REPORT;
USE REPORT;
PROCEDURE C41206A IS

     TYPE SMALL IS RANGE 1..100;
     TYPE T IS ARRAY (SMALL RANGE <> ) OF INTEGER;
     SUBTYPE T1 IS T(5..10);
     A : T1 := (5,6,7,8,9,10);
     B : T(8..7) := (8..7 => 1);

BEGIN
     TEST ("C41206A", "USING A RANGE L..R, WHERE L=SUCC(R), " &
                      "TO FORM A NULL SLICE FROM AN ARRAY");

     BEGIN
          IF A (7..6) /= B OR A (SMALL(IDENT_INT(7))..6) /= B THEN
               FAILED ("SLICE NOT NULL - 1");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED - 1");
     END;

     BEGIN
          IF A (5..4) /= B OR A (SMALL(IDENT_INT(5))..4) /= B THEN
               FAILED ("SLICE NOT NULL - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED - 2");
     END;

     BEGIN
          IF A (50..49) /= B OR A (SMALL(IDENT_INT(50))..49) /= B THEN
               FAILED ("SLICE NOT NULL - 3");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED - 3");
     END;

     BEGIN
          IF B (50..49) /= B OR B (SMALL(IDENT_INT(50))..49) /= B THEN
               FAILED ("SLICE NOT NULL - 4");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED - 4");
     END;

     RESULT;
END C41206A;
