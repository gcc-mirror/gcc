-- C35003D.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A FLOATING-POINT
--     SUBTYPE INDICATION WHEN THE LOWER OR UPPER BOUND OF A NON-NULL
--     RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C35003D IS

     SUBTYPE FLT1 IS FLOAT RANGE -100.0 .. 100.0;

BEGIN
     TEST ("C35003D", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
                      "FLOATING-POINT SUBTYPE INDICATION WHEN THE " &
                      "LOWER OR UPPER BOUND OF A NON-NULL RANGE LIES " &
                      "OUTSIDE THE RANGE OF THE TYPE MARK");
     BEGIN
          DECLARE
               SUBTYPE F IS FLT1 RANGE 0.0..101.0+FLT1(IDENT_INT(0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED (F1)");
               DECLARE
                    Z : F := 1.0;
               BEGIN
                    IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (F1)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (F1)");
     END;

     BEGIN
          DECLARE
               SUBTYPE F IS FLT1 RANGE -101.0..0.0;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (F2)");
               DECLARE
                    Z : F := -1.0;
               BEGIN
                    IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (F2)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (F2)");
     END;

     RESULT;

END C35003D;
