-- C35003A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR AN INTEGER OR
--     ENUMERATION SUBTYPE INDICATION WHEN THE LOWER OR UPPER BOUND
--     OF A NON-NULL RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 01/25/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C35003A IS

     TYPE ENUM IS (ZERO, ONE, TWO, THREE);
     SUBTYPE SUBENUM IS ENUM RANGE ONE..TWO;
     TYPE INT IS RANGE 1..10;
     SUBTYPE SUBINT IS INTEGER RANGE -10..10;
     TYPE A1 IS ARRAY (0..11) OF INTEGER;
     TYPE A2 IS ARRAY (INTEGER RANGE -11..10) OF INTEGER;

BEGIN
     TEST ("C35003A", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR AN " &
                      "INTEGER OR ENUMERATION SUBTYPE INDICATION " &
                      "WHEN THE LOWER OR UPPER BOUND OF A NON-NULL " &
                      "RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK");
     BEGIN
          DECLARE
               SUBTYPE SUBSUBENUM IS SUBENUM RANGE ZERO..TWO;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (E1)");
               DECLARE
                    Z : SUBSUBENUM := ONE;
               BEGIN
                    IF NOT EQUAL(SUBSUBENUM'POS(Z),SUBSUBENUM'POS(Z))
                    THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (E1)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (E1)");
     END;

     BEGIN
          DECLARE
               TYPE A IS ARRAY (SUBENUM RANGE ONE..THREE) OF INTEGER;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (E2)");
               DECLARE
                    Z : A := (OTHERS => 0);
               BEGIN
                    IF NOT EQUAL(Z(ONE),Z(ONE)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (E2)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (E2)");
     END;

     BEGIN
          DECLARE
               TYPE I IS ACCESS INT RANGE INT(IDENT_INT(0))..10;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (I1)");
               DECLARE
                    Z : I := NEW INT'(1);
               BEGIN
                    IF NOT EQUAL(INTEGER(Z.ALL),INTEGER(Z.ALL)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (I1)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (I1)");
     END;

     BEGIN
          DECLARE
               TYPE I IS NEW INT RANGE 1..INT'SUCC(10);
          BEGIN
               FAILED ("NO EXCEPTION RAISED (I2)");
               DECLARE
                    Z : I := 1;
               BEGIN
                    IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (I2)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (I2)");
     END;

     BEGIN
          DECLARE
               TYPE R IS RECORD
                    A : SUBINT RANGE IDENT_INT(-11)..0;
               END RECORD;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (S1)");
               DECLARE
                    Z : R := (A => 1);
               BEGIN
                    IF NOT EQUAL(INTEGER(Z.A),INTEGER(Z.A)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (S1)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (S1)");
     END;

     BEGIN
          DECLARE
               Z : SUBINT RANGE 0..IDENT_INT(11) := 0;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (S2)");
               IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                    COMMENT ("DON'T OPTIMIZE Z");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (S2)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (S2)");
     END;

     BEGIN
          DECLARE
               SUBTYPE I IS SUBINT RANGE A1'RANGE;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (R1)");
               DECLARE
                    Z : I := 1;
               BEGIN
                    IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (R1)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (R1)");
     END;

     BEGIN
          DECLARE
               SUBTYPE I IS SUBINT RANGE A2'RANGE;
          BEGIN
               FAILED ("NO EXCEPTION RAISED (R2)");
               DECLARE
                    Z : I := 1;
               BEGIN
                    IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                         COMMENT ("DON'T OPTIMIZE Z");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN WRONG PLACE (R2)");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED (R2)");
     END;

     RESULT;

END C35003A;
