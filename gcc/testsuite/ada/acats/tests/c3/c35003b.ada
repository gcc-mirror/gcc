-- C35003B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A SUBTYPE INDICATION
--     OF A DISCRETE GENERIC FORMAL TYPE WHEN THE LOWER OR UPPER BOUND
--     OF A NON-NULL RANGE LIES OUTSIDE THE RANGE OF THE TYPE MARK.

-- HISTORY:
--     JET 07/08/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C35003B IS

     TYPE ENUM IS (WE, LOVE, WRITING, TESTS);
     TYPE INT IS RANGE -10..10;

     GENERIC
          TYPE GEN_ENUM IS (<>);
          TYPE GEN_INT IS RANGE <>;
     PACKAGE GEN_PACK IS
          SUBTYPE SUBENUM IS GEN_ENUM RANGE
               GEN_ENUM'SUCC(GEN_ENUM'FIRST) ..
               GEN_ENUM'PRED(GEN_ENUM'LAST);
          SUBTYPE SUBINT IS GEN_INT RANGE
               GEN_INT'SUCC(GEN_INT'FIRST) ..
               GEN_INT'PRED(GEN_INT'LAST);
          TYPE A1 IS ARRAY (0..GEN_INT'LAST) OF INTEGER;
          TYPE A2 IS ARRAY (GEN_INT RANGE GEN_INT'FIRST..0) OF INTEGER;
     END GEN_PACK;

     PACKAGE BODY GEN_PACK IS
     BEGIN
          TEST ("C35003B", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                           "FOR A SUBTYPE INDICATION OF A DISCRETE " &
                           "GENERIC FORMAL TYPE WHEN THE LOWER OR " &
                           "UPPER BOUND OF A NON-NULL RANGE LIES " &
                           "OUTSIDE THE RANGE OF THE TYPE MARK");
          BEGIN
               DECLARE
                    SUBTYPE SUBSUBENUM IS SUBENUM RANGE
                         GEN_ENUM'FIRST..SUBENUM'LAST;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED (E1)");
                    DECLARE
                         Z : SUBSUBENUM := SUBENUM'FIRST;
                    BEGIN
                         IF NOT EQUAL(SUBSUBENUM'POS(Z),
                                      SUBSUBENUM'POS(Z)) THEN
                              COMMENT ("DON'T OPTIMIZE Z");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG " &
                                 "PLACE (E1)");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED (E1)");
          END;

          BEGIN
               DECLARE
                    TYPE A IS ARRAY (SUBENUM RANGE SUBENUM'FIRST ..
                         GEN_ENUM'LAST) OF INTEGER;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED (E2)");
                    DECLARE
                         Z : A := (OTHERS => 0);
                    BEGIN
                         IF NOT EQUAL(Z(SUBENUM'FIRST),
                                      Z(SUBENUM'FIRST)) THEN
                              COMMENT ("DON'T OPTIMIZE Z");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG PLACE " &
                                 "(E2)");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED (E2)");
          END;

          BEGIN
               DECLARE
                    TYPE I IS ACCESS SUBINT RANGE
                         GEN_INT'FIRST..SUBINT'LAST;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED (I1)");
                    DECLARE
                         Z : I := NEW SUBINT'(SUBINT'FIRST);
                    BEGIN
                         IF NOT EQUAL(INTEGER(Z.ALL),INTEGER(Z.ALL))
                         THEN
                              COMMENT ("DON'T OPTIMIZE Z");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG PLACE " &
                                 "(I1)");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED (I1)");
          END;

          BEGIN
               DECLARE
                    TYPE I IS NEW
                         SUBINT RANGE SUBINT'FIRST..GEN_INT'LAST;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED (I2)");
                    DECLARE
                         Z : I := I'FIRST;
                    BEGIN
                         IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                              COMMENT ("DON'T OPTIMIZE Z");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG PLACE " &
                                 "(I2)");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED (I2)");
          END;

          BEGIN
               DECLARE
                    SUBTYPE I IS SUBINT RANGE A1'RANGE;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED (R1)");
                    DECLARE
                         Z : I := SUBINT'FIRST;
                    BEGIN
                         IF NOT EQUAL(INTEGER(Z),INTEGER(Z)) THEN
                              COMMENT ("DON'T OPTIMIZE Z");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN WRONG PLACE " &
                                 "(R1)");
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
                         FAILED ("EXCEPTION RAISED IN WRONG PLACE " &
                                 "(R2)");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED (R2)");
          END;
     END GEN_PACK;

     PACKAGE ENUM_PACK IS NEW GEN_PACK(ENUM, INT);

BEGIN
     RESULT;
END C35003B;
