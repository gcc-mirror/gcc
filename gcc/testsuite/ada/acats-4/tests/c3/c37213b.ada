-- C37213B.ADA

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
-- CHECK THAT IF
--        A DISCRIMINANT CONSTRAINT
-- DEPENDS ON A DISCRIMINANT, THE NON-DISCRIMINANT EXPRESSIONS IN THE
-- CONSTRAINT ARE EVALUATED WHEN THE COMPONENT SUBTYPE DEFINITION IS
-- ELABORATED, BUT THE VALUES ARE CHECKED WHEN THE RECORD TYPE IS:
--
--   CASE B: USED WITHOUT A CONSTRAINT ONLY IN AN ALLOCATOR OR OBJECT
--      DECLARATION.

-- JBG 10/17/86

WITH REPORT; USE REPORT;
PROCEDURE C37213B IS

     SUBTYPE SM IS INTEGER RANGE 1..10;

     TYPE REC (D1, D2 : SM) IS
          RECORD NULL; END RECORD;

     F1_CONS : INTEGER := 2;

     FUNCTION CHK (
          CONS    : INTEGER;
          VALUE   : INTEGER;
          MESSAGE : STRING) RETURN BOOLEAN IS
     BEGIN
          IF CONS /= VALUE THEN
               FAILED (MESSAGE & ": CONS IS " &
                       INTEGER'IMAGE(CONS));
          END IF;
          RETURN TRUE;
     END CHK;

     FUNCTION F1 RETURN INTEGER IS
     BEGIN
          F1_CONS := F1_CONS - IDENT_INT(1);
          RETURN F1_CONS;
     END F1;

BEGIN
     TEST ("C37213B", "CHECK EVALUATION OF DISCRIMINANT EXPRESSIONS " &
                      "WHEN CONSTRAINT DEPENDS ON DISCRIMINANT, " &
                      "AND DISCRIMINANTS HAVE DEFAULTS");

-- CASE B

     DECLARE
          TYPE CONS (D3 : INTEGER := 1) IS
               RECORD
                    C1 : REC (D3, F1);       -- F1 EVALUATED
               END RECORD;
          CHK1 : BOOLEAN := CHK (F1_CONS, 1, "F1 NOT EVALUATED");
          X : CONS;             -- F1 NOT EVALUATED AGAIN
          Y : CONS;             -- F1 NOT EVALUATED AGAIN
          CHK2 : BOOLEAN := CHK (F1_CONS, 1, "F1 EVALUATED");
     BEGIN
          IF X /= (1, (1, 1)) OR Y /= (1, (1, 1)) THEN
               FAILED ("DISCRIMINANT VALUES NOT CORRECT");
          END IF;
     END;

     F1_CONS := 12;

     DECLARE
          TYPE CONS (D3 : INTEGER := 1) IS
               RECORD
                    C1 : REC(D3, F1);
               END RECORD;
     BEGIN
          BEGIN
               DECLARE
                    X : CONS;
               BEGIN
                    FAILED ("DISCRIMINANT CHECK NOT PERFORMED - 1");
                    IF X /= (1, (1, 1)) THEN
                         COMMENT ("SHOULDN'T GET HERE");
                    END IF;
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION - 1");
          END;

          BEGIN
               DECLARE
                    TYPE ACC_CONS IS ACCESS CONS;
                    X : ACC_CONS;
               BEGIN
                    X := NEW CONS;
                    FAILED ("DISCRIMINANT CHECK NOT PERFORMED - 2");
                    BEGIN
                         IF X.ALL /= (1, (1, 1)) THEN
                              COMMENT ("IRRELEVANT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT CHECKED TOO SOON - 2");
          END;

          BEGIN
               DECLARE
                    SUBTYPE SCONS IS CONS;
               BEGIN
                    DECLARE
                         X : SCONS;
                    BEGIN
                         FAILED ("DISCRIMINANT CHECK NOT " &
                                 "PERFORMED - 3");
                         IF X /= (1, (1, 1)) THEN
                              COMMENT ("IRRELEVANT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT CHECKED TOO SOON - 3");
          END;

          BEGIN
               DECLARE
                    TYPE ARR IS ARRAY (1..5) OF CONS;
               BEGIN
                    DECLARE
                         X : ARR;
                    BEGIN
                         FAILED ("DISCRIMINANT CHECK NOT " &
                                 "PERFORMED - 4");
                         IF X /= (1..5 => (1, (1, 1))) THEN
                              COMMENT ("IRRELEVANT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT CHECKED TOO SOON - 4");
          END;

          BEGIN
               DECLARE
                    TYPE NREC IS
                         RECORD
                              C1 : CONS;
                         END RECORD;
               BEGIN
                    DECLARE
                         X : NREC;
                    BEGIN
                         FAILED ("DISCRIMINANT CHECK NOT " &
                                 "PERFORMED - 5");
                         IF X /= (C1 => (1, (1, 1))) THEN
                              COMMENT ("IRRELEVANT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 5");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT CHECKED TOO SOON - 5");
          END;

          BEGIN
               DECLARE
                    TYPE DREC IS NEW CONS;
               BEGIN
                    DECLARE
                         X : DREC;
                    BEGIN
                         FAILED ("DISCRIMINANT CHECK NOT " &
                                 "PERFORMED - 6");
                         IF X /= (1, (1, 1)) THEN
                              COMMENT ("IRRELEVANT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 6");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT CHECKED TOO SOON - 6");
          END;

     END;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ("CONSTRAINT CHECK DONE TOO EARLY");
          RESULT;

END C37213B;
