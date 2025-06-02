-- C37213F.ADA

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
--   CASE D: USED WITHOUT A CONSTRAINT ONLY IN AN ALLOCATOR OR OBJECT
--      DECLARATION AND THE COMPONENT IS PRESENT IN THE DEFAULT SUBTYPE.

-- JBG 10/17/86

WITH REPORT; USE REPORT;
PROCEDURE C37213F IS

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
     TEST ("C37213F", "CHECK EVALUATION OF DISCRIMINANT EXPRESSIONS " &
                      "WHEN CONSTRAINT DEPENDS ON DISCRIMINANT, " &
                      "DISCRIMINANTS HAVE DEFAULTS, AND COMPONENT" &
                      "SUBTYPE DETERMINES WHETHER CONSTRAINT SHOULD " &
                      "BE CHECKED");

-- CASE D1: COMPONENT IS PRESENT

     DECLARE
          TYPE CONS (D3 : INTEGER := IDENT_INT(1)) IS
               RECORD
                    CASE D3 IS
                         WHEN -5..10 =>
                              C1 : REC (D3, F1);       -- F1 EVALUATED
                         WHEN OTHERS =>
                              C2 : INTEGER := IDENT_INT(0);
                    END CASE;
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
          TYPE CONS (D3 : INTEGER := IDENT_INT(1)) IS
               RECORD
                    CASE D3 IS
                         WHEN -5..10 =>
                              C1 : REC(D3, F1);
                         WHEN OTHERS =>
                              C2 : INTEGER := IDENT_INT(0);
                    END CASE;
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
                    IF X.ALL /= (1, (1, 1)) THEN
                         COMMENT ("IRRELEVANT");
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 2A");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 2B");
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

-- CASE C2 : COMPONENT IS ABSENT

     F1_CONS := 2;

     DECLARE
          TYPE CONS (D3 : INTEGER := IDENT_INT(-6)) IS
               RECORD
                    CASE D3 IS
                         WHEN -5..10 =>
                              C1 : REC (D3, F1);       -- F1 EVALUATED
                         WHEN OTHERS =>
                              C2 : INTEGER := IDENT_INT(0);
                    END CASE;
               END RECORD;
          CHK1 : BOOLEAN := CHK (F1_CONS, 1, "F1 NOT EVALUATED - 2");
          X : CONS;             -- F1 NOT EVALUATED AGAIN
          Y : CONS;             -- F1 NOT EVALUATED AGAIN
          CHK2 : BOOLEAN := CHK (F1_CONS, 1, "F1 EVALUATED - 2");
     BEGIN
          IF X /= (-6, 0) OR Y /= (-6, 0) THEN
               FAILED ("DISCRIMINANT VALUES NOT CORRECT");
          END IF;
     END;

     F1_CONS := 12;

     DECLARE
          TYPE CONS (D3 : INTEGER := IDENT_INT(11)) IS
               RECORD
                    CASE D3 IS
                         WHEN -5..10 =>
                              C1 : REC(D3, F1);
                         WHEN OTHERS =>
                              C2 : INTEGER := IDENT_INT(0);
                    END CASE;
               END RECORD;
     BEGIN
          BEGIN
               DECLARE
                    X : CONS;
               BEGIN
                    IF X /= (11, 0) THEN
                         FAILED ("WRONG VALUE FOR X - 11");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 11");
          END;

          BEGIN
               DECLARE
                    SUBTYPE SCONS IS CONS;
               BEGIN
                    DECLARE
                         X : SCONS;
                    BEGIN
                         IF X /= (11, 0) THEN
                              FAILED ("X VALUE WRONG - 12");
                         END IF;
                    END;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 12");
          END;

          BEGIN
               DECLARE
                    TYPE ARR IS ARRAY (1..5) OF CONS;
                    X : ARR;
               BEGIN
                    IF X /= (1..5 => (11, 0)) THEN
                         FAILED ("X VALUE INCORRECT - 13");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 13");
          END;

          BEGIN
               DECLARE
                    TYPE NREC IS
                         RECORD
                              C1 : CONS;
                         END RECORD;
                    X : NREC;
               BEGIN
                    IF X /= (C1 => (11, 0)) THEN
                         FAILED ("X VALUE IS INCORRECT - 14");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 14");
          END;

          BEGIN
               DECLARE
                    TYPE NREC IS NEW CONS;
                    X : NREC;
               BEGIN
                    IF X /= (11, 0) THEN
                         FAILED ("X VALUE INCORRECT - 15");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 15");
          END;

          BEGIN
               DECLARE
                    TYPE ACC_CONS IS ACCESS CONS;
                    X : ACC_CONS := NEW CONS;
               BEGIN
                    IF X.ALL /= (11, 0) THEN
                         FAILED ("X VALUE INCORRECT - 17");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("NONEXISTENT CONSTRAINT CHECKED - 17");
          END;
     END;


     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ("CONSTRAINT CHECK DONE TOO EARLY");
          RESULT;

END C37213F;
