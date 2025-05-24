-- C37213H.ADA

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
--     CHECK, WHERE AN INDEX CONSTRAINT DEPENDS ON A RECORD
--     DISCRIMINANT WITH A DEFAULT VALUE AND THE RECORD TYPE IS NOT
--     EXPLICITLY CONSTRAINED, THAT THE NON-DISCRIMINANT EXPRESSIONS
--     IN THE INDEX CONSTRAINT ARE:
--          1) EVALUATED WHEN THE RECORD COMPONENT SUBTYPE DEFINITION
--             IS ELABORATED,
--          2) PROPERLY CHECKED FOR COMPATIBILITY ONLY IN AN ALLOCATION
--             OR OBJECT DECLARATION AND ONLY IF THE DISCRIMINANT-
--             DEPENDENT COMPONENT IS PRESENT IN THE SUBTYPE.

-- HISTORY:
--     JBG  10/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER; MODIFIED THE CHECK OF
--                    SUBTYPE 'SCONS', IN BOTH SUBPARTS OF THE TEST,
--                    TO INDICATE FAILURE IF CONSTRAINT_ERROR IS RAISED
--                    FOR THE SUBTYPE DECLARATION AND FAILURE IF
--                    CONSTRAINT_ERROR IS NOT RAISED FOR AN OBJECT
--                    DECLARATION OF THIS SUBTYPE; RELOCATED THE CALL TO
--                    REPORT.TEST SO THAT IT COMES BEFORE ANY
--                    DECLARATIONS;  ADDED 'SEQUENCE_NUMBER' TO IDENTIFY
--                    THE CURRENT SUBTEST (FOR EXCEPTIONS); CHANGE THE
--                    TYPE OF THE DISCRIMINANT IN THE RECORD 'CONS'
--                    TO AN INTEGER SUBTYPE.
--     VCL  03/30/88  MODIFIED HEADER AND MESSAGES OUTPUT BY REPORT
--                    PACKAGE.

WITH REPORT; USE REPORT;
PROCEDURE C37213H IS
BEGIN
     TEST ("C37213H", "THE NON-DISCRIMINANT EXPRESSIONS OF AN " &
                      "INDEX CONSTRAINT THAT DEPEND ON A " &
                      "DISCRIMINANT WITH A DEFAULT VALUE ARE " &
                      "PROPERLY EVALUATED AND CHECKED WHEN THE " &
                      "RECORD TYPE IS NOT EXPLICITLY CONSTRAINED AND " &
                      "THE COMPONENT IS AND IS NOT PRESENT IN THE " &
                      "SUBTYPE");

     DECLARE
          SEQUENCE_NUMBER : INTEGER;

          SUBTYPE DISCR IS INTEGER RANGE -50..50;
          SUBTYPE SM IS INTEGER RANGE 1..10;
          TYPE MY_ARR IS ARRAY (SM RANGE <>) OF INTEGER;

          F1_CONS : INTEGER := 2;

          FUNCTION CHK (
               CONS    : INTEGER;
               VALUE   : INTEGER;
               MESSAGE : STRING) RETURN BOOLEAN IS
          BEGIN
               IF CONS /= VALUE THEN
                    FAILED (MESSAGE & ": F1_CONS IS " &
                            INTEGER'IMAGE(F1_CONS));
               END IF;
               RETURN TRUE;
          END CHK;

          FUNCTION F1 RETURN INTEGER IS
          BEGIN
               F1_CONS := F1_CONS - IDENT_INT(1);
               RETURN F1_CONS;
          END F1;
     BEGIN


-- CASE 1: DISCRIMINANT-DEPENDENT COMPONENT IS PRESENT.

          SEQUENCE_NUMBER :=1;
          DECLARE
               TYPE CONS (D3 : DISCR := IDENT_INT(1)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(F1..D3); -- F1 EVALUATED.
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(0);
                         END CASE;
                    END RECORD;

               CHK1 : BOOLEAN := CHK (F1_CONS, 1, "F1 NOT EVALUATED");

               X : CONS;                     -- F1 NOT EVALUATED AGAIN.
               Y : CONS;                     -- F1 NOT EVALUATED AGAIN.

               CHK2 : BOOLEAN := CHK (F1_CONS, 1, "F1 EVALUATED");
          BEGIN
               IF X.C1'FIRST /= 1 OR Y.C1'LAST /= 1 THEN
                    FAILED ("VALUES NOT CORRECT");
               END IF;
          END;


          F1_CONS := 12;

          SEQUENCE_NUMBER := 2;
          DECLARE
               TYPE CONS (D3 : DISCR := IDENT_INT(1)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..F1);
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(0);
                         END CASE;
                    END RECORD;
          BEGIN
               BEGIN
                    DECLARE
                         X : CONS;
                    BEGIN
                         FAILED ("INDEX CHECK NOT PERFORMED - 1");
                         IF X /= (1, (1, 1)) THEN
                              COMMENT ("INCORRECT VALUES FOR X - 1");
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
               END;

               BEGIN
                    DECLARE
                         SUBTYPE SCONS IS CONS;
                    BEGIN
                         DECLARE
                              X : SCONS;
                         BEGIN
                              FAILED ("INDEX CHECK NOT PERFORMED - 2");
                              IF X /= (1, (1, 1)) THEN
                                   COMMENT ("INCORRECT VALUES FOR X " &
                                            "- 2");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED " &
                                      "- 2A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 2B");
               END;

               BEGIN
                    DECLARE
                         TYPE ARR IS ARRAY (1..5) OF CONS;
                    BEGIN
                         DECLARE
                              X : ARR;
                         BEGIN
                              FAILED ("INDEX CHECK NOT PERFORMED - 3");
                              IF X /= (1..5 => (1, (1, 1))) THEN
                                   COMMENT ("INCORRECT VALUES FOR X " &
                                            "- 3");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED " &
                                      "- 3A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 3B");
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
                              FAILED ("INDEX CHECK NOT PERFORMED - 4");
                              IF X /= (C1 => (1, (1, 1))) THEN
                                   COMMENT ("INCORRECT VALUES FOR X " &
                                            "- 4");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED " &
                                      "- 4A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 4B");
               END;

               BEGIN
                    DECLARE
                         TYPE NREC IS NEW CONS;
                    BEGIN
                         DECLARE
                              X : NREC;
                         BEGIN
                              FAILED ("INDEX CHECK NOT PERFORMED - 5");
                              IF X /= (1, (1, 1)) THEN
                                   COMMENT ("INCORRECT VALUES FOR X " &
                                            "- 5");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED " &
                                       "- 5A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 5B");
               END;

               BEGIN
                    DECLARE
                         TYPE ACC_CONS IS ACCESS CONS;
                    BEGIN
                         DECLARE
                              X : ACC_CONS;
                         BEGIN
                              X := NEW CONS;
                              FAILED ("INDEX CHECK NOT PERFORMED - 6");
                              IF X.ALL /= (1, (1, 1)) THEN
                                   COMMENT ("INCORRECT VALUES FOR X " &
                                            "- 6");
                              END IF;
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   COMMENT ("UNEXPECTED EXCEPTION " &
                                            "RAISED - 6A");
                         END;
                    EXCEPTION
                         WHEN OTHERS =>
                              COMMENT ("UNEXPECTED EXCEPTION RAISED " &
                                       "- 6B");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 6C");
               END;
          END;


-- CASE D2: DISCRIMINANT-DEPENDENT COMPONENT IS ABSENT.

          F1_CONS := 2;

          SEQUENCE_NUMBER := 3;
          DECLARE
               TYPE CONS (D3 : DISCR := IDENT_INT(-6)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..F1); -- F1 EVALUATED.
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(0);
                         END CASE;
                    END RECORD;
               CHK1 : BOOLEAN := CHK (F1_CONS, 1, "F1 NOT EVALUATED");

               X : CONS;                      -- F1 NOT EVALUATED AGAIN.
               Y : CONS;                      -- F1 NOT EVALUATED AGAIN.

               CHK2 : BOOLEAN := CHK (F1_CONS, 1, "F1 EVALUATED");
          BEGIN
               IF X /= (-6, 0) OR Y /= (-6, 0) THEN
                    FAILED ("VALUES NOT CORRECT");
               END IF;
          END;

          F1_CONS := 12;

          SEQUENCE_NUMBER := 4;
          DECLARE
               TYPE CONS (D3 : DISCR := IDENT_INT(11)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..F1);
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
                              FAILED ("X VALUE IS INCORRECT - 11");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 11");
               END;

               BEGIN
                    DECLARE
                         SUBTYPE SCONS IS CONS;
                    BEGIN
                         DECLARE
                              X : SCONS;
                         BEGIN
                              IF X /= (11, 0) THEN
                                   FAILED ("X VALUE INCORRECT - 12");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED - " &
                                      "12A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 12B");
               END;

               BEGIN
                    DECLARE
                         TYPE ARR IS ARRAY (1..5) OF CONS;
                    BEGIN
                         DECLARE
                              X : ARR;
                         BEGIN
                              IF X /= (1..5 => (11, 0)) THEN
                                   FAILED ("X VALUE INCORRECT - 13");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED - " &
                                      "13A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 13B");
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
                              IF X /= (C1 => (11, 0)) THEN
                                   FAILED ("X VALUE INCORRECT - 14");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED - " &
                                      "14A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 14B");
               END;

               BEGIN
                    DECLARE
                         TYPE NREC IS NEW CONS;
                    BEGIN
                         DECLARE
                              X : NREC;
                         BEGIN
                              IF X /= (11, 0) THEN
                                   FAILED ("X VALUE INCORRECT - 15");
                              END IF;
                         END;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED - " &
                                      "15A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 15B");
               END;

               BEGIN
                    DECLARE
                         TYPE ACC_CONS IS ACCESS CONS;
                         X : ACC_CONS;
                    BEGIN
                         X := NEW CONS;
                         IF X.ALL /= (11, 0) THEN
                              FAILED ("X VALUE INCORRECT - 17");
                         END IF;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("UNEXPECTED EXCEPTION RAISED - " &
                                      "17A");
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                          FAILED ("UNEXPECTED EXCEPTION RAISED - 17B");
               END;
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("INDEX VALUES IMPROPERLY CHECKED - " &
                       INTEGER'IMAGE (SEQUENCE_NUMBER));
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED " &
                       INTEGER'IMAGE (SEQUENCE_NUMBER));
     END;

     RESULT;
END C37213H;
