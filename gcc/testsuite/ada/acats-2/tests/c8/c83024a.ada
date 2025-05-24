-- C83024A.ADA

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
--     CHECK THAT A DECLARATION IN A DECLARATIVE REGION FOR A GENERIC
--     PACKAGE HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK
--     THAT THE OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH
--     DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH
--     AND THE OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAH DECLARATION.

-- HISTORY:
--     BCB 08/30/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C83024A IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN
     TEST ("C83024A", "CHECK THAT A DECLARATION IN A DECLARATIVE " &
                      "REGION FOR A GENERIC PACKAGE HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     ONE:
     DECLARE
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;
          OBJ : INTEGER := IDENT_INT(3);

          GENERIC
               X : IN INTEGER := A;
               A : IN OUT INTEGER;
          PACKAGE INNER IS
               C : INTEGER := A;
          END INNER;

          PACKAGE BODY INNER IS
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 10");
               END IF;

               IF ONE.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
               END IF;

               IF ONE.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 12");
               END IF;

               IF C /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 13");
               END IF;

               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 14");
               END IF;

               IF EQUAL(1,1) THEN
                    A := IDENT_INT(4);
               ELSE
                    A := 1;
               END IF;
          END INNER;

          PACKAGE NEW_INNER IS NEW INNER (A => OBJ);

     BEGIN  -- ONE
          IF OBJ /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 15");
          END IF;
     END ONE;

     TWO:
     DECLARE            -- AFTER THE SPECIFICATION OF PACKAGE.
          A : INTEGER := IDENT_INT(2);

          GENERIC
               X : IN OUT INTEGER;
          PACKAGE INNER IS
               A : INTEGER := IDENT_INT(3);
          END INNER;

          B : INTEGER := A;

          PACKAGE BODY INNER IS
               C : INTEGER := TWO.A;
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
               END IF;

               IF TWO.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
               END IF;

               IF TWO.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
               END IF;

               IF C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 23");
               END IF;

               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 24");
               END IF;

               IF EQUAL(1,1) THEN
                    X := A;
               ELSE
                    NULL;
               END IF;
          END INNER;

          PACKAGE NEW_INNER IS NEW INNER (A);

     BEGIN  -- TWO
          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 25");
          END IF;
     END TWO;

     THREE:
     DECLARE                 --  OVERLOADING OF FUNCTIONS.

          OBJ : INTEGER := 1;
          FLO : FLOAT := 6.25;

          FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

          GENERIC
               X : IN OUT INTEGER;
               F : IN FLOAT;
          PACKAGE INNER IS
          END INNER;

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          PACKAGE BODY INNER IS
          BEGIN
               X := INTEGER(F);
          END INNER;

          PACKAGE NEW_INNER IS NEW INNER (OBJ, FLO);

     BEGIN
          IF OBJ /= IDENT_INT(6) THEN
               FAILED ("INCORRECT VALUE RETURNED FROM FUNCTION - 60");
          END IF;
     END THREE;

     RESULT;
END C83024A;
