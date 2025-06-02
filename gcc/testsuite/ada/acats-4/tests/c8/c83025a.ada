-- C83025A.ADA

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
--     CHECK THAT A DECLARATION IN THE DECLARATIVE REGION OF A GENERIC
--     SUBPROGRAM HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK
--     THAT THE OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH
--     DECLARATIVE REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH
--     AND THE OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 08/31/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C83025A IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN
     TEST ("C83025A", "CHECK THAT A DECLARATION IN THE DECLARATIVE " &
                      "REGION OF A GENERIC SUBPROGRAM HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     ONE:
     DECLARE                      -- SUBPROGRAM DECLARATIVE REGION.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;

          GENERIC
          PROCEDURE INNER (X : IN OUT INTEGER);

          PROCEDURE INNER (X : IN OUT INTEGER) IS
               C : INTEGER := A;
               A : INTEGER := IDENT_INT(3);
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 1");
               END IF;

               IF ONE.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 2");
               END IF;

               IF ONE.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 3");
               END IF;

               IF C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 4");
               END IF;

               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 5");
               END IF;

               IF EQUAL(1,1) THEN
                    X := A;
               ELSE
                    X := ONE.A;
               END IF;
          END INNER;

          PROCEDURE NEW_INNER IS NEW INNER;

     BEGIN  -- ONE
          NEW_INNER (A);

          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 6");
          END IF;
     END ONE;

     TWO:
     DECLARE               -- FORMAL PARAMETER OF GENERIC SUBPROGRAM.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;
          OBJ : INTEGER := IDENT_INT(3);

          GENERIC
          PROCEDURE INNER (X : IN INTEGER := A;
                           A : IN OUT INTEGER);

          PROCEDURE INNER (X : IN INTEGER := TWO.A;
                           A : IN OUT INTEGER) IS
               C : INTEGER := A;
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH -10");
               END IF;

               IF TWO.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 11");
               END IF;

               IF TWO.B /= IDENT_INT(2) THEN
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

          PROCEDURE NEW_INNER IS NEW INNER;

     BEGIN  -- TWO
          NEW_INNER (A => OBJ);

          IF OBJ /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 15");
          END IF;
     END TWO;

     THREE:
     DECLARE      -- AFTER THE SPECIFICATION OF GENERIC SUBPROGRAM.
          GENERIC
               A : INTEGER := IDENT_INT(3);
          FUNCTION INNER (X : INTEGER) RETURN INTEGER;

          A : INTEGER := IDENT_INT(2);

          B : INTEGER := A;

          FUNCTION INNER (X : INTEGER) RETURN INTEGER IS
               C : INTEGER := THREE.A;
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 20");
               END IF;

               IF THREE.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 21");
               END IF;

               IF THREE.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 22");
               END IF;

               IF C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 23");
               END IF;

               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 24");
               END IF;

               IF EQUAL(1,1) THEN
                    RETURN A;
               ELSE
                    RETURN X;
               END IF;
          END INNER;

          FUNCTION NEW_INNER IS NEW INNER;

     BEGIN  -- THREE
          IF NEW_INNER(A) /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 25");
          END IF;
     END THREE;

     FOUR:
     DECLARE
          A : INTEGER := IDENT_INT(2);

          GENERIC
               A : INTEGER;
               B : INTEGER := A;
          PROCEDURE INNER (X : IN OUT INTEGER);

          PROCEDURE INNER (X : IN OUT INTEGER) IS
               C : INTEGER := FOUR.A;
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 30");
               END IF;

               IF B /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 31");
               END IF;

               IF FOUR.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 32");
               END IF;

               IF C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 33");
               END IF;

               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 34");
               END IF;

               IF EQUAL(1,1) THEN
                    X := A;
               ELSE
                    X := FOUR.A;
               END IF;
          END INNER;

          PROCEDURE NEW_INNER IS NEW INNER (A => IDENT_INT(3));

     BEGIN
          NEW_INNER (A);

          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 35");
          END IF;
     END FOUR;

     FIVE:
     DECLARE                 --  OVERLOADING OF FUNCTIONS.

          OBJ : INTEGER := 1;
          FLO : FLOAT := 5.0;

          FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

          GENERIC
          PROCEDURE INNER (X : IN OUT INTEGER; F : IN FLOAT);

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          PROCEDURE INNER (X : IN OUT INTEGER; F : IN FLOAT) IS
          BEGIN
               X := INTEGER(F);
          END INNER;

          PROCEDURE NEW_INNER IS NEW INNER;

     BEGIN  -- FIVE
          FLO := 6.25;

          NEW_INNER (OBJ, FLO);

          IF OBJ /= IDENT_INT(6) THEN
               FAILED ("INCORRECT VALUE RETURNED FROM FUNCTION - 40");
          END IF;
     END FIVE;

     RESULT;
END C83025A;
