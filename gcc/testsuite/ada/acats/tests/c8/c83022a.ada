-- C83022A.ADA

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
--     CHECK THAT A DECLARATION IN A SUBPROGRAM FORMAL PART OR BODY
--     HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK THAT THE
--     OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH DECLARATIVE
--     REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH AND THE
--     OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAH DECLARATION.

-- HISTORY:
--     TBN 08/01/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83022A IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN
     TEST ("C83022A", "CHECK THAT A DECLARATION IN A SUBPROGRAM " &
                      "FORMAL PART OR BODY HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     ONE:
     DECLARE                      -- SUBPROGRAM DECLARATIVE REGION.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;

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

     BEGIN  -- ONE
          INNER (A);
          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 6");
          END IF;
     END ONE;

     TWO:
     DECLARE                     -- FORMAL PARAMETER OF SUBPROGRAM.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;
          OBJ : INTEGER := IDENT_INT(3);

          PROCEDURE INNER (X : IN INTEGER := A;
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

     BEGIN  -- TWO
          INNER (A => OBJ);
          IF OBJ /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 15");
          END IF;
     END TWO;

     THREE:
     DECLARE            -- AFTER THE SPECIFICATION OF SUBPROGRAM.
          A : INTEGER := IDENT_INT(2);

          FUNCTION INNER (X : INTEGER) RETURN INTEGER;

          B : INTEGER := A;

          FUNCTION INNER (X : INTEGER) RETURN INTEGER IS
               C : INTEGER := A;
               A : INTEGER := IDENT_INT(3);
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

     BEGIN  -- THREE
          IF INNER(A) /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 25");
          END IF;
     END THREE;

     FOUR:
     DECLARE                              -- RENAMING DECLARATION.
          A : INTEGER := IDENT_INT(2);

          PROCEDURE TEMPLATE (X : IN INTEGER := A;
                              Y : IN OUT INTEGER);

          PROCEDURE INNER (Z : IN INTEGER := A;
                           A : IN OUT INTEGER) RENAMES TEMPLATE;

          B : INTEGER := A;
          OBJ : INTEGER := 5;

          PROCEDURE TEMPLATE (X : IN INTEGER := A;
                              Y : IN OUT INTEGER) IS
          BEGIN  -- TEMPLATE
               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FOR VARIABLE - 30");
               END IF;
               IF Y /= IDENT_INT(5) THEN
                    FAILED ("INCORRECT RESULTS FOR VARIABLE - 31");
               END IF;
               Y := IDENT_INT(2 * X);
               IF FOUR.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FOR OUTER HOMOGRAPH - " &
                            "32");
               END IF;
          END TEMPLATE;

     BEGIN  -- FOUR
          IF B /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 32");
          END IF;
          INNER (A => OBJ);
          IF OBJ /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 33");
          END IF;
     END FOUR;

     FIVE:
     DECLARE                         -- GENERIC FORMAL SUBPROGRAM.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;

          PROCEDURE INNER (X : IN OUT INTEGER);

          GENERIC
               WITH PROCEDURE SUBPR (Y : IN OUT INTEGER) IS <>;
          PACKAGE P IS
               PAC_VAR : INTEGER := 1;
          END P;

          PROCEDURE INNER (X : IN OUT INTEGER) IS
               C : INTEGER := A;
               A : INTEGER := IDENT_INT(3);
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH - 41");
               END IF;
               IF FIVE.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 42");
               END IF;
               IF FIVE.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 43");
               END IF;
               IF C /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 44");
               END IF;
               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 45");
               END IF;
               IF EQUAL(1,1) THEN
                    X := A;
               ELSE
                    X := FIVE.A;
               END IF;
          END INNER;

          PACKAGE BODY P IS
          BEGIN
               SUBPR (A);
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE PASSED OUT - 46");
               END IF;
               IF PAC_VAR /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT VALUE FOR PAC_VAR - 47");
               END IF;
          END P;

          PACKAGE NEW_P IS NEW P (INNER);

     BEGIN  -- FIVE
          NULL;
     END FIVE;

     SIX:
     DECLARE                              -- GENERIC INSTANTIATION.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;
          OBJ : INTEGER := IDENT_INT(3);

          GENERIC
          PROCEDURE INNER (X : IN INTEGER := A;
                           A : IN OUT INTEGER);

          PROCEDURE INNER (X : IN INTEGER := SIX.A;
                           A : IN OUT INTEGER) IS
               C : INTEGER := A;
          BEGIN
               IF A /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH -50");
               END IF;
               IF SIX.A /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH - 51");
               END IF;
               IF SIX.B /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 52");
               END IF;
               IF C /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE FOR INNER VARIABLE - 53");
               END IF;
               IF X /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE PASSED IN - 54");
               END IF;
               IF EQUAL(1,1) THEN
                    A := IDENT_INT(4);
               ELSE
                    A := 1;
               END IF;
          END INNER;

          PROCEDURE SUBPR IS NEW INNER;

     BEGIN  -- SIX
          SUBPR (A => OBJ);
          IF OBJ /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 55");
          END IF;
     END SIX;

     SEVEN:
     DECLARE                 --  OVERLOADING OF FUNCTIONS.

          OBJ : INTEGER := 1;
          FLO : FLOAT := 5.0;

          FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

          PROCEDURE INNER (X : IN OUT INTEGER; F : IN FLOAT);

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          PROCEDURE INNER (X : IN OUT INTEGER; F : IN FLOAT) IS
          BEGIN
               X := INTEGER(F);
          END INNER;

     BEGIN
          FLO := 6.25;
          INNER (OBJ, FLO);
          IF OBJ /= IDENT_INT(6) THEN
               FAILED ("INCORRECT VALUE RETURNED FROM FUNCTION - 60");
          END IF;
     END SEVEN;


     RESULT;
END C83022A;
