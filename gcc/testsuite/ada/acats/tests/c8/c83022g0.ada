-- C83022G0M.ADA

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
--     HOMOGRAPH DECLARATION, IF THE SUBPROGRAM BODY IS COMPILED
--     SEPARATELY AS A SUBUNIT.

-- SEPARATE FILES ARE:
--     C83022G0M.ADA - (THIS FILE) MAIN PROGRAM.
--     C83022G1.ADA -- SUBPROGRAM BODIES.

-- HISTORY:
--     BCB 08/26/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C83022G0M IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     A : INTEGER := IDENT_INT(2);
     B : INTEGER := A;

     OBJ : INTEGER := IDENT_INT(3);

     FLO : FLOAT := 5.0;

     PROCEDURE TEMPLATE (X : IN INTEGER := A;
                         Y : IN OUT INTEGER);

     PROCEDURE INNER4 (Z : IN INTEGER := A;
                       A : IN OUT INTEGER) RENAMES TEMPLATE;

     PROCEDURE INNER (X : IN OUT INTEGER) IS SEPARATE;

     PROCEDURE INNER2 (X : IN INTEGER := A;
                       A : IN OUT INTEGER) IS SEPARATE;

     FUNCTION INNER3 (X : INTEGER) RETURN INTEGER IS SEPARATE;

     PROCEDURE TEMPLATE (X : IN INTEGER := A;
                         Y : IN OUT INTEGER) IS SEPARATE;

     PROCEDURE INNER5 (X : IN OUT INTEGER) IS SEPARATE;

     GENERIC
          WITH PROCEDURE SUBPR (Y : IN OUT INTEGER) IS <>;
     PACKAGE P IS
          PAC_VAR : INTEGER := 1;
     END P;

     PACKAGE BODY P IS
     BEGIN
          SUBPR (A);

          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 1");
          END IF;

          IF PAC_VAR /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE FOR PAC_VAR - 2");
          END IF;
     END P;

     PACKAGE NEW_P IS NEW P (INNER5);

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

     FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

     PROCEDURE INNER6 (X : IN OUT INTEGER; F : IN FLOAT);

     FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

     PROCEDURE INNER6 (X : IN OUT INTEGER; F : IN FLOAT) IS SEPARATE;

BEGIN
     TEST ("C83022G", "CHECK THAT A DECLARATION IN A SUBPROGRAM " &
                      "FORMAL PART OR BODY HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     A := IDENT_INT(2);
     B := A;

     INNER (A);

     IF A /= IDENT_INT(3) THEN
          FAILED ("INCORRECT VALUE PASSED OUT - 3");
     END IF;

     A := IDENT_INT(2);

     INNER2 (A => OBJ);

     IF OBJ /= IDENT_INT(4) THEN
          FAILED ("INCORRECT VALUE PASSED OUT - 4");
     END IF;

     A := IDENT_INT(2);

     B := A;

     IF INNER3(A) /= IDENT_INT(3) THEN
          FAILED ("INCORRECT VALUE PASSED OUT - 5");
     END IF;

     A := IDENT_INT(2);

     B := A;
     OBJ := 5;

     IF B /= IDENT_INT(2) THEN
          FAILED ("INCORRECT VALUE FOR OUTER VARIABLE - 6");
     END IF;

     INNER4 (A => OBJ);

     IF OBJ /= IDENT_INT(4) THEN
          FAILED ("INCORRECT VALUE PASSED OUT - 7");
     END IF;

     OBJ := 1;

     FLO := 6.25;

     INNER6 (OBJ, FLO);

     IF OBJ /= IDENT_INT(6) THEN
          FAILED ("INCORRECT VALUE RETURNED FROM FUNCTION - 8");
     END IF;

     RESULT;
END C83022G0M;
