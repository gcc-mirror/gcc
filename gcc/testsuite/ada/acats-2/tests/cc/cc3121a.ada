-- CC3121A.ADA

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
-- CHECK THAT AN UNCONSTRAINED FORMAL GENERIC PARAMETER OF MODE "IN"
-- HAVING AN ARRAY TYPE OR A TYPE WITH DISCRIMINANTS HAS THE CONSTRAINTS
-- OF THE ACTUAL PARAMETER.

-- TBN  9/29/86

WITH REPORT; USE REPORT;
PROCEDURE CC3121A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;

     TYPE ARRAY1 IS ARRAY (INT RANGE <>) OF INTEGER;

     TYPE REC1 (D : INT) IS
          RECORD
               VAR1 : INTEGER := 1;
          END RECORD;

     TYPE REC2 (D : INT := 2) IS
          RECORD
               A : ARRAY1 (D .. IDENT_INT(4));
               B : REC1 (D);
               C : INTEGER := 1;
          END RECORD;

     TYPE ARRAY2 IS ARRAY (INT RANGE <>) OF REC2;

BEGIN
     TEST ("CC3121A", "CHECK THAT AN UNCONSTRAINED FORMAL GENERIC " &
                      "PARAMETER OF MODE 'IN' HAVING AN ARRAY TYPE " &
                      "OR A TYPE WITH DISCRIMINANTS HAS THE " &
                      "CONSTRAINTS OF THE ACTUAL PARAMETER");

     DECLARE
          OBJ_ARA1 : ARRAY1 (IDENT_INT(2) .. 5);

          GENERIC
               VAR : ARRAY1;
          PROCEDURE PROC;

          PROCEDURE PROC IS
          BEGIN
               IF VAR'FIRST /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FOR VAR'FIRST");
               END IF;
               IF VAR'LAST /= IDENT_INT(5) THEN
                    FAILED ("INCORRECT RESULTS FOR VAR'LAST");
               END IF;
          END PROC;

          PROCEDURE PROC1 IS NEW PROC (OBJ_ARA1);
     BEGIN
          PROC1;
     END;

     -------------------------------------------------------------------
     DECLARE
          OBJ_REC2 : REC2;

          GENERIC
               VAR : REC2;
          FUNCTION FUNC RETURN INTEGER;

          FUNCTION FUNC RETURN INTEGER IS
          BEGIN
               IF VAR.D /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.D");
               END IF;
               IF VAR.A'FIRST /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.A'FIRST");
               END IF;
               IF VAR.A'LAST /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.A'LAST");
               END IF;
               IF VAR.B.D /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.B.D");
               END IF;
               RETURN IDENT_INT(1);
          END FUNC;

          FUNCTION FUNC1 IS NEW FUNC (OBJ_REC2);

     BEGIN
          IF FUNC1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT RESULTS FROM FUNC1 CALL");
          END IF;
     END;

     -------------------------------------------------------------------
     DECLARE
          OBJ_ARA2 : ARRAY2 (IDENT_INT(6) .. 8);

          GENERIC
               VAR : ARRAY2;
          PROCEDURE PROC;

          PROCEDURE PROC IS
          BEGIN
               IF VAR'FIRST /= IDENT_INT(6) THEN
                    FAILED ("INCORRECT RESULTS FOR VAR'FIRST");
               END IF;
               IF VAR'LAST /= IDENT_INT(8) THEN
                    FAILED ("INCORRECT RESULTS FOR VAR'LAST");
               END IF;
               IF VAR(6).D /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR(6).D");
               END IF;
               IF VAR(6).A'FIRST /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR(6).A'FIRST");
               END IF;
               IF VAR(6).A'LAST /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR(6).A'LAST");
               END IF;
               IF VAR(6).B.D /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR(6).B.D");
               END IF;
          END PROC;

          PROCEDURE PROC2 IS NEW PROC (OBJ_ARA2);
     BEGIN
          PROC2;
     END;

     -------------------------------------------------------------------
     DECLARE
          OBJ_REC3 : REC2 (3);

          GENERIC
               VAR : REC2;
          PACKAGE PAC IS
               PAC_VAR : INTEGER := 1;
          END PAC;

          PACKAGE BODY PAC IS
          BEGIN
               IF VAR.D /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.D");
               END IF;
               IF VAR.A'FIRST /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.A'FIRST");
               END IF;
               IF VAR.A'LAST /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.A'LAST");
               END IF;
               IF VAR.B.D /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RESULTS FROM VAR.B.D");
               END IF;
          END PAC;

          PACKAGE PAC1 IS NEW PAC (OBJ_REC3);

     BEGIN
          NULL;
     END;

     -------------------------------------------------------------------

     RESULT;
END CC3121A;
