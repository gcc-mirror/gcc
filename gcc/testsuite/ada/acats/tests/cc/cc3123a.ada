-- CC3123A.ADA

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
-- CHECK THAT DEFAULT EXPRESSIONS FOR GENERIC IN PARAMETERS ARE ONLY
-- EVALUATED IF THERE ARE NO ACTUAL PARAMETERS.

-- TBN  12/01/86

WITH REPORT; USE REPORT;
PROCEDURE CC3123A IS

BEGIN
     TEST ("CC3123A", "CHECK THAT DEFAULT EXPRESSIONS FOR GENERIC IN " &
                      "PARAMETERS ARE ONLY EVALUATED IF THERE ARE " &
                      "NO ACTUAL PARAMETERS");
     DECLARE
          TYPE ENUM IS (I, II, III);
          OBJ_INT : INTEGER := 1;
          OBJ_ENUM : ENUM := I;

          GENERIC
               GEN_INT : IN INTEGER := IDENT_INT(2);
               GEN_BOOL : IN BOOLEAN := IDENT_BOOL(FALSE);
               GEN_ENUM : IN ENUM := II;
          PACKAGE P IS
               PAC_INT : INTEGER := GEN_INT;
               PAC_BOOL : BOOLEAN := GEN_BOOL;
               PAC_ENUM : ENUM := GEN_ENUM;
          END P;

          PACKAGE P1 IS NEW P;
          PACKAGE P2 IS
               NEW P (IDENT_INT(OBJ_INT), GEN_ENUM => OBJ_ENUM);
          PACKAGE P3 IS NEW P (GEN_BOOL => IDENT_BOOL(TRUE));
     BEGIN
          IF P1.PAC_INT /= 2 OR P1.PAC_BOOL OR P1.PAC_ENUM /= II THEN
               FAILED ("DEFAULT VALUES WERE NOT EVALUATED");
          END IF;
          IF P2.PAC_INT /= 1 OR P2.PAC_BOOL OR P2.PAC_ENUM /= I THEN
               FAILED ("DEFAULT VALUES WERE NOT EVALUATED CORRECTLY " &
                       "- 1");
          END IF;
          IF P3.PAC_INT /= 2 OR NOT(P3.PAC_BOOL) OR
               P3.PAC_ENUM /= II THEN
               FAILED ("DEFAULT VALUES WERE NOT EVALUATED CORRECTLY " &
                       "- 2");
          END IF;
     END;

     -------------------------------------------------------------------
     DECLARE
          OBJ_INT1 : INTEGER := 3;

          FUNCTION FUNC (X : INTEGER) RETURN INTEGER;

          GENERIC
               GEN_INT1 : IN INTEGER := FUNC (1);
               GEN_INT2 : IN INTEGER := FUNC (GEN_INT1 + 1);
          PROCEDURE PROC;

          PROCEDURE PROC IS
               PROC_INT1 : INTEGER := GEN_INT1;
               PROC_INT2 : INTEGER := GEN_INT2;
          BEGIN
               IF PROC_INT1 /= 3 THEN
                    FAILED ("DEFAULT VALUES WERE NOT EVALUATED " &
                            "CORRECTLY - 3");
               END IF;
               IF PROC_INT2 /= 4 THEN
                    FAILED ("DEFAULT VALUES WERE NOT EVALUATED " &
                            "CORRECTLY - 4");
               END IF;
          END PROC;

          FUNCTION FUNC (X : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= IDENT_INT(4) THEN
                    FAILED ("DEFAULT VALUES WERE NOT EVALUATED " &
                            "CORRECTLY - 5");
               END IF;
               RETURN IDENT_INT(X);
          END FUNC;

          PROCEDURE NEW_PROC IS NEW PROC (GEN_INT1 => OBJ_INT1);

     BEGIN
          NEW_PROC;
     END;

     -------------------------------------------------------------------
     DECLARE
          TYPE ARA_TYP IS ARRAY (1 .. 2) OF INTEGER;
          TYPE REC IS
               RECORD
                    ANS : BOOLEAN;
                    ARA : ARA_TYP;
               END RECORD;
          TYPE ARA_REC IS ARRAY (1 .. 5) OF REC;

          FUNCTION F (X : INTEGER) RETURN INTEGER;

          OBJ_REC : REC := (FALSE, (3, 4));
          OBJ_ARA : ARA_REC := (1 .. 5 => (FALSE, (3, 4)));

          GENERIC
               GEN_OBJ1 : IN ARA_TYP := (F(1), 2);
               GEN_OBJ2 : IN REC := (TRUE, GEN_OBJ1);
               GEN_OBJ3 : IN ARA_REC := (1 .. F(5) => (TRUE, (1, 2)));
          FUNCTION FUNC RETURN INTEGER;

          FUNCTION FUNC RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT(1);
          END FUNC;

          FUNCTION F (X : INTEGER) RETURN INTEGER IS
          BEGIN
               FAILED ("DEFAULT VALUES WERE EVALUATED - 1");
               RETURN IDENT_INT(X);
          END F;

          FUNCTION NEW_FUNC IS NEW FUNC ((3, 4), OBJ_REC, OBJ_ARA);

     BEGIN
          IF NOT EQUAL (NEW_FUNC, 1) THEN
               FAILED ("INCORRECT RESULT FROM GENERIC FUNCTION - 1");
          END IF;
     END;

     -------------------------------------------------------------------
     DECLARE
          SUBTYPE INT IS INTEGER RANGE 1 .. 5;
          TYPE ARA_TYP IS ARRAY (1 .. 2) OF INTEGER;
          TYPE COLOR IS (RED, WHITE);
          TYPE CON_REC (D : INT) IS
               RECORD
                    A : COLOR;
                    B : ARA_TYP;
               END RECORD;
          TYPE UNCON_OR_CON_REC (D : INT := 2) IS
               RECORD
                    A : COLOR;
                    B : ARA_TYP;
               END RECORD;
          FUNCTION F (X : COLOR) RETURN COLOR;

          OBJ_CON1 : CON_REC (1) := (1, WHITE, (3, 4));
          OBJ_UNCON : UNCON_OR_CON_REC := (2, WHITE, (3, 4));
          OBJ_CON2 : UNCON_OR_CON_REC (3) := (3, WHITE, (3, 4));

          GENERIC
               GEN_CON1 : IN CON_REC := (2, F(RED), (1, 2));
               GEN_UNCON : IN UNCON_OR_CON_REC := (2, F(RED), (1, 2));
               GEN_CON2 : IN UNCON_OR_CON_REC := GEN_UNCON;
          FUNCTION FUNC RETURN INTEGER;

          FUNCTION FUNC RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT(1);
          END FUNC;

          FUNCTION F (X : COLOR) RETURN COLOR IS
          BEGIN
               FAILED ("DEFAULT VALUES WERE EVALUATED - 2");
               RETURN WHITE;
          END F;

          FUNCTION NEW_FUNC IS NEW FUNC (OBJ_CON1, OBJ_UNCON, OBJ_CON2);

     BEGIN
          IF NOT EQUAL (NEW_FUNC, 1) THEN
               FAILED ("INCORRECT RESULT FROM GENERIC FUNCTION - 2");
          END IF;
     END;

     RESULT;
END CC3123A;
