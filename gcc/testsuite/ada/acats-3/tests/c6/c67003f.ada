-- C67003F.ADA

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
-- CHECK THAT THE PREDEFINED OPERATORS FOR THE PREDEFINED TYPES CAN BE 
--   REDEFINED.
-- CHECK THAT THE REDEFINED OPERATOR IS INVOKED WHEN INFIX OR PREFIX
--   NOTATION IS USED.

-- HISTORY:
--   WMC 03/21/92   TEST CREATED FROM CONSOLIDATION OF C67003[A-E].ADA


WITH REPORT;

PROCEDURE C67003F IS

     USE REPORT;

BEGIN

     TEST ("C67003F", "CHECK THAT REDEFINITION OF " &
           "OPERATORS FOR PREDEFINED TYPES WORKS");

     DECLARE     -- INTEGER OPERATORS.

          -- INTEGER INFIX OPERATORS.

          FUNCTION "*" (X, Y : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= Y THEN
                    RETURN 1;
               ELSE RETURN 0;
               END IF;
          END "*";

          FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= Y THEN
                    RETURN 2;
               ELSE RETURN 0;
               END IF;
          END "+";
     
          FUNCTION "REM" (X, Y : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= Y THEN
                    RETURN 3;
               ELSE RETURN 0;
               END IF;
          END "REM";
     
          -- INTEGER PREFIX OPERATORS.

          FUNCTION "+" (X : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= 0 THEN
                    RETURN 4;
               ELSE RETURN 0;
               END IF;
          END "+";

          FUNCTION "ABS" (X : INTEGER) RETURN INTEGER IS
          BEGIN
               IF X /= 0 THEN
                    RETURN 5;
               ELSE RETURN 0;
               END IF;
          END "ABS";

          -- INTEGER RELATIONAL OPERATOR.

          FUNCTION "<" (X, Y : INTEGER) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "<";

     BEGIN

          IF IDENT_INT (3) * IDENT_INT (5) /= 1 THEN
               FAILED ("REDEFINITION OF INTEGER ""*"" IS DEFECTIVE");
          END IF;

          IF IDENT_INT (1) + IDENT_INT (30) /= 2 THEN
               FAILED ("REDEFINITION OF INTEGER ""+"" IS DEFECTIVE");
          END IF;

          IF IDENT_INT (7) REM IDENT_INT (8) /= 3 THEN
               FAILED ("REDEFINITION OF ""REM"" IS DEFECTIVE");
          END IF;

          IF + (IDENT_INT (10)) /= 4 THEN
               FAILED ("REDEFINITION OF INTEGER UNARY ""+"" IS DEFECTIVE");
          END IF;

          IF ABS (IDENT_INT (2)) /= 5 THEN
               FAILED ("REDEFINITION OF INTEGER ""ABS"" IS DEFECTIVE");
          END IF;

          IF IDENT_INT (7) < IDENT_INT (8) THEN
               FAILED ("REDEFINITION OF INTEGER ""<"" IS DEFECTIVE");
          END IF;

     END;

     DECLARE   -- FLOAT OPERATORS.

          -- NOTE THAT ALL LITERAL VALUES USED SHOULD BE
          --   REPRESENTABLE EXACTLY.

          FUNCTION IDENT_FLOAT (X : FLOAT) RETURN FLOAT IS
             I : INTEGER := INTEGER (X);
          BEGIN
             IF EQUAL (I, I) THEN          -- ALWAYS EQUAL.
                  RETURN X;
             END IF;
             RETURN 0.0;
          END IDENT_FLOAT;

          -- FLOAT INFIX OPERATORS.

          FUNCTION "-" (X, Y : FLOAT) RETURN FLOAT IS
          BEGIN
               IF X /= Y THEN
                    RETURN 1.0;
               ELSE RETURN 0.0;
               END IF;
          END "-";

          FUNCTION "/" (X, Y : FLOAT) RETURN FLOAT IS
          BEGIN
               IF X /= Y THEN
                    RETURN 2.0;
               ELSE RETURN 0.0;
               END IF;
          END "/";

          FUNCTION "**" (X : FLOAT; Y : INTEGER) RETURN FLOAT IS
          BEGIN
               IF INTEGER (X) /= Y THEN
                    RETURN 3.0;
               ELSE RETURN 0.0;
               END IF;
          END "**";

          -- FLOAT PREFIX OPERATOR.

          FUNCTION "-" (X : FLOAT) RETURN FLOAT IS
          BEGIN
               IF X /= 0.0 THEN
                    RETURN 4.0;
               ELSE RETURN 0.0;
               END IF;
          END "-";

          -- FLOAT RELATIONAL OPERATOR.

          FUNCTION "<=" (X, Y : FLOAT) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "<=";

     BEGIN

          IF IDENT_FLOAT (50.0) - IDENT_FLOAT (100.0) /= 1.0 THEN
               FAILED ("REDEFINITION OF FLOAT ""-"" IS DEFECTIVE");
          END IF;

          IF IDENT_FLOAT (5.0) / IDENT_FLOAT (1.0) /= 2.0 THEN
               FAILED ("REDEFINITION OF FLOAT ""/"" IS DEFECTIVE");
          END IF;

          IF IDENT_FLOAT (3.0) ** IDENT_INT (2) /= 3.0 THEN
               FAILED ("REDEFINITION OF FLOAT ""**"" IS DEFECTIVE");
          END IF;

          IF -(IDENT_FLOAT (5.0)) /= 4.0 THEN
               FAILED ("REDEFINITION OF FLOAT UNARY ""-"" IS DEFECTIVE");
          END IF;

          IF IDENT_FLOAT (1.0) <= IDENT_FLOAT (5.0) THEN
               FAILED ("REDEFINITION OF FLOAT ""<="" IS DEFECTIVE");
          END IF;

     END;

     DECLARE     -- BOOLEAN OPERATORS.

          -- BOOLEAN LOGICAL OPERATORS.

          FUNCTION "AND" (X, Y : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               IF X AND THEN Y THEN
                    RETURN FALSE;
               ELSE RETURN TRUE;
               END IF;
          END "AND";

          FUNCTION "XOR" (X, Y : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "XOR";

          -- BOOLEAN RELATIONAL OPERATOR.

          FUNCTION ">" (X, Y : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END ">";

     BEGIN

          IF IDENT_BOOL (TRUE) AND IDENT_BOOL (TRUE) THEN
               FAILED ("REDEFINITION OF ""AND"" IS DEFECTIVE");
          END IF;

          IF IDENT_BOOL (TRUE) XOR IDENT_BOOL (FALSE) THEN
               FAILED ("REDEFINITION OF ""XOR"" IS DEFECTIVE");
          END IF;

          IF IDENT_BOOL (TRUE) > IDENT_BOOL (FALSE) THEN
               FAILED ("REDEFINITION OF BOOLEAN "">"" IS DEFECTIVE");
          END IF;

     END;
    
     DECLARE     -- STRING OPERATORS.

          S1 : STRING (1..2) := "A" & IDENT_CHAR ('B');
          S2 : STRING (1..2) := "C" & IDENT_CHAR ('D');

          FUNCTION "&" (X, Y : STRING) RETURN STRING IS
               Z : STRING (1 .. X'LENGTH + Y'LENGTH);
          BEGIN
               Z (1 .. Y'LENGTH) := Y;
               Z (Y'LENGTH + 1 .. Z'LAST) := X;
               RETURN Z;
          END "&";

          FUNCTION "&" (X : CHARACTER; Y : STRING) RETURN STRING IS
               Z : STRING (1 .. Y'LENGTH + 1);
          BEGIN
               Z (1 .. Y'LENGTH) := Y;
               Z (Z'LAST) := X;
               RETURN Z;
          END "&";

          -- STRING RELATIONAL OPERATOR.

          FUNCTION ">=" (X, Y : STRING) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END ">=";

     BEGIN

          IF S1 & S2 /= "CDAB" THEN
               FAILED ("BAD REDEFINITION OF ""&"" (S,S)");
          END IF;

          IF IDENT_CHAR ('C') & S1 /= "ABC" THEN
               FAILED ("BAD REDEFINITION OF ""&"" (C,S)");
          END IF;

          IF S2 >= S1 THEN
               FAILED ("BAD REDEFINITION OF STRING "">=""");
          END IF;

     END;

     DECLARE      -- CHARACTER OPERATORS.

          -- CHARACTER RELATIONAL OPERATORS.

          FUNCTION ">" (X, Y : CHARACTER) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END ">";

          FUNCTION "<=" (X, Y : CHARACTER) RETURN BOOLEAN IS
          BEGIN
               RETURN X = Y;
          END "<=";

     BEGIN

          IF IDENT_CHAR ('C') > IDENT_CHAR ('B') THEN
               FAILED ("REDEFINITION OF CHARACTER "">"" IS DEFECTIVE");
          END IF;

          IF IDENT_CHAR ('A') <= IDENT_CHAR ('E') THEN
               FAILED ("REDEFINITION OF CHARACTER ""<="" IS DEFECTIVE");
          END IF;

     END;

     RESULT;

END C67003F;
