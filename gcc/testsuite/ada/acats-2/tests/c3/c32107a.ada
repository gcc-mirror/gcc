-- C32107A.ADA

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
-- CHECK THAT OBJECT DECLARATIONS ARE ELABORATED IN THE ORDER OF THEIR 
-- OCCURRENCE, I.E., THAT EXPRESSIONS ASSOCIATED WITH ONE DECLARATION
-- (INCLUDING DEFAULT EXPRESSIONS, IF APPROPRIATE) ARE EVALUATED BEFORE
-- ANY EXPRESSION BELONGING TO THE NEXT DECLARATION. ALSO, CHECK THAT
-- EXPRESSIONS IN THE SUBTYPE INDICATION OR THE CONSTRAINED ARRAY 
-- DEFINITION ARE EVALUATED BEFORE ANY INITIALIZATION EXPRESSIONS ARE
-- EVALUATED.

-- R.WILLIAMS 9/24/86

WITH REPORT; USE REPORT;
PROCEDURE C32107A IS

     BUMP : INTEGER := 0;

     ORDER_CHECK : INTEGER;

     G1, H1, I1 : INTEGER;

     FIRST_CALL : BOOLEAN := TRUE;

     TYPE ARR1 IS ARRAY (POSITIVE RANGE <>) OF INTEGER;

     TYPE ARR1_NAME IS ACCESS ARR1;

     TYPE ARR2 IS ARRAY (POSITIVE RANGE <>, POSITIVE RANGE <>) OF 
          INTEGER;

     TYPE REC (D : INTEGER) IS
          RECORD
               COMP : INTEGER;
          END RECORD;          

     TYPE REC_NAME IS ACCESS REC;

     FUNCTION F RETURN INTEGER IS
     BEGIN
          BUMP := BUMP + 1;
          RETURN BUMP;
     END F;

     FUNCTION G RETURN INTEGER IS
     BEGIN
          BUMP := BUMP + 1;
          G1 := BUMP;
          RETURN BUMP;
     END G;
     
     FUNCTION H RETURN INTEGER IS
     BEGIN
          BUMP := BUMP + 1;
          H1 := BUMP;
          RETURN BUMP;
     END H;
     
     FUNCTION I RETURN INTEGER IS
     BEGIN
          IF FIRST_CALL THEN
               BUMP := BUMP + 1;
               I1 := BUMP;
               FIRST_CALL := FALSE;
          END IF;
          RETURN I1;
     END I;

BEGIN
     TEST ( "C32107A", "CHECK THAT OBJECT DECLARATIONS ARE " &
                       "ELABORATED IN THE ORDER OF THEIR " &
                       "OCCURRENCE, I.E., THAT EXPRESSIONS " &
                       "ASSOCIATED WITH ONE DECLARATION (INCLUDING " &
                       "DEFAULT EXPRESSIONS, IF APPROPRIATE) ARE " &
                       "EVALUATED BEFORE ANY EXPRESSION BELONGING " &
                       "TO THE NEXT DECLARATION.  ALSO, CHECK THAT " &
                       "EXPRESSIONS IN THE SUBTYPE INDICATION OR " &
                       "THE CONSTRAINED ARRAY DEFINITION ARE " &
                       "EVALUATED BEFORE ANY INITIALIZATION " &
                       "EXPRESSIONS ARE EVALUATED" );
     
     DECLARE -- (A).
          I1 : INTEGER := 10000 * F;
          A1 : CONSTANT ARRAY (1 .. H) OF REC (G * 100) :=
               (1 .. H1 => (G1 * 100, I * 10));
          I2 : CONSTANT INTEGER := F * 1000;
     BEGIN
          ORDER_CHECK := I1 + I2 + A1'LAST + A1 (1).D + A1 (1).COMP;
          IF ORDER_CHECK = 15243 OR ORDER_CHECK = 15342 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (A)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 15343 OR " &
                        "15242 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (A)" );
          END IF;
     END; -- (A).         

     BUMP := 0;

     DECLARE -- (B).
          A : ARR2 (1 .. F, 1 .. F * 10);
          R : REC (G * 100) := (G1 * 100, F * 1000);
          I : INTEGER RANGE 1 .. H;
          S : REC (F * 10);               
     BEGIN
          ORDER_CHECK := 
               A'LAST (1) + A'LAST (2) + R.D + R.COMP; 
          IF (H1 + S.D = 65) AND 
             (ORDER_CHECK = 4321 OR ORDER_CHECK = 4312) THEN
               COMMENT ( "ORDER_CHECK HAS VALUE 65 " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (B)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 65 4321 OR " &
                        "65 4312 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (H1 + S.D) &
                         INTEGER'IMAGE (ORDER_CHECK) & " - (B)" );
          END IF;
     END; -- (B).         

     BUMP := 0;

     DECLARE -- (C).
          I1 : CONSTANT INTEGER RANGE 1 .. G * 10 := F;
          A1 : ARRAY (1 .. F * 100) OF INTEGER RANGE 1 .. H * 1000;
     BEGIN
          ORDER_CHECK := I1 + (G1 * 10) + A1'LAST + (H1 * 1000);
          IF ORDER_CHECK = 4312 OR ORDER_CHECK = 3412 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (C)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4312 OR " &
                        "3412 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (C)" );
          END IF;
     END; -- (C).         

     BUMP := 0;
     FIRST_CALL := TRUE;

     DECLARE -- (D).
          A1 : ARRAY (1 .. G) OF REC (H * 10000) := 
               (1 .. G1 => (H1 * 10000, I * 100));
          R1 : CONSTANT REC := (F * 1000, F * 10);

     BEGIN
          ORDER_CHECK := 
               A1'LAST + A1 (1).D + A1 (1).COMP + R1.D + R1.COMP;
          IF ORDER_CHECK = 25341 OR ORDER_CHECK = 24351 OR
             ORDER_CHECK = 15342 OR ORDER_CHECK = 14352 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (D)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 25341, " &
                        "24351, 15342 OR 14352  -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (D)" );
          END IF;
     END; -- (D).         

     BUMP := 0;

     DECLARE -- (E).
          A1 : CONSTANT ARR1_NAME := NEW ARR1' (1 .. F => F * 10);
          R1 : REC_NAME (H * 100) := NEW REC'(H1 * 100, F * 1000);

     BEGIN
          ORDER_CHECK := A1.ALL'LAST + A1.ALL (1) + R1.D + R1.COMP;
          IF ORDER_CHECK /= 4321 THEN
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4321 " &
                        "-- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (E)" );
          END IF;
     END; -- (E).         

     BUMP := 0;
     FIRST_CALL := TRUE;

     DECLARE -- (F).
          A1 : CONSTANT ARRAY (1 .. G) OF INTEGER RANGE 1 .. H * 100 :=
               (1 .. G1 => I * 10);
          A2 : ARR1 (1 .. F * 1000);
     BEGIN
          ORDER_CHECK := 
               A1'LAST + (H1 * 100) + A1 (1) + A2'LAST;
          IF ORDER_CHECK = 4231 OR ORDER_CHECK = 4132 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (F)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4231 OR " &
                        "4132 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (F)" );
          END IF;
     END; -- (F).         

     BUMP := 0;

     DECLARE -- (G).
          A1 : ARR1_NAME (1 .. G) := NEW ARR1 (1 .. G1);
          R1 : CONSTANT REC_NAME (H * 10) := 
               NEW REC'(H1 * 10, F * 100);
     BEGIN
          ORDER_CHECK := A1.ALL'LAST + R1.D + R1.COMP;
          IF ORDER_CHECK /= 321 THEN
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 321 OR " &
                        "-- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (G)" );
          END IF;
     END; -- (G).         

     BUMP := 0;

     DECLARE -- (H). 
          TYPE REC (D : INTEGER := F) IS
               RECORD
                    COMP : INTEGER := F * 10;
               END RECORD;

          R1 : REC;
          R2 : REC (G * 100) := (G1 * 100, F * 1000);
     BEGIN
          ORDER_CHECK := R1.D + R1.COMP + R2.D + R2.COMP;
          IF ORDER_CHECK = 4321 OR ORDER_CHECK = 4312 OR
             ORDER_CHECK = 3421 OR ORDER_CHECK = 3412 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (H)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4321, " &
                        "4312, 3421, OR 3412 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (H)" );
          END IF;
     END; -- (H).         

     BUMP := 0;

     DECLARE -- (I).
          TYPE REC2 (D1, D2 : INTEGER) IS
               RECORD
                    COMP : INTEGER;
               END RECORD;

          R1 : REC2 (G  * 1000, H  * 10000) := 
                    (G1 * 1000, H1 * 10000, F * 100);
          R2 : REC2 (F, F * 10);
     BEGIN
          ORDER_CHECK := R1.D1 + R1.D2 + R1.COMP + R2.D1 + R2.D2;
          IF ORDER_CHECK = 21354 OR ORDER_CHECK = 21345 OR
             ORDER_CHECK = 12345 OR ORDER_CHECK = 12354 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (I)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 21354, " &
                        "21345, 12354, OR 12345 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (I)" );
          END IF;

     END; -- (I).         

     BUMP := 0;

     DECLARE -- (J).
          PACKAGE P IS 
               TYPE PRIV (D : INTEGER) IS PRIVATE;

               P1 : CONSTANT PRIV;
               P2 : CONSTANT PRIV;          

               FUNCTION GET_A (P : PRIV) RETURN INTEGER;
          PRIVATE
               TYPE PRIV (D : INTEGER) IS
                    RECORD
                         COMP : INTEGER;
                    END RECORD;
               P1 : CONSTANT PRIV := (F , F * 10);
               P2 : CONSTANT PRIV := (F * 100, F * 1000);
          END P;
                    
          PACKAGE BODY P IS
               FUNCTION GET_A (P : PRIV) RETURN INTEGER IS
               BEGIN
                    RETURN P.COMP;
               END GET_A;
          END P;
          
          USE P;
     BEGIN
          ORDER_CHECK := P1.D + GET_A (P1) + P2.D + GET_A (P2);
          IF ORDER_CHECK = 4321 OR ORDER_CHECK = 4312 OR
             ORDER_CHECK = 3412 OR ORDER_CHECK = 3421 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (J)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4321, " &
                        "4312, 3421, OR 3412 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (J)" );
          END IF;
     END; -- (J).         

     BUMP := 0;

     DECLARE -- (K).
          PACKAGE P IS 
               TYPE PRIV (D1, D2 : INTEGER) IS PRIVATE;

          PRIVATE
               TYPE PRIV (D1, D2 : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END P;
          
          USE P;

          P1 : PRIV (F, F * 10);
          P2 : PRIV (F * 100, F * 1000);

     BEGIN
          ORDER_CHECK := P1.D1 + P1.D2 + P2.D1 + P2.D2;
          IF ORDER_CHECK = 4321 OR ORDER_CHECK = 4312 OR
             ORDER_CHECK = 3412 OR ORDER_CHECK = 3421 THEN
               COMMENT ( "ORDER_CHECK HAS VALUE " &
                          INTEGER'IMAGE (ORDER_CHECK) & " - (K)" );
          ELSE
               FAILED ( "OBJECTS NOT ELABORATED IN PROPER ORDER " &
                        "VALUE OF ORDER_CHECK SHOULD BE 4321, 4312, " &
                        "3421, OR 3412 -- ACTUAL VALUE IS " & 
                         INTEGER'IMAGE (ORDER_CHECK) & " - (K)" );
          END IF;

     END; -- (K).         

     RESULT;
END C32107A;
