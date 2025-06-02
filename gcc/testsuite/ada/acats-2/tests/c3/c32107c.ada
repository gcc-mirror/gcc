-- C32107C.ADA

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
-- FOR OBJECTS OF A GENERIC FORMAL TYPE WHOSE ACTUAL PARAMETER IS A 
-- TYPE WITH DEFAULT VALUES, CHECK THAT OBJECT DECLARATIONS ARE 
-- ELABORATED IN THE ORDER OF THEIR OCCURRENCE, I.E., THAT EXPRESSIONS 
-- ASSOCIATED WITH ONE DECLARATION (INCLUDING DEFAULT EXPRESSIONS) ARE 
-- EVALUATED BEFORE ANY EXPRESSION BELONGING TO THE NEXT DECLARATION. 

-- R.WILLIAMS 9/24/86

WITH REPORT; USE REPORT;
PROCEDURE C32107C IS

     BUMP : INTEGER := 0;

     G1, H1 : INTEGER;

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
     
BEGIN
     TEST ( "C32107C", "FOR OBJECTS OF A GENERIC FORMAL TYPE WHOSE " &
                       "ACTUAL PARAMETER IS A TYPE WITH DEFAULT " &
                       "VALUES, CHECK THAT OBJECT DECLARATIONS ARE " &
                       "ELABORATED IN THE ORDER OF THEIR " &
                       "OCCURRENCE, I.E., THAT EXPRESSIONS " &
                       "ASSOCIATED WITH ONE DECLARATION (INCLUDING " &
                       "DEFAULT EXPRESSIONS) ARE EVALUATED BEFORE " &
                       "ANY EXPRESSION BELONGING TO THE NEXT " &
                       "DECLARATION" );

     DECLARE -- (A).
          TYPE REC (D : INTEGER := F) IS
               RECORD
                    A : INTEGER := F;
               END RECORD;

          FUNCTION GET_A (R : REC) RETURN INTEGER IS
          BEGIN
               RETURN R.A;
          END GET_A;

          GENERIC
               TYPE T IS (<>);
               TYPE PRIV (D : T) IS PRIVATE;
               WITH FUNCTION GET_A (P : PRIV) RETURN INTEGER IS <>;
          PROCEDURE P;

          PROCEDURE P IS
               P1 : PRIV (T'VAL (F));
               P2 : PRIV (T'VAL (F * 100));
               ORDER_CHECK : INTEGER;
               
          BEGIN
               ORDER_CHECK := 
                    T'POS (P1.D) + T'POS (P2.D) + 
                    (GET_A (P1) * 10) + (GET_A (P2) * 1000);
               IF ORDER_CHECK /= 4321 THEN
                    FAILED ( "OBJECTS NOT ELABORATED IN PROPER " &
                             "ORDER VALUE OF ORDER_CHECK SHOULD BE " &
                             "4321 -- ACTUAL VALUE IS " & 
                              INTEGER'IMAGE (ORDER_CHECK) & " - (A)" );
               END IF;
          END P;
          
          PROCEDURE PROC IS NEW P (INTEGER, REC);

     BEGIN
          PROC;
     END; -- (A).         

     BUMP := 0;

     DECLARE -- (B).
          TYPE REC (D1 : INTEGER := F; D2 : INTEGER := F) IS
               RECORD
                    A : INTEGER := F;
               END RECORD;

          FUNCTION GET_A (R : REC) RETURN INTEGER IS
          BEGIN
               RETURN R.A;
          END GET_A;

          GENERIC
               TYPE T IS (<>);
               TYPE PRIV (D1 : T; D2 : T) IS PRIVATE;
               WITH FUNCTION GET_A (P : PRIV) RETURN INTEGER IS <>;
          PROCEDURE P;

          PROCEDURE P IS
               P1 : PRIV (T'VAL (F * 1000), T'VAL (F * 10000));
               P2 : PRIV (T'VAL (F), T'VAL (F * 10));
               ORDER_CHECK : INTEGER;
               
          BEGIN
               ORDER_CHECK := 
               T'POS (P1.D1) + T'POS (P1.D2) + 
               T'POS (P2.D1) + T'POS (P2.D2) + 
               (GET_A (P1) * 100);
               IF (GET_A (P2) = 6) AND 
                  (ORDER_CHECK = 12345 OR ORDER_CHECK = 21345 OR
                   ORDER_CHECK = 21354 OR ORDER_CHECK = 12354) THEN
                    COMMENT ( "ORDER_CHECK HAS VALUE " &
                               INTEGER'IMAGE (ORDER_CHECK) & 
                              " - (B)" );
               ELSE
                    FAILED ( "OBJECTS NOT ELABORATED IN PROPER " &
                             "ORDER VALUE OF ORDER_CHECK SHOULD BE " &
                             "6 12345, 6 21345, 6 21354, OR " &
                             "6 12354 -- ACTUAL VALUE IS " &
                              INTEGER'IMAGE (GET_A (P2)) &
                              INTEGER'IMAGE (ORDER_CHECK) & " - (B)" );
               END IF;

          END P;
          
          PROCEDURE PROC IS NEW P (INTEGER, REC);

     BEGIN
          PROC;
     END; -- (B).         

     RESULT;
END C32107C;
