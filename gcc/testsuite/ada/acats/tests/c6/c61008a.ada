-- C61008A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF THE DEFAULT VALUE
-- FOR A FORMAL PARAMETER DOES NOT SATISFY THE CONSTRAINTS OF THE 
-- SUBTYPE_INDICATION WHEN THE DECLARATION IS ELABORATED, ONLY WHEN
-- THE DEFAULT IS USED.

--   SUBTESTS ARE:
--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.
--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.
--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.
--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

-- DAS  1/20/81
-- SPS 10/26/82
-- VKG 1/13/83
-- SPS 2/9/83
-- BHS 7/9/84

WITH REPORT;
PROCEDURE C61008A IS

     USE REPORT;

BEGIN

     TEST ("C61008A", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF " &
                      "AN INITIALIZATION VALUE DOES NOT SATISFY " &
                      "CONSTRAINTS ON A FORMAL PARAMETER");

     --------------------------------------------------

     DECLARE -- (A)

          PROCEDURE PA (I1, I2 : INTEGER) IS

               TYPE A1 IS ARRAY (1..I1,1..I2) OF INTEGER;

               PROCEDURE PA1 (A : A1 := ((1,0),(0,1))) IS
               BEGIN
                    FAILED ("BODY OF PA1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN PA1");
               END PA1;

          BEGIN
               PA1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PA1");
          END PA;

     BEGIN   -- (A)
          PA (IDENT_INT(1), IDENT_INT(10));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO PA");
     END;    -- (A)

     --------------------------------------------------

     DECLARE -- (B)

          PROCEDURE PB (I1, I2 : INTEGER) IS

               SUBTYPE INT IS INTEGER RANGE I1..I2;

               PROCEDURE PB1 (I : INT := -1) IS
               BEGIN
                    FAILED ("BODY OF PB1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN PB1");
               END PB1;

          BEGIN
               PB1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PB1");
          END PB;

     BEGIN   -- (B)
          PB (IDENT_INT(0), IDENT_INT(63));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO PB");
     END;    -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          PROCEDURE PC (I1, I2 : INTEGER) IS
               TYPE AR1 IS ARRAY (1..3) OF INTEGER RANGE I1..I2; 
               TYPE REC IS
                    RECORD
                         I : INTEGER RANGE I1..I2;
                         A : AR1 ;
                    END RECORD;

               PROCEDURE PC1 (R : REC := (-3,(0,2,3))) IS
               BEGIN
                    FAILED ("BODY OF PC1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN PC1");
               END PC1;

          BEGIN
               PC1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PC1");
          END PC;

     BEGIN   -- (C)
          PC (IDENT_INT(1), IDENT_INT(3));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO PC");
     END;    -- (C)

     --------------------------------------------------

     DECLARE -- (D1)

          PROCEDURE P1D (I1, I2 : INTEGER) IS

               TYPE A1 IS ARRAY (1..2,1..2) OF INTEGER RANGE I1..I2;

               PROCEDURE P1D1 (A : A1 := ((1,-1),(1,2))) IS
               BEGIN
                    FAILED ("BODY OF P1D1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN P1D1");
               END P1D1;

          BEGIN
               P1D1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - P1D1");
          END P1D;

     BEGIN   -- (D1)
          P1D (IDENT_INT(1), IDENT_INT(2));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO P1D");
     END;    -- (D1)

     --------------------------------------------------

     DECLARE -- (D2)

          PROCEDURE P2D (I1, I2 : INTEGER) IS
               
               TYPE A1 IS ARRAY (1..2,1..2) OF INTEGER RANGE I1..I2;

               PROCEDURE P2D1 (A : A1 := (3..4 => (1,2))) IS
               BEGIN
                    FAILED ("BODY OF P2D1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN P2D1");
               END P2D1;

          BEGIN
               P2D1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - P2D1");
          END P2D;

     BEGIN  -- (D2)
          P2D (IDENT_INT(1), IDENT_INT(2));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO P2D");
     END;   -- (D2)

     --------------------------------------------------

     DECLARE -- (E)

          PROCEDURE PE (I1, I2 : INTEGER) IS
               SUBTYPE INT IS INTEGER RANGE 0..10;
               TYPE ARR IS ARRAY (1..3) OF INT;
               TYPE REC (I : INT) IS
                    RECORD
                         A : ARR;
                    END RECORD;

               SUBTYPE REC4 IS REC(I1);

               PROCEDURE PE1 (R : REC4 := (3,(1,2,3))) IS
               BEGIN
                    FAILED ("BODY OF PE1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN PE1");
               END PE1;

          BEGIN
               PE1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PE1");
          END PE;

     BEGIN   -- (E)
          PE (IDENT_INT(4), IDENT_INT(10));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN CALL TO PE");
     END;    -- (E)

     --------------------------------------------------

     RESULT;

END C61008A;
