-- C48009B.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT CONSTRAINT_ERROR
-- IS RAISED IF T IS AN UNCONSTRAINED RECORD OR PRIVATE TYPE, (X) IS AN
-- AGGREGATE OR A VALUE OF TYPE T, AND ONE OF THE DISCRIMINANT VALUES IN
-- X:
--   1) DOES NOT SATISFY THE RANGE CONSTRAINT FOR THE CORRESPONDING
--      DISCRIMINANT OF T.
--   2) DOES NOT EQUAL THE DISCRIMINANT VALUE SPECIFIED IN THE
--      DECLARATION OF THE ALLOCATOR'S BASE TYPE.
--   3) A DISCRIMINANT VALUE IS COMPATIBLE WITH A DISCRIMINANT'S SUBTYPE
--      BUT DOES NOT PROVIDE A COMPATIBLE INDEX OR DISCRIMINANT
--      CONSTRAINT FOR A SUBCOMPONENT DEPENDENT ON THE DISCRIMINANT.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/02/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE  C48009B  IS

     USE REPORT;

BEGIN

     TEST( "C48009B" , "FOR ALLOCATORS OF THE FORM 'NEW T '(X)', " &
                       "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                       "APPROPRIATE - UNCONSTRAINED RECORD AND " &
                       "PRIVATE TYPES");

     DECLARE

          SUBTYPE I1_7 IS INTEGER RANGE IDENT_INT(1)..IDENT_INT(7);
          SUBTYPE I1_10 IS INTEGER RANGE IDENT_INT(1)..IDENT_INT(10);
          SUBTYPE I2_9 IS INTEGER RANGE IDENT_INT(2)..IDENT_INT(9);

          TYPE REC (A : I2_9) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE ARR IS ARRAY (I2_9 RANGE <>) OF INTEGER;

          TYPE T_REC (C : I1_10) IS
               RECORD
                    D : REC(C);
               END RECORD;

          TYPE T_ARR (C : I1_10) IS
               RECORD
                    D : ARR(2..C);
                    E : ARR(C..9);
               END RECORD;

          TYPE T_REC_REC (A : I1_10) IS
               RECORD
                    B : T_REC(A);
               END RECORD;

          TYPE T_REC_ARR (A : I1_10) IS
               RECORD
                    B : T_ARR(A);
               END RECORD;

          TYPE  TB (  A : I1_7 )  IS
               RECORD
                    R : INTEGER;
               END RECORD;

          TYPE A_T_REC_REC IS ACCESS T_REC_REC;
          TYPE A_T_REC_ARR IS ACCESS T_REC_ARR;
          TYPE ATB IS ACCESS TB;
          TYPE ACTB IS ACCESS TB(3);

          VA_T_REC_REC : A_T_REC_REC;
          VA_T_REC_ARR : A_T_REC_ARR;
          VB  : ATB;
          VCB : ACTB;

          PACKAGE  P  IS
               TYPE  PRIV( A : I1_10 )  IS PRIVATE;
               CONS_PRIV : CONSTANT PRIV;
          PRIVATE
               TYPE  PRIV( A : I1_10 )  IS
                    RECORD
                         R : INTEGER;
                    END RECORD;
               CONS_PRIV : CONSTANT PRIV := (2, 3);
          END P;

          USE P;

          TYPE  A_PRIV  IS  ACCESS P.PRIV;
          TYPE  A_CPRIV IS  ACCESS P.PRIV (3);
     
          VP  : A_PRIV;
          VCP : A_CPRIV;

          FUNCTION ALLOC1(X : P.PRIV) RETURN A_CPRIV IS
          BEGIN
               IF EQUAL(1, 1) THEN
                    RETURN NEW P.PRIV'(X);
               ELSE
                    RETURN NULL;
               END IF;
          END ALLOC1;
          FUNCTION ALLOC2(X : TB) RETURN ACTB IS
          BEGIN
               IF EQUAL(1, 1) THEN
                    RETURN NEW TB'(X);
               ELSE
                    RETURN NULL;
               END IF;
          END ALLOC2;

     BEGIN

          BEGIN     -- B1
               VB  :=  NEW TB'(A => IDENT_INT(0), R => 1);
               FAILED ("NO EXCEPTION RAISED - CASE 1A");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>  NULL;
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE 1A" );
          END;

          BEGIN     
               VB  :=  NEW TB'(A => 8, R => 1);
               FAILED ("NO EXCEPTION RAISED - CASE 1B");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>  NULL;
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE 1B");
          END; -- B1

          BEGIN     -- B2
               VCB := NEW TB'(2, 3);
               FAILED ("NO EXCEPTION RAISED - CASE 2A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 2A");
          END;

          BEGIN
               IF ALLOC2((IDENT_INT(4), 3)) = NULL THEN
                    FAILED ("IMPOSSIBLE - CASE 2B");
               END IF;
               FAILED ("NO EXCEPTION RAISED - CASE 2B");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 2B");
          END;

          BEGIN

               IF ALLOC1(CONS_PRIV) = NULL THEN
                    FAILED ("IMPOSSIBLE - CASE 2C");
               END IF;
               FAILED ("NO EXCEPTION RAISED - CASE 2C");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 2C");

          END; -- B2

          BEGIN     -- B3

               VA_T_REC_REC := NEW T_REC_REC'(1, (1, (A => 1)));
               FAILED ("NO EXCEPTION RAISED - CASE 3A");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 3A");

          END;

          BEGIN

               VA_T_REC_REC := NEW T_REC_REC'(10, 
                                             (10, (A => 10)));
               FAILED ("NO EXCEPTION RAISED - CASE 3B");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 3B");

          END;

          BEGIN

               VA_T_REC_ARR := NEW T_REC_ARR'(1, (1, (OTHERS => 1),
                                                     (OTHERS => 2)));
               FAILED ("NO EXCEPTION RAISED - CASE 3C");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 3C");

          END;

          BEGIN

               VA_T_REC_ARR := NEW T_REC_ARR'(10, (10, (OTHERS => 1),
                                                       (OTHERS => 2)));
               FAILED ("NO EXCEPTION RAISED - CASE 3D");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 3D");

          END;

     END;

     RESULT;

END C48009B;
