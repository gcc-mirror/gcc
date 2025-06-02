-- C48008A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T X", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS AN UNCONSTRAINED RECORD, PRIVATE, OR LIMITED TYPE, X
-- IS A DISCRIMINANT CONSTRAINT, AND
--   1) ONE OF THE VALUES OF X IS OUTSIDE THE RANGE OF THE CORRESPONDING
--      DISCRIMINANT;
--   2) ONE OF THE DISCRIMINANT VALUES IS NOT COMPATIBLE WITH A
--      CONSTRAINT OF A SUBCOMPONENT IN WHICH IT IS USED;
--   3) ONE OF THE DISCRIMINANT VALUES DOES NOT EQUAL THE CORRESPONDING
--      VALUE OF THE ALLOCATOR'S BASE TYPE;
--   4) A DEFAULT INITIALIZATION RAISES AN EXCEPTION.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/02/83
-- EG  07/05/84
-- PWB 02/05/86  CORRECTED TEST ERROR:
--               CHANGED "FAILED" TO "COMMENT" IN PROCEDURE INCR_CHECK,
--               SO AS NOT TO PROHIBIT EVAL OF DEFLT EXPR (AI-00397/01)
--               ADDED COMMENTS FOR CASES.

WITH REPORT;

PROCEDURE  C48008A  IS

     USE REPORT;

BEGIN

     TEST( "C48008A" , "FOR ALLOCATORS OF THE FORM 'NEW T X', " &
                       "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                       "APPROPRIATE - UNCONSTRAINED RECORD AND " &
                       "PRIVATE TYPES");

     DECLARE

          DISC_FLAG : BOOLEAN := FALSE;
          INCR_VAL : INTEGER;
          FUNCTION INCR(A : INTEGER) RETURN INTEGER;

          SUBTYPE I1_7 IS INTEGER RANGE IDENT_INT(1)..IDENT_INT(7);
          SUBTYPE I1_10 IS INTEGER RANGE IDENT_INT(1)..IDENT_INT(10);
          SUBTYPE I2_9 IS INTEGER RANGE IDENT_INT(2)..IDENT_INT(9);

          TYPE REC (A : I2_9) IS
               RECORD
                    B : INTEGER := INCR(2);
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
                    R : INTEGER := INCR(1);
               END RECORD;

          TYPE UR (A : INTEGER) IS
               RECORD
                    B : I2_9 := INCR(1);
               END RECORD;

          TYPE A_T_REC_REC IS ACCESS T_REC_REC;
          TYPE A_T_REC_ARR IS ACCESS T_REC_ARR;
          TYPE ATB IS ACCESS TB;
          TYPE ACTB IS ACCESS TB(3);
          TYPE A_UR IS ACCESS UR;

          VA_T_REC_REC : A_T_REC_REC;
          VA_T_REC_ARR : A_T_REC_ARR;
          VB  : ATB;
          VCB : ACTB;
          V_A_UR : A_UR;

          BOOL : BOOLEAN;

          FUNCTION DISC (A : INTEGER) RETURN INTEGER;


          PACKAGE  P  IS
               TYPE  PRIV( A : I1_10 := DISC(8) )  IS PRIVATE;
               CONS_PRIV : CONSTANT PRIV;
          PRIVATE
               TYPE  PRIV( A : I1_10 := DISC(8) )  IS
                    RECORD
                         R : INTEGER := INCR(1);
                    END RECORD;
               CONS_PRIV : CONSTANT PRIV := (2, 3);
          END P;

          TYPE  A_PRIV  IS  ACCESS P.PRIV;
          TYPE  A_CPRIV IS  ACCESS P.PRIV (3);

          VP  : A_PRIV;
          VCP : A_CPRIV;

          PROCEDURE PREC_REC (X : A_T_REC_REC) IS
          BEGIN
               NULL;
          END PREC_REC;

          PROCEDURE PREC_ARR (X : A_T_REC_ARR) IS
          BEGIN
               NULL;
          END PREC_ARR;

          PROCEDURE PB (X : ATB) IS
          BEGIN
               NULL;
          END PB;

          PROCEDURE PCB (X : ACTB) IS
          BEGIN
               NULL;
          END PCB;

          PROCEDURE PPRIV (X : A_PRIV) IS
          BEGIN
               NULL;
          END PPRIV;

          PROCEDURE PCPRIV (X : A_CPRIV) IS
          BEGIN
               NULL;
          END PCPRIV;

          FUNCTION DISC (A : INTEGER) RETURN INTEGER IS
          BEGIN
               DISC_FLAG := TRUE;
               RETURN A;
          END DISC;

          FUNCTION INCR(A : INTEGER) RETURN INTEGER IS
          BEGIN
               INCR_VAL := IDENT_INT(INCR_VAL+1);
               RETURN A;
          END INCR;

          PROCEDURE INCR_CHECK(CASE_ID : STRING) IS
          BEGIN
               IF INCR_VAL /= IDENT_INT(0) THEN
                    COMMENT ("DEFAULT INITIAL VALUE WAS EVALUATED - " &
                             "CASE " & CASE_ID);
               END IF;
          END INCR_CHECK;

     BEGIN

          BEGIN  -- A1A: 0 ILLEGAL FOR TB.A.
               INCR_VAL := 0;
               VB  :=  NEW TB (A => 0);
               FAILED ("NO EXCEPTION RAISED - CASE A1A");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>
                    INCR_CHECK("A1A");
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE A1A" );
          END;   -- A1A

          BEGIN  -- A1B: 8 ILLEGAL IN I1_7.
               INCR_VAL := 0;
               VB  :=  NEW TB (A => I1_7'(IDENT_INT(8)));
               FAILED ("NO EXCEPTION RAISED - CASE A1B");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>
                    INCR_CHECK("A1B");
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE A1B");
          END;   -- A1B

          BEGIN  -- A1C: 8 ILLEGAL FOR TB.A.
               INCR_VAL := 0;
               PB(NEW TB (A => 8));
               FAILED ("NO EXCEPTION RAISED - CASE A1C");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>
                    INCR_CHECK("A1C");
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE A1C");
          END;   --A1C

          BEGIN  --A1D: 0 ILLEGAL FOR TB.A.
               INCR_VAL := 0;
               BOOL := ATB'(NEW TB(A => 0)) = NULL;
               FAILED ("NO EXCEPTION RAISED - CASE A1D");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>
                    INCR_CHECK("A1D");
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE A1D");
          END;   --A1D

          BEGIN  --A1E: 11 ILLEGAL FOR PRIV.A.
               DISC_FLAG := FALSE;
               INCR_VAL := 0;
               VP := NEW P.PRIV(11);
               FAILED("NO EXCEPTION RAISED - CASE A1E");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF DISC_FLAG THEN
                         FAILED ("DISCR DEFAULT EVALUATED WHEN " &
                                 "EXPLICIT VALUE WAS PROVIDED - A1E");
                    END IF;
                    INCR_CHECK("A1E");
               WHEN OTHERS           =>
                    FAILED("WRONG EXCEPTION RAISED - CASE A1E");
          END;   -- A1E

          BEGIN  -- A2A: 1 ILLEGAL FOR REC.A.
               INCR_VAL := 0;
               VA_T_REC_REC := NEW T_REC_REC(A => I1_10'(IDENT_INT(1)));
               FAILED ("NO EXCEPTION RAISED - CASE A2A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK("A2A");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A2A");
          END;   -- A2A

          BEGIN  --A2B: 10 ILLEGAL FOR REC.A.
               INCR_VAL := 0;
               VA_T_REC_REC := NEW T_REC_REC (10);
               FAILED ("NO EXCEPTION RAISED - CASE A2B");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK("A2B");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A2B");
          END;   -- A2B

          BEGIN  -- A2C: 1 ILLEGAL FOR T.ARR.E'FIRST.
               INCR_VAL := 0;
               PREC_ARR (NEW T_REC_ARR (1));
               FAILED ("NO EXCEPTION RAISED - CASE A2C");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK ("A2C");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A2C");
          END;   -- A2C

          BEGIN  -- A2D: 10 ILLEGAL FOR T_ARR.D'LAST.
               INCR_VAL := 0;
               BOOL := NEW T_REC_ARR (IDENT_INT(10)) = NULL;
               FAILED ("NO EXCEPTION RAISED - CASE A2D");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK ("A2D");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A2D");
          END;   -- A2D

          BEGIN -- A3A: ASSIGNMENT VIOLATES CONSTRAINT ON VCB'S SUBTYPE.
               INCR_VAL := 0;
               VCB := NEW TB (4);
               FAILED ("NO EXCEPTION RAISED - CASE A3A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK("A3A");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A3A");
          END;   -- A3A

          BEGIN  -- A3B: PARM ASSOC VIOLATES CONSTRAINT ON PARM SUBTYPE.
               INCR_VAL := 0;
               PCB (NEW TB (4));
               FAILED ("NO EXCEPTION RAISED - CASE A3B");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK("A3B");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A3B");
          END;   -- A3B

          BEGIN  -- A3C: 2 VIOLATES CONSTRAINT ON SUBTYPE ACTB.
               INCR_VAL := 0;
               BOOL := ACTB'(NEW TB (IDENT_INT(2))) = NULL;
               FAILED ("NO EXCEPTION RAISED - CASE A3C");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    INCR_CHECK("A3C");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A3C");
          END;   -- A3C

          BEGIN  -- A4A: EVALUATION OF DEFAULT RAISES EXCEPTION.
               INCR_VAL := 0;
               V_A_UR := NEW UR(4);
               FAILED ("NO EXCEPTION RAISED - CASE A4A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE A4A");
          END;   -- A4A

     END;

     RESULT;

END C48008A;
