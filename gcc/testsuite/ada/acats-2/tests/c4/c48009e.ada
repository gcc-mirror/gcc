-- C48009E.ADA
 
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
-- IS RAISED IF T IS A CONSTRAINED ARRAY TYPE AND:
--   1) A NAMED NULL OR NON-NULL BOUND FOR X DOES NOT EQUAL THE
--      CORRESPONDING BOUND FOR T;
--   2) A BOUND OF T DOES NOT EQUAL THE CORRESPONDING VALUE SPECIFIED IN
--      THE DECLARATION OF THE ALLOCATOR'S BASE TYPE; 
--   3) A POSITIONAL AGGREGATE DOES NOT HAVE THE NUMBER OF COMPONENTS
--      REQUIRED BY T OR BY THE ALLOCATOR'S BASE TYPE. 
 
 -- RM  01/08/80
 -- NL  10/13/81
 -- SPS 10/26/82
 -- JBG 03/03/83
 -- EG  07/05/84
 -- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.
 -- KAS 11/14/95 CHANGED FAILURE AT SLIDING ASSIGNMENT TO COMMENT ON LANGUAGE
 -- KAS 11/30/95 REINSTRUMENTED CASES TO SELECT LANGUAGE SEMANTICS
 -- PWN 05/03/96 Enforced Ada 95 sliding rules
 -- PWN 10/24/96 Adjusted expected results for Ada 95.
 -- TMB 11/19/96 BACKED OUT CHANGE FOR SLIDING WITH ACCESS TYPES
 -- MRM 12/16/96 Removed problem code from withdrawn version of test, and
 --              implemented a dereference-index check to ensure Ada95
 --              required behavior.
 -- PWB.CTA 03/07/97 Restored checks from 1.11 in 2 cases where sliding does
 --                  not occur 
 WITH REPORT;
 
 PROCEDURE  C48009E  IS
 
      USE REPORT ;
 
 BEGIN
 
      TEST("C48009E","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                     "THAT CONSTRAINT_ERROR IS RAISED WHEN "          &
                     "APPROPRIATE - CONSTRAINED ARRAY TYPES");
      DECLARE
 
           TYPE UA IS ARRAY(INTEGER RANGE <>) OF INTEGER;
           TYPE CA3_2 IS ARRAY(3 .. 2) OF INTEGER;
           TYPE SA1_3 IS ARRAY(1 .. 3) OF INTEGER;
           TYPE NA1_3 IS ARRAY(1 .. IDENT_INT(3)) OF INTEGER;
           SUBTYPE CA2_6 IS UA(2 .. 6);
           SUBTYPE CA1_4 IS UA(1 .. 4);
           SUBTYPE CA1_6 IS UA(1 .. 6);
           SUBTYPE CA4_1 IS UA(4 .. 1);
           SUBTYPE CA4_2 IS UA(4 .. 2);
 
           TYPE A_CA3_2 IS ACCESS CA3_2;
           TYPE A_SA1_3 IS ACCESS SA1_3;
           TYPE A_NA1_3 IS ACCESS NA1_3;
           TYPE A_CA1_5 IS ACCESS UA(1 .. 5);
           TYPE A_CA4_2 IS ACCESS CA4_2;
 
           V_A_CA3_2 : A_CA3_2;
           V_A_SA1_3 : A_SA1_3;
           V_A_NA1_3 : A_NA1_3;
           V_A_CA1_5 : A_CA1_5;
 
           FUNCTION ALLOC1(X : CA2_6) RETURN A_CA1_5 IS
           BEGIN
                IF EQUAL(1, 1) THEN
                     RETURN NEW CA2_6'(X);
                ELSE
                     RETURN NULL;
                END IF;
           END ALLOC1;
           FUNCTION ALLOC2(X : CA4_1) RETURN A_CA4_2 IS
           BEGIN
                IF EQUAL(1, 1) THEN
                     RETURN NEW CA4_1'(X);
                ELSE
                     RETURN NULL;
                END IF;
           END ALLOC2;
 
      BEGIN
 
          BEGIN
               V_A_CA3_2 := NEW CA3_2'(IDENT_INT(4) .. IDENT_INT(2)
                                       => 5);
               FAILED ("NO EXCEPTION RAISED - CASE 1A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 1A");
          END;
 
           BEGIN
                V_A_NA1_3 := NEW NA1_3'(1 .. IDENT_INT(2) => 4);
                FAILED ("NO EXCEPTION RAISED - CASE 1B");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>
                     NULL;
                WHEN OTHERS =>
                     FAILED ("WRONG EXCEPTION RAISED - CASE 1B");
           END;
 
           BEGIN
                -- note that ALLOC1 returns A_CA1_5, so both
                -- (1) and (5) are valid index references!
                IF ALLOC1((2 .. 6 => 2))(5) /= 2 THEN
                     FAILED ("Wrong Value Returned - CASE 2A");
                ELSIF ALLOC1((2 .. 6 => 3))(1) /= 3 THEN
                     FAILED ("Unlikely Index Case - CASE 2A");
                END IF;
           EXCEPTION
                WHEN OTHERS =>
                     FAILED ("EXCEPTION RAISED - CASE 2A");
           END;
 
           BEGIN
                IF ALLOC2((4 .. 1 => 3)) = NULL THEN
                     FAILED ("IMPOSSIBLE - CASE 2B");
                END IF;
                COMMENT ("ADA 95 SLIDING ASSIGNMENT");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>
                     FAILED ("ADA 83 NON-SLIDING ASSIGNMENT");
                WHEN OTHERS =>
                     FAILED ("WRONG EXCEPTION RAISED - CASE 2B");
           END;
 
           BEGIN
                V_A_SA1_3 := NEW SA1_3'(1, 2);
                FAILED ("NO EXCEPTION RAISED - CASE 3A");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>  
                     NULL;
                WHEN OTHERS =>  
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3A");
           END;
 
           BEGIN
                V_A_SA1_3 := NEW SA1_3'(3, 4, 5, 6);
                FAILED ("NO EXCEPTION RAISED - CASE 3B");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>  
                     NULL;
                WHEN OTHERS =>  
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3B");
           END;
 
           BEGIN
                V_A_NA1_3 := NEW NA1_3'(1, 2);
                FAILED ("NO EXCEPTION RAISED - CASE 3C");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>  
                     NULL;
                WHEN OTHERS =>  
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3C");
           END;
 
           BEGIN -- SATISFIES T BUT NOT BASE TYPE.
                V_A_CA1_5 := NEW CA1_4'(1, 2, 3, 4);
                FAILED ("NO EXCEPTION RAISED - CASE 3D");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>  
                     NULL;
                WHEN OTHERS =>  
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3D");
           END;
 
           BEGIN -- SATISFIES T BUT NOT BASE TYPE.
                V_A_CA1_5 := NEW CA1_6'(1, 2, 3, 4, 5, 6);
                FAILED ("NO EXCEPTION RAISED - CASE 3E");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>
                     NULL;
                WHEN OTHERS =>
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3E");
           END;
 
           BEGIN -- SATISFIES BASE TYPE BUT NOT T.
                V_A_CA1_5 := NEW CA1_4'(1, 2, 3, 4, 5);
                FAILED ("NO EXCEPTION RAISED - CASE 3F");
           EXCEPTION
                WHEN CONSTRAINT_ERROR =>
                     NULL;
                WHEN OTHERS =>
                     FAILED ("WRONG EXCEPTION RAISED - CASE 3F");
           END;
 
          BEGIN -- SATISFIES BASE TYPE BUT NOT T.
               V_A_CA1_5 := NEW CA1_6'(1, 2, 3, 4, 5);
               FAILED ("NO EXCEPTION RAISED - CASE 3G");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 3G");
          END;
 
      END ;
 
      RESULT ;
 
 END C48009E ;
 
