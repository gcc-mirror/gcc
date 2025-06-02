-- C48005A.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T X" ALLOCATES A NEW OBJECT
-- EACH TIME IT IS EXECUTED AND THAT IF T IS AN UNCONSTRAINED RECORD,
-- PRIVATE, OR LIMITED TYPE, THE ALLOCATED OBJECT HAS THE DISCRIMINANT
-- VALUES SPECIFIED BY X.

-- EG  08/08/84

WITH REPORT;

PROCEDURE C48005A IS

     USE REPORT;

BEGIN

     TEST("C48005A","CHECK THAT THE FORM 'NEW T X' ALLOCATES A " &
                    "NEW OBJECT AND THAT IF T IS AN UNCONSTRAINED " &
                    "RECORD, PRIVATE, OR LIMITED TYPE, THE " &
                    "ALLOCATED OBJECT HAS THE DISCRIMINANT " &
                    "VALUES SPECIFIED BY X");

     DECLARE

          TYPE UR1(A : INTEGER) IS
               RECORD
                    B : INTEGER := 7;
                    C : INTEGER := 4;
               END RECORD;
          TYPE UR2(A : INTEGER) IS
               RECORD
                    CASE A IS
                         WHEN 1 =>
                              A1 : INTEGER := 4;
                         WHEN 2 =>
                              A2 : INTEGER := 5;
                         WHEN OTHERS =>
                              NULL;
                    END CASE;
               END RECORD;

          TYPE A_UR1 IS ACCESS UR1;
          TYPE A_UR2 IS ACCESS UR2;

          V1AUR1 : A_UR1;
          V1AUR2, V2AUR2 : A_UR2;

          TYPE REC (A : INTEGER) IS
               RECORD
                    B : INTEGER;
               END RECORD;

          TYPE A_REC IS ACCESS REC;

          V_A_REC : A_REC;

          TYPE ARR IS ARRAY(1 .. 1) OF INTEGER;

          TYPE RECVAL IS
               RECORD
                    A : INTEGER;
                    B : ARR;
               END RECORD;

          FUNCTION FUN (A : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT(A);
          END FUN;
          FUNCTION FUN (A : INTEGER) RETURN RECVAL IS
          BEGIN
               FAILED ("WRONG OVERLOADED FUNCTION CALLED");
               RETURN (1, (1 => 2));
          END FUN;

     BEGIN

          V1AUR1 := NEW UR1(3);
          IF ( V1AUR1.A /= 3 OR V1AUR1.B /= 7 OR
               V1AUR1.C /= IDENT_INT(4) ) THEN
               FAILED("WRONG VALUES - V1UAR1");
          END IF;

          V1AUR2 := NEW UR2(IDENT_INT(2));
          IF ( V1AUR2.A /= 2 OR V1AUR2.A2 /= IDENT_INT(5) ) THEN
               FAILED("WRONG VALUES - V1AUR2");
          END IF;

          V2AUR2 := NEW UR2(IDENT_INT(3));
          IF ( V2AUR2.A /= IDENT_INT(3) ) THEN
               FAILED("WRONG VALUES - V2AUR2");
          END IF;

          V_A_REC := NEW REC(FUN(2));
     END;

     RESULT;

END C48005A;
