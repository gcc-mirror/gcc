-- C48009C.ADA

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
-- IS RAISED IF T IS A CONSTRAINED RECORD OR PRIVATE TYPE, (X) IS AN
-- AGGREGATE OR A VALUE OF TYPE T, AND ONE OF THE DISCRIMINANT VALUES IN
-- X:
--   1) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE FOR T.
--   2) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE SPECIFIED
--      IN THE DECLARATION OF THE ALLOCATOR'S BASE TYPE.
--   3) DOES NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE IN THE
--      ACCESS TO ACCESS CASE.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- EG  07/05/84

WITH REPORT;

PROCEDURE C48009C IS

     USE REPORT;

BEGIN

     TEST("C48009C","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN "          &
                    "APPROPRIATE - CONSTRAINED RECORD TYPES");

     DECLARE

          TYPE TC0(A, B : INTEGER) IS
               RECORD
                    C : INTEGER RANGE 1 .. 7;
               END RECORD;
          SUBTYPE TC IS TC0(2, 3);
          TYPE ATC IS ACCESS TC0(2, 3);
          SUBTYPE TC4_5 IS TC0(IDENT_INT(4), IDENT_INT(5));
          VC : ATC;

     BEGIN

          BEGIN
               VC := NEW TC'(102, 3, 4);
               FAILED ("NO EXCEPTION RAISED - CASE 1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           =>  
                    FAILED("WRONG EXCEPTION RAISED - CASE 1");
          END;

          BEGIN
               VC := NEW TC4_5'(IDENT_INT(4), IDENT_INT(5), 1);
               FAILED ("NO EXCEPTION RAISED - CASE 2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           =>  
                    FAILED("WRONG EXCEPTION RAISED - CASE 2");
          END;

     END;

     DECLARE

          TYPE UR(A : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE A_UR IS ACCESS UR;
          SUBTYPE CA_UR IS A_UR(2);
          TYPE A_CA_UR IS ACCESS CA_UR;

          V : A_CA_UR;

     BEGIN

          V := NEW CA_UR'(NEW UR'(A => IDENT_INT(3)));
          FAILED ("NO EXCEPTION RAISED - CASE 3");

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - CASE 3");

     END;

     RESULT;

END C48009C;
