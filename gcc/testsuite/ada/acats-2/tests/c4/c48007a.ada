-- C48007A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS AN UNCONSTRAINED TYPE WITH DEFAULT DISCRIMINANTS
-- (RECORD, PRIVATE OR LIMITED) AND ONE DEFAULT DISCRIMINANT VALUE DOES
-- NOT EQUAL THE CORRESPONDING VALUE SPECIFIED FOR THE ALLOCATOR'S BASE
-- TYPE.

-- EG  08/10/84

WITH REPORT;

PROCEDURE C48007A IS

     USE REPORT;

BEGIN

     TEST("C48007A","FOR ALLOCATORS OF THE FORM 'NEW T' CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN "     &
                    "APPROPRIATE - UNCONSTRAINED TYPE WITH "    &
                    "DEFAULT DISCRIMINANTS");

     DECLARE

          TYPE UR(A : INTEGER := 1; B : INTEGER := 2) IS
               RECORD
                    C : INTEGER := 7;
               END RECORD;

          PACKAGE P IS

               TYPE UP(A : INTEGER := 12; B : INTEGER := 13) IS
                    PRIVATE;
               TYPE UL(A, B : INTEGER := 4) IS LIMITED PRIVATE;

          PRIVATE

               TYPE UP(A : INTEGER := 12; B : INTEGER := 13) IS
                    RECORD
                         C : INTEGER := 8;
                    END RECORD;
               TYPE UL(A, B : INTEGER := 4) IS
                    RECORD
                         C : INTEGER := 9;
                    END RECORD;

          END P;

          USE P;

          TYPE A_UR IS ACCESS UR(1, 9);
          TYPE A_UP IS ACCESS UP(9, 13);
          TYPE A_UL IS ACCESS UL(4, 9);

          VUR : A_UR;
          VUP : A_UP;
          VUL : A_UL;

     BEGIN

          BEGIN -- UR

               VUR := NEW UR;
               FAILED("NO EXCEPTION RAISED - UR");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - UR");

          END;

          BEGIN -- UP

               VUP := NEW UP;
               FAILED("NO EXCEPTION RAISED - UP");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - UP");

          END;

          BEGIN -- UL

               VUL := NEW UL;
               FAILED("NO EXCEPTION RAISED - UL");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - UL");

          END;

     END;

     RESULT;

END C48007A;
