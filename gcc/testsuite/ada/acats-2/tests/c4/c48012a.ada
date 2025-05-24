-- C48012A.ADA

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
-- CHECK THAT DISCRIMINANTS GOVERNING VARIANT PARTS NEED NOT BE
-- SPECIFIED WITH STATIC VALUES IN AN ALLOCATOR OF THE FORM 
-- "NEW T X".

-- EG  08/30/84

WITH REPORT;

PROCEDURE C48012A IS

     USE REPORT;

BEGIN

     TEST("C48012A","CHECK THAT DISCRIMINANTS GOVERNING VARIANT " &
                    "PARTS NEED NOT BE SPECIFIED WITH STATIC "    &
                    "VALUES IN AN ALLOCATOR OF THE FORM 'NEW T X'");

     DECLARE

          TYPE INT IS RANGE 1 .. 5;
          TYPE ARR IS ARRAY(INT RANGE <>) OF INTEGER;

          TYPE UR(A : INT) IS
               RECORD
                    CASE A IS
                         WHEN 1 =>
                              NULL;
                         WHEN OTHERS =>
                              B : ARR(1 .. A);
                    END CASE;
               END RECORD;

          TYPE A_UR IS ACCESS UR;

          V_A_UR : A_UR;

     BEGIN

          V_A_UR := NEW UR(A => INT(IDENT_INT(2)));
          IF V_A_UR.A /= 2 THEN
               FAILED ("WRONG DISCRIMINANT VALUE");
          ELSIF V_A_UR.B'FIRST /= 1 AND V_A_UR.B'LAST /= 2 THEN
               FAILED ("WRONG BOUNDS IN VARIANT PART");
          END IF;

     END;

     RESULT;

END C48012A;
