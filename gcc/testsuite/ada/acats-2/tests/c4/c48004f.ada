-- C48004F.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS AN ACCESS TYPE.

-- RM  01/12/80
-- JBG 03/03/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE C48004F  IS

     USE REPORT;

BEGIN

     TEST("C48004F","CHECK THAT THE FORM 'NEW T' IS PERMITTED IF T " &
                    "IS AN ACCESS TYPE");

     DECLARE

          TYPE AINT IS ACCESS INTEGER;
          TYPE A_AINT IS ACCESS AINT;
          VA_AINT : A_AINT;

          TYPE AST IS ACCESS STRING;
          SUBTYPE CAST_4 IS AST(1 .. 4);
          TYPE A_AST IS ACCESS AST;
          TYPE ACAST_3 IS ACCESS AST(1 .. 3);
          V_AAST : A_AST;
          V_ACAST_3 : ACAST_3;

          TYPE UR(A, B : INTEGER) IS
               RECORD
                    C : INTEGER;
               END RECORD;
          SUBTYPE CR IS UR(1, 2);
          TYPE A_CR IS ACCESS CR;
          TYPE AA_CR IS ACCESS A_CR;
          V_AA_CR : AA_CR;

     BEGIN

          VA_AINT := NEW AINT;
          IF VA_AINT.ALL /= NULL THEN
               FAILED ("VARIABLE IS NOT NULL - CASE 1");
          END IF;

          BEGIN

               V_ACAST_3 := NEW CAST_4;
               IF V_ACAST_3.ALL /= NULL THEN
                    FAILED ("VARIABLE IS NOT NULL - CASE 2");
               END IF;

          EXCEPTION

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - CASE 2");

          END;

          V_AAST := NEW AST;
          IF V_AAST.ALL /= NULL THEN
               FAILED ("VARIABLE IS NOT NULL - CASE 3");
          END IF;

          V_AA_CR := NEW A_CR;
          IF V_AA_CR.ALL /= NULL THEN
               FAILED ("VARIABLE IS NOT NULL - CASE 4");
          END IF;

     END;

     RESULT;

END C48004F;
