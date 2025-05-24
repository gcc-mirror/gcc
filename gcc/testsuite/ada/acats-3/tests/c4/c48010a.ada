-- C48010A.ADA

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
-- CHECK THAT NULL ARRAYS AND NULL RECORDS CAN BE ALLOCATED.

-- EG  08/30/84

WITH REPORT;

PROCEDURE C48010A IS

     USE REPORT;

BEGIN

     TEST("C48010A","CHECK THAT NULL ARRAYS AND NULL RECORDS CAN " &
                    "BE ALLOCATED");

     DECLARE

          TYPE CA IS ARRAY(4 .. 3) OF INTEGER;
          TYPE CR IS
               RECORD
                    NULL;
               END RECORD;

          TYPE A_CA IS ACCESS CA;
          TYPE A_CR IS ACCESS CR;

          TYPE AA_CA IS ACCESS A_CA;
          TYPE AA_CR IS ACCESS A_CR;

          V_A_CA  : A_CA;
          V_A_CR  : A_CR;
          V_AA_CA : AA_CA;
          V_AA_CR : AA_CR;

     BEGIN

          V_A_CA := NEW CA;
          IF V_A_CA = NULL THEN
               FAILED ("NULL ARRAY WAS NOT ALLOCATED - CA");
          ELSIF V_A_CA.ALL'FIRST /= 4 AND V_A_CA.ALL'LAST /= 3 THEN
               FAILED ("NULL ARRAY BOUNDS ARE INCORRECT - CA");
          END IF;

          V_A_CR := NEW CR;
          IF V_A_CR = NULL THEN
               FAILED ("NULL RECORD WAS NOT ALLOCATED - CR");
          END IF;

          V_AA_CA := NEW A_CA'(NEW CA);
          IF V_AA_CA.ALL = NULL THEN
               FAILED ("NULL ARRAY WAS NOT ALLOCATED - A_CA");
          ELSIF V_AA_CA.ALL.ALL'FIRST /= 4 AND 
                V_AA_CA.ALL.ALL'LAST /= 3 THEN
               FAILED ("NULL ARRAY BOUNDS ARE INCORRECT - A_CA");
          END IF;

          V_AA_CR := NEW A_CR'(NEW CR);
          IF (V_AA_CR = NULL OR V_AA_CR.ALL = NULL) THEN
               FAILED ("NULL RECORD WAS NOT ALLOCATED - A_CR");
          END IF;

     END;

     RESULT;

END C48010A;
