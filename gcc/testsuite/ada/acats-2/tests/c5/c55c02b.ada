-- C55C02B.ADA

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
-- CHECK THAT THE WHILE CONDITION IS EVALUATED EACH TIME.

-- DAT 1/29/81
-- SPS 3/2/83

WITH REPORT;
PROCEDURE C55C02B IS

     USE REPORT;

     I : INTEGER := 0;

     FT : ARRAY (FALSE .. TRUE) OF BOOLEAN 
          := (IDENT_BOOL (FALSE), IDENT_BOOL (TRUE));

BEGIN
     TEST ("C55C02B", "WHILE CONDITION IS EVALUATED EACH TIME THROUGH");

     WHILE I /= 10 LOOP
          I := I + 1;
     END LOOP;
     IF I /= 10 THEN
          FAILED ("BAD LOOP FLOW - OPTIMIZABLE CONDITION");
     END IF;

     I := 10;
     WHILE FT (IDENT_BOOL (I /= 14)) LOOP
          I := I + 1;
     END LOOP;
     IF I /= 14 THEN
          FAILED ("BAD LOOP FLOW - DYNAMIC CONDITION");
     END IF;

     RESULT;
END C55C02B;
