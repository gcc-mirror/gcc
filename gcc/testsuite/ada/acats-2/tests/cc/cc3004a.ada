-- CC3004A.ADA

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
-- CHECK THAT ACTUAL PARAMETERS IN A NAMED GENERIC ACTUAL PARAMETER 
-- ASSOCIATION MAY BE OUT OF ORDER, AND ARE ASSOCIATED WITH THE 
-- CORRECT FORMALS.

-- DAT 9/16/81 
-- SPS 10/26/82

WITH REPORT; USE REPORT;

PROCEDURE CC3004A IS 
BEGIN
     TEST ("CC3004A", "ORDER OF NAMED GENERIC ACTUAL PARAMETERS");

     DECLARE
          GENERIC
               A,B : INTEGER;
               C : INTEGER;
               D : INTEGER;
          PACKAGE P1 IS END P1;

          TYPE AI IS ACCESS INTEGER;

          GENERIC
               TYPE D IS ( <> );
               VD : D;
               TYPE AD IS ACCESS D;
               VA : AD;
          PACKAGE P2 IS END P2;

          X : AI := NEW INTEGER '(IDENT_INT(23));
          Y : AI := NEW INTEGER '(IDENT_INT(77));

          PACKAGE BODY P1 IS
          BEGIN
               IF A /= IDENT_INT(4)  OR
                  B /= IDENT_INT(12) OR
                  C /= IDENT_INT(11) OR
                  D /= IDENT_INT(-33)
               THEN
                    FAILED ("WRONG GENERIC PARAMETER ASSOCIATIONS");
               END IF;
          END P1;

          PACKAGE BODY P2 IS
          BEGIN
               IF VA.ALL /= VD THEN 
                    FAILED ("WRONG GENERIC PARM ASSOCIATIONS 2");
               END IF;
          END P2;

          PACKAGE N1 IS NEW P1 (C => 11, A => 4, D => -33, B => 12);

          PACKAGE N2 IS NEW P2 (VA => X, AD => AI, D => INTEGER, 
                                VD => 23);

          PACKAGE N3 IS NEW P2 (INTEGER, 77, VA => Y, AD => AI);

     BEGIN
          NULL;
     END;

     RESULT;
END CC3004A;
