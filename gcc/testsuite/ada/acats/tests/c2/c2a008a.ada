-- C2A008A.ADA

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
-- CHECK THAT UPPER AND LOWER CASE "E" MAY APPEAR IN BASED LITERALS,
-- WHEN USING COLONS IN PLACE OF THE SHARP SIGN.

-- TBN 2/28/86

WITH REPORT; USE REPORT;
PROCEDURE C2A008A IS

     TYPE FLOAT IS DIGITS 5;
     INT_1 : INTEGER := 15:A:E1;
     INT_2 : INTEGER := 15:A:e1;
     FLO_1 : FLOAT := 16:FD.C:E1;
     FLO_2 : FLOAT := 16:FD.C:e1;

BEGIN
     TEST("C2A008A", "CHECK THAT UPPER AND LOWER CASE ""E"" MAY " &
                     "APPEAR IN BASED LITERALS, WHEN USING COLONS " &
                     "IN PLACE OF THE SHARP SIGN");

     IF INT_1 /= INT_2 THEN
          FAILED ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 1");
     END IF;

     IF FLO_1 /= FLO_2 THEN
          FAILED ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 2");
     END IF;

     INT_1 := 14:BC:E1;
     INT_2 := 14:BC:e1;
     FLO_1 := 16:DEF.AB:E0;
     FLO_2 := 16:DEF.AB:e0;

     IF INT_1 /= INT_2 THEN
          FAILED ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 3");
     END IF;

     IF FLO_1 /= FLO_2 THEN
          FAILED ("UPPER AND LOWER CASE LETTERS NOT ALLOWED - 4");
     END IF;
     
     RESULT;
END C2A008A;
