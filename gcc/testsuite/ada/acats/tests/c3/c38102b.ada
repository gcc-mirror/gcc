-- C38102B.ADA

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
-- CHECK THAT INCOMPLETE TYPES CAN BE FLOAT.

-- DAT 3/24/81
-- SPS 10/25/82

WITH REPORT; USE REPORT;

PROCEDURE C38102B IS

BEGIN
     TEST ("C38102B", "INCOMPLETE TYPE CAN BE FLOAT");
     
     DECLARE

          TYPE F;
          TYPE G;
          TYPE AF IS ACCESS F;
          TYPE F IS DIGITS 2;
          TYPE G IS NEW F RANGE 1.0 .. 1.5;
          TYPE AG IS ACCESS G RANGE 1.0 .. 1.3;
     
          XF : AF := NEW F' (2.0);
          XG : AG := NEW G' (G (XF.ALL/2.0));

     BEGIN
          IF XG.ALL NOT IN G THEN
               FAILED ("ACCESS TO FLOAT");
          END IF;
     END;
     
     RESULT;
END C38102B;
