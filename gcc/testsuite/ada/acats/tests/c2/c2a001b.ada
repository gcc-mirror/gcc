-- C2A001B.ADA

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
-- CHECK THAT NON-CONSECUTIVE UNDERSCORES ARE PERMITTED
-- IN EVERY PART OF A BASED FLOATING POINT LITERAL THAT
-- USES COLONS INSTEAD OF SHARPS.

-- DCB 04/22/80
-- JRK 10/27/80
-- JBG 5/28/85

WITH REPORT;
PROCEDURE C2A001B IS

     USE REPORT;

     F1, F2, F3, F4, F5 : FLOAT;

BEGIN
     TEST("C2A001B", "UNDERSCORES ALLOWED IN BASED FLOATING POINT " &
                     "LITERALS THAT HAVE COLONS");

     F1 := 1.2_5E1;
     F2 := 1_6:1.A:;
     F3 := 8:1_3.5:;
     F4 := 8:2.3_7:;
     F5 := 8:3.4:E1_1;

     IF F1 = 1.25E1 AND F2 = 16:1.A: AND F3 = 8:13.5: AND
        F4 = 8:2.37: AND F5 = 8:3.4:E11 THEN
          NULL;
     ELSE 
          FAILED("UNDERSCORES IN FLOATING POINT LITERALS NOT " &
                 "HANDLED CORRECTLY");
     END IF;

     RESULT;
END C2A001B;
