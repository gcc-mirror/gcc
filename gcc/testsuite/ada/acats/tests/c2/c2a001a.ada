-- C2A001A.ADA

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
-- IN EVERY PART OF A BASED INTEGER LITERAL WHEN SHARPS
-- ARE USED INSTEAD OF COLONS.

-- INTEGER LITERALS.

-- DCB 1/24/80
-- JRK 10/27/80
-- JBG 5/28/85

WITH REPORT;
PROCEDURE C2A001A IS

     USE REPORT;

     I1, I2, I3, I4 : INTEGER;

BEGIN
     TEST("C2A001A", "UNDERSCORES ALLOWED IN BASED INTEGER LITERALS " &
                     "THAT HAVE COLONS");

     I1 := 12_3;
     I2 := 1_6:D:;
     I3 := 2:1011_0101:;
     I4 := 16:D:E0_1;

     IF I1 = 123 AND I2 = 16:D: AND I3 = 2:10110101: AND
        I4 = 16:D:E01 THEN
          NULL;
     ELSE 
          FAILED("UNDERSCORES IN INTEGER LITERALS NOT HANDLED " &
                 "CORRECTLY");
     END IF;

     RESULT;
END C2A001A;
