-- C24202D.ADA

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
-- IN EVERY PART OF BASED INTEGER, FLOATING POINT, AND FIXED POINT LITERALS.

-- WMC 03/16/92  CONSOLIDATION OF C24202A.ADA, C24202B.ADA, C24202C.ADA

WITH REPORT;

PROCEDURE C24202D IS

     USE REPORT;

     TYPE FIXED1 IS DELTA 2.0**(-6) RANGE 0.0 .. 10.0;

     I1, I2     : INTEGER;
     F1, F2, F3 : FLOAT;
     F4, F5     : FIXED1;

BEGIN
     TEST("C24202D", "UNDERSCORES ALLOWED IN NUMERIC LITERALS");

     I1 := 12_3;
     I2 := 16#D#E0_1;

     IF (I1 /= 123) OR (I2 /= 16#D#E01) THEN
          FAILED("UNDERSCORES IN INTEGER LITERALS NOT HANDLED CORRECTLY");
     END IF;


     F1 := 1.2_5E1;
     F2 := 8#1_3.5#;
     F3 := 8#3.4#E1_1;

     IF (F1 /= 1.25E1) OR (F2 /= 8#13.5#) OR (F3 /= 8#3.4#E11) THEN
          FAILED("UNDERSCORES IN FLOATING POINT LITERALS NOT " &
                 "HANDLED CORRECTLY");
     END IF;


     F4 := 1_6#1.A#;
     F5 := 8#2.3_7#;

     IF (F4 /= 16#1.A#) OR (F5 /= 8#2.37#) THEN
          FAILED("UNDERSCORES IN FIXED POINT LITERALS NOT " &
                 "HANDLED CORRECTLY");
     END IF;

     RESULT;

END C24202D;
