-- C24211A.ADA

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
-- OBJECTIVE:
--     CHECK THAT LEGAL FORMS INVOLVING A DIGIT FOLLOWED BY A COLON ARE
--     CORRECTLY ANALYZED USING A TWO CHARACTER LOOK-AHEAD.

-- HISTORY:
--     DHH 01/19/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C24211A IS

     TYPE FIXED IS DELTA 0.0125 RANGE -1.0 .. 100.0;

     A : INTEGER RANGE 0 .. 2:10::= 1;
     B : INTEGER RANGE 0 .. 2#10#:= 1;
     X : FIXED RANGE 0.0 .. 16:3.0::= 1.0;
     Y : FIXED RANGE 0.0 .. 16#3.0#:= 1.0;
     IN2 : INTEGER;
     BOOL : BOOLEAN:=3:10:=3:10:;

BEGIN

     TEST("C24211A", "CHECK THAT LEGAL FORMS INVOLVING A DIGIT " &
                     "FOLLOWED BY A COLON ARE CORRECTLY ANALYZED " &
                     "USING A TWO CHARACTER LOOK-AHEAD");

     IF IDENT_INT(A) /= B THEN
          FAILED("CALCULATIONS OF BASED INTEGER LITERALS WHEN " &
                 "REPRESENTED BY SHARPS DO NOT MATCH CALCULATIONS " &
                 "OF BASED INTEGER LITERALS REPRESENTED BY COLONS");
     END IF;
     A := A + 1;


     IF EQUAL(3,3) THEN
          Y := X + Y;
     ELSE
          Y := X - Y;
     END IF;

     IF (2 * X) = Y THEN
          NULL;
     ELSE
          FAILED("CALCULATIONS OF BASED REAL LITERALS WHEN " &
                 "REPRESENTED BY SHARPS DO NOT MATCH CALCULATIONS " &
                 "OF BASED REAL LITERALS REPRESENTED BY COLONS");
     END IF;
     IF NOT BOOL THEN
          FAILED("BOOLEAN VALUE BASED ON REAL LITERAL WAS CALCULATED " &
                 "INCORRECTLY");
          IN2:=2:10:;
     ELSE
          BOOL := FALSE;
          IN2:=3:10:;
     END IF;
     IF BOOL THEN
          A := A + 1;
     ELSE
          A := A - 1;
     END IF;

     RESULT;
END C24211A;
