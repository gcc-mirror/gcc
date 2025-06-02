-- C52005A.ADA

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
-- CHECK THAT THE CONSTRAINT_ERROR EXCEPTION IS RAISED WHEN A STATIC
--    EXPRESSION VALUE IS OUTSIDE THE STATIC RANGE OF INTEGER, BOOLEAN,
--    CHARACTER, AND ENUMERATION ASSIGNMENT TARGET VARIABLES.

-- DCB 2/5/80
-- JRK 7/21/80
-- SPS 3/21/83

WITH REPORT;
PROCEDURE C52005A IS

     USE REPORT;

BEGIN
     TEST ("C52005A", "CHECK THAT CONSTRAINT_ERROR EXCEPTION IS RAISED "
         & "ON STATIC OUT OF RANGE INTEGER, BOOLEAN, CHARACTER, " &
           "AND ENUMERATION ASSIGNMENTS");

-------------------------

     DECLARE
          I1 : INTEGER RANGE 0..10 := 5;

     BEGIN
          I1 := 11;

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE INT ASSNMT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF I1 /= 5 THEN
               FAILED ("VALUE ALTERED BEFORE INT RANGE" &
                       "EXCEPTION");
          END IF;

     END;

-------------------------

     DECLARE
          I2 : INTEGER RANGE 0..10 := 5;

     BEGIN
          I2 := 10;

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED ON LEGAL INTEGER ASSIGNMENT");
     END;

-------------------------

     DECLARE
          B1 : BOOLEAN RANGE TRUE..TRUE := TRUE;

     BEGIN
          B1 := FALSE;

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE BOOL ASSNMT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF B1 /= TRUE THEN
               FAILED ("VALUE ALTERED BEFORE BOOLEAN RANGE EXCEPTION");
          END IF;
     END;

-------------------------

     DECLARE
          B2 : BOOLEAN := TRUE;

     BEGIN
          B2 := FALSE;

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED ON LEGAL BOOLEAN ASSNMNT");

     END;

-------------------------

     DECLARE
          C1 : CHARACTER RANGE 'B'..'Z' := 'M';

     BEGIN
          C1 := 'A';

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE CHAR ASSNMNT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF C1 /= 'M' THEN
               FAILED ("VALUE ALTERED BEFORE CHARACTER RANGE " &
                       "EXCEPTION");
          END IF; 

     END;

-------------------------

     DECLARE
          C2 : CHARACTER RANGE 'B'..'Z' := 'M';

     BEGIN
          C2 := 'B';

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED OF LEGAL CHARACTER ASSNMNT");

     END;

-------------------------

     DECLARE
          TYPE DAY IS (SUN, MON, TUE, WED, THU, FRI, SAT);
          WORKDAY : DAY RANGE MON..FRI := TUE;

     BEGIN
          WORKDAY := SUN;

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE ENUM. " &
                  "ASSIGNMENT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF WORKDAY /= TUE THEN
               FAILED ("VALUE ALTERED BEFORE ENUM. RANGE EXCEPTION");
          END IF;

     END;

-------------------------

     DECLARE
          TYPE DAY IS (SUN, MON, TUE, WED, THU, FRI, SAT);
          WORKDAY : DAY RANGE MON..FRI := TUE;

     BEGIN
          WORKDAY := FRI;

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED ON LEGAL ENUM. ASSNMNT");

     END;

-------------------------

     RESULT;
END C52005A;
