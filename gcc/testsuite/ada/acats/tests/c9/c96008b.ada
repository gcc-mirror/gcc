-- C96008B.ADA

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
-- MISCELLANEOUS CHECKS ON THE PRE-DEFINED FUNCTIONS IN THE PACKAGE
-- CALENDAR.  SUBTESTS ARE:
--   (A) THE FUNCTION TIME_OF() MUST ADVANCE DAY WHEN CALLED WITH THE
--       SECONDS ARGUMENT HAVING THE VALUE 86_400.

-- CPP 8/16/84
-- JRK 12/4/84

WITH CALENDAR;  USE CALENDAR;
WITH REPORT;  USE REPORT;
PROCEDURE C96008B IS

     NOW1, NOW2 : TIME;
     YR : YEAR_NUMBER;
     MO : MONTH_NUMBER;
     DY : DAY_NUMBER;
     SEC : DAY_DURATION;

BEGIN

     TEST ("C96008B", "CHECK THAT TIME_OF() ADVANCES DAY");

     NOW1 := TIME_OF (1984, 8, 13, 86_400.0);
     NOW2 := TIME_OF (1984, 8, 14, 0.0);

     IF NOW1 /= NOW2 THEN
          FAILED ("TIME_OF DID NOT CONVERT 86_400 SECONDS TO A DAY");
     END IF;

     SPLIT (NOW2, YR, MO, DY, SEC);

     IF DY /= 14 THEN
          FAILED ("DAY OF NOW2 INCORRECT");
     END IF;
     IF SEC /= 0.0 THEN
          FAILED ("SECONDS OF NOW2 INCORRECT");
     END IF;

     SPLIT (NOW1, YR, MO, DY, SEC);

     IF DY /= 14 OR SEC /= 0.0 OR
        DAY (NOW1) /= 14 OR SECONDS (NOW1) /= 0.0 THEN
          FAILED ("TIME_OF DID NOT ADVANCE DAY");
     END IF;

     RESULT;
END C96008B;
