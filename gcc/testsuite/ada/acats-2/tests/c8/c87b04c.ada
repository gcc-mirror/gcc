-- C87B04C.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- IN A RANGE CONSTRAINT OF A SUBTYPE INDICATION, THE EXPRESSIONS
-- FOR THE LOWER AND UPPER BOUNDS MUST BE COMPATIBLE WITH THE SUBTYPE'S
-- EXPLICIT TYPEMARK.
  
-- TRH  29 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B04C IS
  
     TYPE DAY IS (MON, TUE, WED, THU, FRI, SAT, SUN);
     TYPE ORB IS (SUN, MOON, MARS, EARTH);
  
     TYPE GRADE IS ('A', 'B', 'C', 'D', 'F');
     TYPE VOWEL IS ('C', 'E', 'A', 'O', 'I', 'U', 'Y');
  
BEGIN
     TEST ("C87B04C","OVERLOADED EXPRESSIONS IN RANGE CONSTRAINTS" &
          " OF ENUMERATION SUBTYPE INDICATIONS");
  
     DECLARE
          SUBTYPE PASSING IS GRADE RANGE 'A' .. 'C';
          SUBTYPE DISTANT IS ORB   RANGE SUN .. MARS;
  
     BEGIN
          IF DISTANT'POS (DISTANT'FIRST) /= 0 OR
             PASSING'POS (PASSING'FIRST) /= 0 THEN 
             FAILED ("RESOLUTION INCORRECT FOR OVERLOADED " &
                     " ENUMERATION LITERALS");
          END IF;
     END;
    
     RESULT;
END C87B04C;
