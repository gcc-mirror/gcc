-- C87B14C.ADA

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
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, THE LOWER AND UPPER
-- BOUNDS MUST BE OF THE INDEX BASE TYPE.
--
-- TEST (C): INDEX CONSTRAINTS WITH OVERLOADED INFIX OPERATORS.
  
-- TRH  30 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B14C IS
     
     TYPE DAY  IS (MON, TUE, WED, THU, FRI, SAT, SUN);
     TYPE LIST IS ARRAY (DAY RANGE <>) OF BOOLEAN;
     TYPE GRID IS ARRAY (DAY RANGE <>, DAY RANGE <>) OF BOOLEAN;
     SUBTYPE WEEKEND IS DAY RANGE SAT .. SUN;
     SUBTYPE WEEKDAY IS DAY RANGE MON .. FRI;
   
     FUNCTION "*" (X, Y : INTEGER) RETURN WEEKDAY IS
     BEGIN 
          RETURN MON;
     END "*";

     FUNCTION "*" (X, Y : INTEGER) RETURN BOOLEAN IS
     BEGIN 
          FAILED ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
                  " IN SUBTYPE INDICATIONS");
          RETURN TRUE;
     END "*";
  
     FUNCTION "+" (X, Y : INTEGER) RETURN WEEKEND IS
     BEGIN 
          RETURN SAT;
     END "+";

     FUNCTION "+" (X, Y : INTEGER) RETURN FLOAT IS
     BEGIN 
          FAILED ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
                  " IN SUBTYPE INDICATIONS");
          RETURN 2.0;
     END "+";
  
BEGIN
     TEST ("C87B14C","OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &  
           "OF SUBTYPE INDICATIONS");
   
     DECLARE
          SUBTYPE LIST1 IS LIST (WED .. (0 + 0));
          SUBTYPE LIST2 IS LIST ( 0 * 0 .. TUE);
          SUBTYPE LIST3 IS LIST ((0 + 0) .. (0 + 0));
          SUBTYPE LIST4 IS LIST ((0 * 0) .. (0 + 0));
   
          SUBTYPE GRID1 IS GRID (MON .. (0 * 0), (0 * 0) .. TUE);
          SUBTYPE GRID2 IS GRID ((0 * 0) .. WED, FRI .. (0 + 0));
          SUBTYPE GRID3 IS GRID
              ((0 * 0) .. (0 * 0), (0 + 0) .. (0 + 0));
          SUBTYPE GRID4 IS GRID ((0 * 0) .. (0 + 0), TUE .. THU);
  
     BEGIN
          NULL;
     END;
    
     RESULT;
END C87B14C;
