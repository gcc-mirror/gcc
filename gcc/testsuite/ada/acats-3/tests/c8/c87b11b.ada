-- C87B11B.ADA

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
-- IN A SUBTYPE INDICATION, THE DELTA EXPRESSION FOR A FIXED POINT
-- NUMBER MUST BE OF SOME REAL TYPE. 
  
-- TRH  29 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B11B IS
  
     TYPE DELT3 IS DELTA 3.0 RANGE -30.0 .. 30.0;
     
     FUNCTION "+" (X : FLOAT) RETURN INTEGER IS
     BEGIN 
           FAILED ("DELTA EXPRESSION MUST BE OF A REAL TYPE");
           RETURN 2;
     END "+";
  
BEGIN
     TEST ("C87B11B","OVERLOADED DELTA EXPRESSIONS IN " &
           "FIXED POINT SUBTYPE INDICATIONS");
   
     DECLARE
          SUBTYPE DELT2 IS DELT3 DELTA "+"(6.0);
          SUBTYPE DELT1 IS DELT3 DELTA "+"(10.0) RANGE -10.0 .. 10.0;
  
     BEGIN
          NULL;
     END;
    
     RESULT;
END C87B11B;
