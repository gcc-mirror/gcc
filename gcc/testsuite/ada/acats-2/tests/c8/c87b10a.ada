-- C87B10A.ADA

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
-- IN A RANGE CONSTRAINT OF A FIXED POINT OR FLOATING POINT TYPE
-- DEFINITION, BOTH BOUNDS MUST BE OF SOME REAL TYPE, ALTHOUGH
-- THE TWO BOUNDS DO NOT HAVE TO BE OF THE SAME TYPE.
  
-- TRH 7/28/82
-- DSJ 6/10/83
-- JBG 9/19/84
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B10A IS

     SUBTYPE DUR IS DURATION;

     FUNCTION "+" (X : FLOAT) RETURN INTEGER IS
     BEGIN
          FAILED ("RANGE CONSTRAINT FOR REAL TYPE DEFINITIONS " &
                  "MUST HAVE REAL BOUNDS");
          RETURN -10;
     END "+";

     FUNCTION "+" (X, Y : FLOAT) RETURN INTEGER IS
     BEGIN 
          FAILED ("RANGE CONSTRAINT FOR REAL TYPE DEFINITIONS " &
                  "MUST HAVE REAL BOUNDS");
          RETURN -10;
     END "+";
  
BEGIN
     TEST ("C87B10A","RANGE BOUNDS IN REAL TYPE DEFINITIONS MUST BE" &
           " OF SOME (NOT NECESSARILY THE SAME) REAL TYPE");
   
     DECLARE
          TYPE R1 IS DIGITS  2 RANGE 0.0 .. 1.0 + FLOAT'(1.0);
          TYPE R2 IS DELTA 0.1 RANGE FLOAT'(1.0) + 1.0 .. DUR'(2.0);
          TYPE R3 IS DIGITS  2 RANGE +1.0 .. "+" (FLOAT'(2.0), 2.0);
          TYPE R4 IS DELTA 0.1 RANGE 0.0 + FLOAT'(0.0) .. +1.0;
 
  
     BEGIN
          IF 2.0 NOT IN R1 OR -1.0 IN R2 OR
               -1.0 IN R3 OR -0.9 IN R4 THEN
               FAILED ("RANGE BOUNDS IN REAL TYPE DEFINITIONS DO NOT "
                    & "HAVE TO BE OF THE SAME REAL TYPE");
          END IF;
     END;
    
     RESULT;
END C87B10A;
