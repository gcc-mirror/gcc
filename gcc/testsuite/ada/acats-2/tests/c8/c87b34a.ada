-- C87B34A.ADA

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
-- THE MEMBERSHIP TESTS "IN" AND "NOT IN" RESULT IN THE PREDEFINED
-- TYPE BOOLEAN. 
  
-- TRH  4 AUG 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B34A IS
     
     TYPE BIT   IS NEW BOOLEAN;
     TYPE FLAG  IS NEW BOOLEAN;
 
     ERR : BOOLEAN := FALSE;
 
     PROCEDURE P1 (X : BIT) IS
     BEGIN
          ERR := TRUE;
     END P1;
 
     PROCEDURE P1 (X : FLAG) IS
     BEGIN
          ERR := TRUE;
     END P1;
 
     PROCEDURE P1 (X : BOOLEAN) IS
     BEGIN
          NULL;
     END P1;
  
BEGIN
     TEST ("C87B34A","MEMBERSHIP TESTS 'IN' AND 'NOT IN' RETURN " &
           "TYPE PREDEFINED BOOLEAN");
   
     P1 (3 IN 1 .. 5);
     P1 (3 NOT IN 1 .. 5);
 
     IF ERR THEN
        FAILED ("MEMBERSHIP TESTS MUST RETURN PREDEFINED BOOLEAN TYPE");
     END IF;
    
     RESULT;
END C87B34A;
