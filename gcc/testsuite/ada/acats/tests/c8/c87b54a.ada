-- C87B54A.ADA

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
-- THE ARGUMENT OF THE DELAY STATEMENT IS OF THE PREDEFINED FIXED 
-- POINT TYPE DURATION.
  
-- TRH  7 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B54A IS
 
     TYPE TEMPS  IS NEW DURATION;
     TYPE REAL   IS NEW FLOAT;
     TYPE TEMPUS IS DELTA 0.1 RANGE -1.0 .. 1.0;
     ERR : BOOLEAN := FALSE;
 
     FUNCTION F (X : TEMPS) RETURN TEMPS IS
     BEGIN
          ERR := TRUE;
          RETURN X;
     END F;
    
     FUNCTION F (X : REAL) RETURN REAL IS
     BEGIN
          ERR := TRUE;
          RETURN X;
     END F;
 
     FUNCTION F (X : TEMPUS) RETURN TEMPUS IS
     BEGIN
          ERR := TRUE;
          RETURN X;
     END F;
    
     FUNCTION F (X : DURATION) RETURN DURATION IS
     BEGIN
          RETURN X;
     END F;
    
BEGIN
     TEST ("C87B54A","OVERLOADED EXPRESSION WITHIN DELAY STATEMENT");
    
     DECLARE
          TASK T IS
               ENTRY E;
          END T;
  
          TASK BODY T IS
          BEGIN
               DELAY F (0.0);
               DELAY F (1.0);
               DELAY F (-1.0);
          END T;
  
     BEGIN
          IF ERR THEN FAILED ("DELAY STATEMENT TAKES AN ARGUMENT OF " &
                              "THE PREDEFINED FIXED POINT TYPE " &
                              "DURATION");
          END IF;
     END;
 
     RESULT;
END C87B54A;
