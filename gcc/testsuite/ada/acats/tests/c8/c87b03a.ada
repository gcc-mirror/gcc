-- C87B03A.ADA

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
-- THE EXPRESSION IN A NUMBER DECLARATION MUST BE EITHER OF THE TYPE
-- UNIVERSAL_INTEGER OR UNIVERSAL_REAL.
   
-- TRH  16 JUNE 82
     
WITH REPORT; USE REPORT;
    
PROCEDURE C87B03A IS
    
BEGIN
     TEST ("C87B03A","OVERLOADED EXPRESSIONS IN NUMBER DECLARATIONS");
  
     DECLARE
          FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER
               RENAMES STANDARD."-";
           
          FUNCTION "+" (X, Y : FLOAT)   RETURN FLOAT
               RENAMES STANDARD."-";
  
          I1 : CONSTANT         := 1 + 1;
          I2 : CONSTANT INTEGER := 1 + 1;
   
          R1 : CONSTANT       := 1.0 + 1.0;
          R2 : CONSTANT FLOAT := 1.0 + 1.0;
  
     BEGIN
          IF I1 /= 2   OR I2 /= 0   OR 
             R1 /= 2.0 OR R2 /= 0.0 THEN
             FAILED ("OVERLOADED EXPRESSIONS IN NUMBER DECLARATIONS" &
                     " RESOLVED INCORRECTLY");
          END IF;
     END;
   
     RESULT;
END C87B03A;
