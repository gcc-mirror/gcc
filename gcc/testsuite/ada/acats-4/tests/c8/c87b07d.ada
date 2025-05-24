-- C87B07D.ADA

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
-- THE ATTRIBUTES OF THE FORM T'SUCC (X) AND T'PRED (X) TAKE AN
-- OPERAND X OF TYPE T AND RETURN A VALUE OF TYPE T.
  
-- TRH  15 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B07D IS
 
     TYPE NEW_INT IS NEW INTEGER;
     TYPE WHOLE   IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
  
     FUNCTION "+" (X, Y : WHOLE)   RETURN WHOLE 
          RENAMES "*";
     FUNCTION "+" (X, Y : NEW_INT) RETURN NEW_INT
          RENAMES "-";
    
BEGIN
     TEST ("C87B07D","OVERLOADED OPERANDS TO THE ATTRIBUTES " &
           "'PRED' AND 'SUCC'");
    
     IF INTEGER'SUCC (1 + 1) /= 3  OR INTEGER'SUCC (3 + 3) + 1 /= 8  OR
        NEW_INT'SUCC (1 + 1) /= 1  OR NEW_INT'SUCC (3 + 3) + 1 /= 0  OR
        WHOLE'SUCC   (1 + 1) /= 2  OR WHOLE'SUCC   (3 + 3) + 1 /= 10 OR
        INTEGER'PRED (1 + 1) /= 1  OR INTEGER'PRED (3 + 3) + 1 /= 6  OR
        NEW_INT'PRED (1 + 1) /= -1 OR NEW_INT'PRED (3 + 3) + 1 /= -2 OR
        WHOLE'PRED   (1 + 1) /= 0  OR WHOLE'PRED   (3 + 3) + 1 /= 8  
        THEN FAILED ("RESOLUTION INCORRECT FOR OPERAND OR RESULT OF" &
                     " THE 'PRED' OR 'SUCC' ATTRIBUTE");
     END IF;
     
     RESULT;
END C87B07D;
