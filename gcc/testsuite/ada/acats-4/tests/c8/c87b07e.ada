-- C87B07E.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'IMAGE (X), THE OPERAND X MUST
-- BE OF TYPE T. THE RESULT IS OF THE PREDEFINED TYPE STRING.
  
-- TRH  15 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B07E IS
 
     TYPE NEW_INT IS NEW INTEGER;
     TYPE NUMBER  IS NEW INTEGER;
     TYPE NEW_STR IS NEW STRING;
  
     FUNCTION "+" (X : NEW_INT) RETURN NEW_INT
          RENAMES "-";
     FUNCTION "-" (X : NUMBER)  RETURN NUMBER
          RENAMES "+";
 
     PROCEDURE P (X : NEW_STR) IS
     BEGIN
          FAILED ("THE IMAGE ATTRIBUTE MUST RETURN A VALUE OF THE" &
                  " PREDEFINED TYPE STRING");
     END P;
 
     PROCEDURE P (X : STRING) IS
     BEGIN
          NULL;
     END P;
    
BEGIN
     TEST ("C87B07E","OVERLOADED OPERANDS TO THE IMAGE ATTRIBUTE");
    
     IF INTEGER'IMAGE (+12) & INTEGER'IMAGE (-12) &
        NEW_INT'IMAGE (+12) & NEW_INT'IMAGE (-12) &
        NUMBER'IMAGE  (+12) & NUMBER'IMAGE  (-12)  /=
        " 12-12-12-12 12 12" THEN
        FAILED ("RESOLUTION INCORRECT FOR THE 'IMAGE' ATTRIBUTE");
     END IF;
   
     P (INTEGER'IMAGE (+1) & NEW_INT'IMAGE (+1) & NUMBER'IMAGE (-1));
     
     RESULT;
END C87B07E;
