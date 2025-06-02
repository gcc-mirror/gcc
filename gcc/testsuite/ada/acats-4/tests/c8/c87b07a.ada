-- C87B07A.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'POS (X), THE OPERAND X MUST 
-- BE A VALUE OF TYPE T. THE RESULT IS OF TYPE UNIVERSAL_INTEGER.
  
-- TRH  13 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B07A IS
 
     TYPE NATURAL IS NEW INTEGER RANGE 1 .. INTEGER'LAST;
     TYPE WHOLE   IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
     TYPE COLOR   IS (BROWN, RED, WHITE);
     TYPE SCHOOL  IS (HARVARD, BROWN, YALE);
     TYPE SUGAR   IS (DEXTROSE, CANE, BROWN);
 
     FUNCTION "+" (X, Y : NATURAL) RETURN NATURAL
          RENAMES "*";
     FUNCTION "+" (X, Y : WHOLE) RETURN WHOLE
          RENAMES "-";
    
BEGIN
     TEST ("C87B07A","OVERLOADED OPERANDS TO THE 'POS' ATTRIBUTE");
  
     IF NATURAL'POS (1 + 1) /= 1 OR COLOR'POS  (BROWN) /= 0 OR
        WHOLE'POS   (1 + 1) /= 0 OR SCHOOL'POS (BROWN) /= 1 OR
        INTEGER'POS (1 + 1) /= 2 OR SUGAR'POS  (BROWN) /= 2 THEN
        FAILED ("RESOLUTION INCORRECT FOR OPERAND TO 'POS' ATTRIBUTE");
     END IF;
 
     IF NATURAL'POS (3 + 3) + 1 /= 10 OR    -- SECOND "+" IS UNIVERSAL.
        WHOLE'POS   (3 + 3) + 1 /=  1 OR    -- SECOND "+" IS UNIVERSAL.
        INTEGER'POS (3 + 3) + 1 /=  7 THEN  -- SECOND "+" IS UNIVERSAL.
        FAILED ("RESOLUTION INCORRECT - 'POS' ATTRIBUTE RETURNS " &
                "A UNIVERSAL_INTEGER VALUE");
     END IF;

     RESULT;
END C87B07A;
