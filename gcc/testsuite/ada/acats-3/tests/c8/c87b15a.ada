-- C87B15A.ADA

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
-- THE ARRAY ATTRIBUTES OF THE FORM: A'FIRST (N), A'LAST (N), 
-- A'RANGE (N) AND A'LENGTH (N) MUST HAVE A PARAMETER (N) WHICH IS OF
-- THE TYPE UNIVERSAL_INTEGER.
  
-- TRH  26 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B15A IS
 
     FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER
          RENAMES STANDARD."*";
 
     TYPE BOX IS ARRAY (0 .. 1, 3 .. 6, 5 .. 11) OF BOOLEAN;
     B1 : BOX;
 
BEGIN
     TEST ("C87B15A","ARRAY ATTRIBUTES: FIRST (N), LAST (N), RANGE " &
           "(N) AND LENGTH (N) TAKE UNIVERSAL_INTEGER OPERANDS");
 
     IF BOX'FIRST  (1 + 0) /= 0    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 1");
     END IF;

     IF B1'FIRST   (1 + 1) /= 3    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 2");
     END IF;

     IF B1'FIRST   (2 + 1) /= 5    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 3");
     END IF;

     IF BOX'LAST   (0 + 1) /= 1    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 4");
     END IF;

     IF B1'LAST    (1 + 1) /= 6    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 5");
     END IF;

     IF B1'LAST    (1 + 2) /= 11   THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 6");
     END IF;

     IF BOX'LENGTH (0 + 1) /= 2    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 7");
     END IF;

     IF B1'LENGTH  (1 + 1) /= 4    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 8");
     END IF;

     IF B1'LENGTH  (2 + 1) /= 7    THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 9");
     END IF;

     IF 1 NOT IN BOX'RANGE (0 + 1) THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 10");
     END IF;

     IF 4 NOT IN B1'RANGE  (1 + 1) THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 11");
     END IF;

     IF 9 NOT IN B1'RANGE  (2 + 1) THEN
          FAILED ("ARRAY ATTRIBUTE OPERAND MUST BE OF TYPE " &
                  "UNIVERSAL_INTEGER - 12");
     END IF;

     RESULT;
END C87B15A;
