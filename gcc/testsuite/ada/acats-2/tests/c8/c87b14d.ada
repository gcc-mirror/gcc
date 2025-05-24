-- C87B14D.ADA

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
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, IF A BOUND IS OF
-- TYPE UNIVERSAL_INTEGER, IT IS IMPLICITLY CONVERTED TO THE
-- INDEX BASE TYPE.
  
-- TRH  7 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B14D IS
     
     TYPE WHOLE IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
     TYPE LIST  IS ARRAY (WHOLE RANGE <>) OF BOOLEAN;
  
BEGIN
     TEST ("C87B14D","OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &
           "OF SUBTYPE INDICATIONS WITH UNIVERSAL_INTEGER BOUNDS");
   
     DECLARE
          FUNCTION "+" (X, Y : WHOLE) RETURN WHOLE 
               RENAMES "*";
   
          SUBTYPE LIST1 IS LIST (1 + 1 .. 1 + 1);
          SUBTYPE LIST2 IS LIST (1     .. 3 + 3);
          SUBTYPE LIST3 IS LIST (1 + 1 ..     2);
  
     BEGIN
          IF LIST1'FIRST /= 1 OR LIST1'LAST /= 1 OR
             LIST2'FIRST /= 1 OR LIST2'LAST /= 9 OR
             LIST3'FIRST /= 1 OR LIST3'LAST /= 2 THEN 
             FAILED ("RESOLUTION INCORRECT - IMPLICIT CONVERSION " &
                     "OF UNIVERSAL_INTEGER TYPE TO INDEX CONSTRAINT " &
                     "BASE TYPE");
          END IF;
     END;
    
     RESULT;
END C87B14D;
