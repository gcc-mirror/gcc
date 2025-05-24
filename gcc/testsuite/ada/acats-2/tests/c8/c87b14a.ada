-- C87B14A.ADA

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
-- IN SUBTYPE INDICATIONS WITH INDEX CONSTRAINTS, THE LOWER AND UPPER
-- BOUNDS MUST BE OF THE INDEX BASE TYPE.
--
-- TEST (A): INDEX CONSTRAINTS WITH OVERLOADED FUNCTIONS.
  
-- TRH  30 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B14A IS
     
     SUBTYPE WHOLE  IS INTEGER RANGE 0 .. INTEGER'LAST;
     SUBTYPE BASE10 IS INTEGER RANGE 0 .. 9;
     TYPE LIST IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
     TYPE GRID IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF BOOLEAN;
   
     FUNCTION F1 RETURN WHOLE IS
     BEGIN 
          RETURN 1;
     END F1;

     FUNCTION F1 RETURN BOOLEAN IS
     BEGIN 
          FAILED ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
                  " IN SUBTYPE INDICATIONS");
          RETURN TRUE;
     END F1;
  
     FUNCTION F2 RETURN BASE10 IS
     BEGIN 
          RETURN 2;
     END F2;

     FUNCTION F2 RETURN FLOAT IS
     BEGIN 
          FAILED ("RESOLUTION INCORRECT - INDEX CONSTRAINTS " &
                  " IN SUBTYPE INDICATIONS");
          RETURN 2.0;
     END F2;
  
BEGIN
     TEST ("C87B14A","OVERLOADED EXPRESSIONS IN INDEX CONSTRAINTS " &
           "OF SUBTYPE INDICATIONS");
   
     DECLARE
          SUBTYPE LIST1 IS LIST (1  .. F1);
          SUBTYPE LIST2 IS LIST (F1 ..  1);
          SUBTYPE LIST3 IS LIST (F2 .. F2);
          SUBTYPE LIST4 IS LIST (F1 .. F2);
   
          SUBTYPE GRID1 IS GRID (1  .. F1, F1 ..  1);
          SUBTYPE GRID2 IS GRID (F1 ..  2, 2  .. F2);
          SUBTYPE GRID3 IS GRID (F1 .. F1, F2 .. F2);
          SUBTYPE GRID4 IS GRID (F1 .. F2, 1  ..  2);
  
     BEGIN
          NULL;
     END;
    
     RESULT;
END C87B14A;
