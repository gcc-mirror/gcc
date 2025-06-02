-- C87B24B.ADA

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
    
-- THE RANGE BOUNDS FOR A SLICE MUST BE DISCRETE AND OF THE SAME BASE
-- TYPE AS THE ARRAY INDEX.
  
-- TRH  15 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B24B IS
 
     TYPE PIECE IS ARRAY (INTEGER RANGE <>) OF INTEGER;
 
     PI  : PIECE (1 .. 8) := (3, 1, 4, 1, 5, 9, 2, 6);
     S1  : PIECE (1 .. 3);
     S2  : PIECE (4 .. 8);
     ERR : BOOLEAN := FALSE;
       
     FUNCTION F1 (X : INTEGER) RETURN INTEGER IS
     BEGIN 
          RETURN X;
     END F1;
       
     FUNCTION F1 (X : INTEGER) RETURN FLOAT IS
     BEGIN 
          ERR := TRUE;
          RETURN 0.0;
     END F1;
    
     FUNCTION F2 (X : INTEGER) RETURN INTEGER IS
     BEGIN 
          RETURN X;
     END F2;
    
     FUNCTION F2 (X :INTEGER) RETURN CHARACTER IS
     BEGIN 
          ERR := TRUE;
          RETURN 'A';
     END F2;
    
BEGIN
     TEST ("C87B24B","OVERLOADING RESOLUTION OF RANGE " &
           "CONSTRAINTS FOR SLICES");
 
     DECLARE
          FUNCTION "+" (X : INTEGER) RETURN INTEGER 
               RENAMES F1;
   
          FUNCTION "+" (X : INTEGER) RETURN FLOAT 
               RENAMES F1;
   
          FUNCTION "-" (X : INTEGER) RETURN INTEGER 
               RENAMES F2;
 
          FUNCTION "-" (X : INTEGER) RETURN CHARACTER
               RENAMES F2;
  
     BEGIN 
          S1 := PI ("+" (3) .. "-" (5));
          S1 := PI (F2  (2) .. "+" (4));
          S1 := PI ("-" (6) .. F1  (8));
          S1 := PI (F2  (1) .. F2  (3));
          S2 := PI (F2  (4) .. F1  (8));
          S2 := PI (2       .. "+" (6));
          S2 := PI (F1  (1) ..       5);
          S2 := PI ("+" (3) .. "+" (7));
  
          IF ERR THEN
               FAILED (" OVERLOADING RESOLUTION INCORRECT FOR SLICES");
          END IF;
     END;
 
     RESULT;
END C87B24B;
