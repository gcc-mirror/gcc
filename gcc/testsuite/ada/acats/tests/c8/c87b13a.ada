-- C87B13A.ADA

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
-- THE LOWER AND UPPER BOUNDS OF AN INDEX CONSTRAINT IN A CONSTRAINED
-- ARRAY TYPE DEFINITION MUST BE DISCRETE AND OF THE SAME TYPE.
  
-- TRH  1 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B13A IS
     
     TYPE CENTI IS DELTA 0.01 RANGE -1.0 .. 1.0;
   
     FUNCTION F1 (X : INTEGER) RETURN INTEGER IS
     BEGIN 
          RETURN X;
     END F1;

     FUNCTION F1 (X : INTEGER) RETURN CENTI IS
     BEGIN 
          FAILED ("INDEX CONSTRAINT BOUNDS MUST BE DISCRETE AND " &
                  " OF THE SAME TYPE");
          RETURN 0.0;
     END F1;
  
     FUNCTION F1 (X : INTEGER) RETURN FLOAT IS
     BEGIN 
          FAILED ("INDEX CONSTRAINT BOUNDS MUST BE DISCRETE AND " &
                  " OF THE SAME TYPE");
          RETURN 1.0;
     END F1;
  
BEGIN
     TEST ("C87B13A","OVERLOADED INDEX CONSTRAINTS IN " &
           "CONSTRAINED ARRAY TYPE DEFINITIONS");
   
     DECLARE
          TYPE A1 IS ARRAY (F1 (1) .. F1 (1)) OF BOOLEAN;
          TYPE A2 IS ARRAY (1      .. F1 (2)) OF BOOLEAN;
          TYPE A3 IS ARRAY (F1 (1) ..     2)  OF BOOLEAN;
  
     BEGIN
          NULL;
     END;
    
     RESULT;
END C87B13A;
