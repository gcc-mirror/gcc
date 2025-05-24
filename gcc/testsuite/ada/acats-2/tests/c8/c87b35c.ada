-- C87B35C.ADA

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
-- THE EXPONENT OPERAND OF A FLOATING POINT EXPONENTIATION MUST BE
-- OF THE TYPE PREDEFINED INTEGER.
  
-- TRH  4 AUG 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B35C IS
 
     TYPE FIXED IS DELTA 0.01 RANGE 0.0 .. 4.0;
     ERR : BOOLEAN := FALSE;
   
     FUNCTION F1 (X : INTEGER) RETURN INTEGER IS
     BEGIN 
          RETURN X;
     END F1;
       
     FUNCTION F1 (X : INTEGER) RETURN FLOAT IS
     BEGIN 
          ERR := TRUE;
          RETURN 1.0;
     END F1;
    
     FUNCTION F1 (X : INTEGER) RETURN FIXED IS
     BEGIN 
          ERR := TRUE;
          RETURN 1.0;
     END F1;
 
BEGIN
     TEST ("C87B35C","EXPONENT OPERAND FOR FLOATING POINT " &
           "EXPONENTIATION MUST BE OF TYPE PREDEFINED INTEGER");
  
     DECLARE
          FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER
               RENAMES STANDARD."*";
    
     BEGIN
          IF ( FLOAT'(2.0) ** F1(3)   /= 8.0 OR
               FLOAT'(2.0) ** (3 + 1) /= 8.0 ) THEN
               FAILED ("EXPONENT OF FLOATING POINT EXPONENTIATION "
                    & "MUST BE PREDEFINED INTEGER (A)");
          END IF;
          IF ( 2.0 ** F1(3)   /= FLOAT'(8.0) OR
               2.0 ** (3 + 1) /= FLOAT'(8.0) ) THEN
               FAILED ("EXPONENT OF FLOATING POINT EXPONENTIATION"
                    & "MUST BE PREDEFINED INTEGER (B)");
          END IF;
          IF ERR THEN
               FAILED ("EXPONENT OF FLOATING POINT EXPONENTIATION"
                    & "MUST BE PREDEFINED INTEGER (C)");
          END IF;
     END;
 
     RESULT;
END C87B35C;
