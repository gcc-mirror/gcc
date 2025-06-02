-- C87B18A.ADA

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
-- THE TYPES OF THE EXPRESSIONS IN A DISCRIMINANT CONSTRAINT IN
-- A SUBTYPE INDICATION MUST MATCH THE DISCRIMINANT'S EXPLICIT
-- TYPEMARK.
  
-- TRH  1 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B18A IS
     
     ERR : BOOLEAN := FALSE;
 
     FUNCTION F1 RETURN INTEGER IS
     BEGIN 
          RETURN 1;
     END F1;

     FUNCTION F1 RETURN FLOAT IS
     BEGIN 
          ERR := TRUE;
          RETURN 0.0;
     END F1;
 
     FUNCTION F2 RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END F2;

     FUNCTION F2 RETURN STRING IS
     BEGIN 
          ERR := TRUE;
          RETURN "STRING";
     END F2;
  
BEGIN
     TEST ("C87B18A","OVERLOADED EXPRESSIONS IN DISCRIMINANT " &
           "CONSTRAINTS");
   
     DECLARE
          TYPE REC (X : INTEGER := 0; Y : BOOLEAN := TRUE) IS
               RECORD
                    NULL;
               END RECORD;
 
          R1 : REC (F1, F2);
          R2 : REC (Y => F2, X => F1);

     BEGIN
          IF ERR THEN 
               FAILED ("RESOLUTION INCORRECT - DISCRIMINANT " &
                       "CONSTRAINT MUST MATCH DISCRIMINANT TYPE");
          END IF;
     END;
    
     RESULT;
END C87B18A;
