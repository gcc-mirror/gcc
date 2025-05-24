-- C87B44A.ADA

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
-- THE TYPE OF THE EXPRESSION IN A RETURN STATEMENT MUST MATCH THE
-- EXPLICIT TYPEMARK IN THE RETURN CLAUSE OF THE FUNCTION'S
-- SPECIFICATION.
--
-- THE FOUR KINDS OF EXPRESSIONS TESTED HERE ARE:
-- 
--    (A): A CALL TO AN OVERLOADED FUNCTION.
--    (B): AN OVERLOADED OPERATOR SYMBOL.
--    (C): AN OVERLOADED (INFIX) OPERATOR.
--    (D): AN OVERLOADED ENUMERATION LITERAL.
  
-- TRH  25 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B44A IS
  
     TYPE WHOLE  IS NEW INTEGER RANGE 0..INTEGER'LAST;
     TYPE CITRUS IS (LEMON, LIME, ORANGE);
     TYPE HUE    IS (RED, ORANGE, YELLOW);
     
     FUNCTION F1 (X, Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN -1;
     END F1;

     FUNCTION "*" (X, Y : WHOLE) RETURN WHOLE IS
     BEGIN
          RETURN 0;
     END "*";
    
     FUNCTION "*" (X, Y : INTEGER) RETURN HUE IS
     BEGIN
          RETURN ORANGE;
     END "*";

     FUNCTION F1 (X, Y : INTEGER) RETURN CITRUS IS
     BEGIN
          RETURN ORANGE;
     END F1;
   
BEGIN
     TEST ("C87B44A","OVERLOADED EXPRESSIONS IN RETURN STATEMENTS");
     DECLARE

          FUNCTION F2 (X, Y : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN F1 (X, Y);
          END F2;
   
          FUNCTION F2 (X, Y : WHOLE) RETURN WHOLE IS
          BEGIN
               RETURN "*" (X, Y);
          END F2;
   
          FUNCTION F2 (X, Y : INTEGER) RETURN HUE IS
          BEGIN
               RETURN (X * Y);
          END F2;
   
          FUNCTION F2 (X, Y : INTEGER) RETURN CITRUS IS
          BEGIN
               RETURN ORANGE;
          END F2;
   

     BEGIN
          IF INTEGER'(F2 (0, 0)) /= -1 THEN
             FAILED ("(A): RESOLUTION INCORRECT - FUNCTION CALL");
          END IF;
    
          IF WHOLE'(F2 (0, 0)) /= 0 THEN
             FAILED ("(B): RESOLUTION INCORRECT - OPERATOR SYMBOL");
          END IF;
    
          IF HUE'POS (F2 (0, 0)) /= 1 THEN 
             FAILED ("(C): RESOLUTION INCORRECT - INFIX OPERATOR");
          END IF;
    
          IF CITRUS'POS (F2 (0, 0)) /= 2 THEN 
             FAILED ("(D): RESOLUTION INCORRECT - ENUMERATION LITERAL");
          END IF;
     END;
    
     RESULT;
END C87B44A;
