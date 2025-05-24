-- C87B45A.ADA

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
-- FOR A DEFAULT SUBPROGRAM PARAMETER, THE TYPE OF THE INITIALIZATION
-- EXPRESSION MUST MATCH THE PARAMETERS'S EXPLICIT TYPEMARK.
--      
-- THE FOUR KINDS OF EXPRESSIONS TESTED HERE ARE:
-- 
--    (A): A CALL TO AN OVERLOADED FUNCTION.
--    (B): AN OVERLOADED OPERATOR SYMBOL.
--    (C): AN OVERLOADED (INFIX) OPERATOR.
--    (D): AN OVERLOADED ENUMERATION LITERAL.
  
-- TRH  24 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B45A IS
  
     TYPE WHOLE  IS NEW INTEGER RANGE 0..INTEGER'LAST;
     TYPE CITRUS IS (LEMON, LIME, ORANGE);
     TYPE HUE    IS (RED, ORANGE, YELLOW);
     
     FUNCTION F1 (X, Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN -1;
     END F1;

     FUNCTION F1 (X, Y : WHOLE) RETURN WHOLE IS
     BEGIN
          RETURN 0;
     END F1;
    
     FUNCTION F1 (X, Y : INTEGER) RETURN HUE IS
     BEGIN
          RETURN ORANGE;
     END F1;

     FUNCTION F1 (X, Y : INTEGER) RETURN CITRUS IS
     BEGIN
          RETURN ORANGE;
     END F1;
   
BEGIN
     TEST ("C87B45A","OVERLOADED INITIALIZATION EXPRESSIONS" &
           " IN DEFAULT SUBPROGRAM PARAMETERS");
     DECLARE

          FUNCTION "/" (X, Y : INTEGER) RETURN INTEGER
               RENAMES F1;
    
          FUNCTION "/" (X, Y : WHOLE)   RETURN WHOLE
               RENAMES F1;
     
          FUNCTION "/" (X, Y : INTEGER) RETURN HUE
               RENAMES F1;
   
          FUNCTION "/" (X, Y : INTEGER) RETURN CITRUS
               RENAMES F1;
      
          PROCEDURE P1 (I1 : INTEGER := F1  (0, 0);
                        W1 : WHOLE   := F1  (0, 0);
                        C1 : CITRUS  := F1  (0, 0);
                        H1 : HUE     := F1  (0, 0);
                        I2 : INTEGER := "/" (0, 0);
                        W2 : WHOLE   := "/" (0, 0);
                        C2 : CITRUS  := "/" (0, 0);
                        H2 : HUE     := "/" (0, 0);
                        I3 : INTEGER := (0 / 0);
                        W3 : WHOLE   := (0 / 0);
                        C3 : CITRUS  := (0 / 0);
                        H3 : HUE     := (0 / 0);
                        C4 : CITRUS  := ORANGE;
                        H4 : HUE     := ORANGE)  IS
          BEGIN
               IF I1              /= -1 OR W1           /= 0 OR
                  CITRUS'POS (C1) /=  2 OR HUE'POS (H1) /= 1 THEN
                  FAILED ("(A): RESOLUTION INCORRECT - FUNCTION CALL");
               END IF;
    
               IF I2              /= -1 OR W2           /= 0 OR
                  CITRUS'POS (C2) /=  2 OR HUE'POS (H2) /= 1 THEN
                  FAILED ("(B): RESOLUTION INCORRECT " &
                          "- OPERATOR SYMBOL");
               END IF;
    
               IF I3              /= -1 OR W3           /= 0 OR
                  CITRUS'POS (C3) /=  2 OR HUE'POS (H3) /= 1 THEN
                  FAILED ("(C): RESOLUTION INCORRECT - INFIX OPERATOR");
               END IF;
    
               IF CITRUS'POS (C4) /=  2 OR HUE'POS (H4) /= 1 THEN
                  FAILED ("(D): RESOLUTION INCORRECT - ENUMERATION " &
                          "LITERAL");
               END IF;
          END P1;

     BEGIN
          P1;
     END;
    
     RESULT;
END C87B45A;
