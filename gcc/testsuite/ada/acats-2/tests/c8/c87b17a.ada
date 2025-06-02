-- C87B17A.ADA

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
  
-- THE INITIALIZATION EXPRESSION FOR A DEFAULT DISCRIMINANT
-- IN A TYPE DECLARATION MUST MATCH THE DISCRIMINANT'S EXPLICIT
-- TYPEMARK.
--
-- THE THREE KINDS OF TYPE DECLARATIONS TESTED HERE ARE:
--
--    (A): RECORD TYPE.
--    (B): PRIVATE TYPE.
--    (C): INCOMPLETE RECORD TYPE.
    
-- TRH  18 JUNE 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B17A IS
  
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
     TEST ("C87B17A","OVERLOADED INITIALIZATION EXPRESSIONS" &
           " IN DEFAULT DISCRIMINANTS");
   
     DECLARE

          FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER
               RENAMES F1;
    
          FUNCTION "+" (X, Y : WHOLE)   RETURN WHOLE
               RENAMES F1;
     
          FUNCTION "+" (X, Y : INTEGER) RETURN HUE
               RENAMES F1;
   
          FUNCTION "+" (X, Y : INTEGER) RETURN CITRUS
               RENAMES F1;

          TYPE REC1 (I1 : INTEGER := 0 + 0; H1 : HUE := F1 (0, 0) ) IS
               RECORD
                    NULL;
               END RECORD;
    
          PACKAGE PVT IS 
               TYPE REC2 (H2 : HUE := ORANGE; W2 : WHOLE := 0 + 0 )
                    IS PRIVATE;
          PRIVATE
               TYPE REC2 (H2 : HUE := ORANGE; W2 : WHOLE := 0 + 0 ) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PVT;
          USE PVT;
  
          TYPE REC3 (C1 : CITRUS := ORANGE; W1 : WHOLE := "+" (0, 0));
  
          TYPE LINK IS ACCESS REC3;
   
          TYPE REC3 (C1 : CITRUS := ORANGE; W1 : WHOLE := "+" (0, 0)) IS
               RECORD
                    NULL;
               END RECORD;
   
          R1 : REC1;
          R2 : REC2;
          R3 : REC3;
   
     BEGIN
          IF R1.I1 /= -1 OR HUE'POS (R1.H1) /= 1 THEN 
             FAILED ("(A): RESOLUTION INCORRECT FOR RECORD TYPES");
          END IF;
   
          IF HUE'POS (R2.H2) /=  1 OR R2.W2 /= 0 THEN
             FAILED ("(B): RESOLUTION INCORRECT FOR PRIVATE TYPES");
          END IF;
  
          IF CITRUS'POS (R3.C1) /= 2 OR R3.W1 /= 0 THEN
             FAILED ("(C): RESOLUTION INCORRECT FOR INCOMPLETE" &
                     " RECORD TYPES");
          END IF;
     END;
    
     RESULT;
END C87B17A;
