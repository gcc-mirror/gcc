-- C32001C.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR RECORD TYPES, THE 
-- SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED
-- ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE 
-- INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE EVALUATIONS 
-- YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

WITH REPORT; USE REPORT;

PROCEDURE C32001C IS

     TYPE ARR IS ARRAY (1 .. 2) OF INTEGER;
     F1, G1 : ARR;
     BUMP : ARR := (0, 0);

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP(I) + 1;
          F1 (I) := BUMP (I);
          RETURN BUMP (I);
     END F;

     FUNCTION G (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP(I) + 1;
          G1 (I) := BUMP (I);
          RETURN BUMP (I);
     END G;

     FUNCTION H (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP(I) + 1;
          RETURN BUMP (I);
     END H;

BEGIN
     TEST ("C32001C", "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " & 
                      "FOR RECORD TYPES, THE SUBTYPE INDICATION " & 
                      "AND THE INITIALIZATION EXPRESSIONS ARE " &
                      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
                      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
                      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
                      "EVALUATIONS YIELD THE SAME RESULT AS A " &
                      "SEQUENCE OF SINGLE OBJECT DECLARATIONS" );
     
     DECLARE

          TYPE REC (D1, D2 : INTEGER) IS
               RECORD 
                    VALUE : INTEGER;
               END RECORD;
     
          R1, R2   : REC (F (1), G (1)) := 
                     (F1 (1), G1 (1), VALUE => H (1));
          CR1, CR2 : CONSTANT REC (F (2), G (2)) := 
                     (F1 (2), G1 (2), VALUE => H (2));
          
          PROCEDURE CHECK 
            (R : REC; V1, V2, VAL : INTEGER; S : STRING) IS
          BEGIN          
               IF R.D1 = V1 THEN
                    IF R.D2 = V2 THEN
                         COMMENT ( S & ".D1 INITIALIZED TO " & 
                                   INTEGER'IMAGE (V1) & " AND " &
                                   S & ".D2 INITIALIZED TO " &
                                   INTEGER'IMAGE (V2));
                    ELSE
                         FAILED ( S & 
                                  ".D2 INITIALIZED INCORRECTLY - 1" );
                    END IF;
               ELSIF R.D1 = V2 THEN
                    IF R.D2 =V1 THEN
                         COMMENT ( S & ".D1 INITIALIZED TO " & 
                                   INTEGER'IMAGE (V2) & " AND " &
                                   S & ".D2 INITIALIZED TO " &
                                   INTEGER'IMAGE (V1));
                    ELSE
                         FAILED ( S & 
                                  ".D2 INITIALIZED INCORRECTLY - 2" );
                    END IF;
               ELSE
                    FAILED ( S & ".D1 INITIALIZED INCORRECTLY TO " &
                              INTEGER'IMAGE (R.D1) );
               END IF;
               
               IF R.VALUE /= VAL THEN
                    FAILED ( S & ".VALUE INITIALIZED INCORRECTLY" );
               END IF;
          END CHECK;
     
     BEGIN     
          CHECK (R1, 1, 2, 3, "R1");
          CHECK (R2, 4, 5, 6, "R2");

          CHECK (CR1, 1, 2, 3, "CR1");
          CHECK (CR2, 4, 5, 6, "CR2");
     END;     

     RESULT;
END C32001C;
