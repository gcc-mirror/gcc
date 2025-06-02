-- C37010B.ADA

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
-- CHECK THAT EXPRESSIONS IN AN INDEX CONSTRAINT OR DISCRIMINANT 
-- CONSTRAINT ARE EVALUATED WHEN THE COMPONENT DECLARATION IS 
-- ELABORATED EVEN IF SOME BOUNDS OR DISCRIMINANTS ARE GIVEN BY
-- A DISCRIMINANT OF AN ENCLOSING RECORD TYPE.

-- R.WILLIAMS 8/22/86

WITH REPORT; USE REPORT;
PROCEDURE C37010B IS
     
     INIT :INTEGER := IDENT_INT (5);

     TYPE R (D1, D2 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE ACCR IS ACCESS R;

     TYPE ARR IS ARRAY (INTEGER RANGE <> ) OF INTEGER;

     TYPE ACCA IS ACCESS ARR;

     FUNCTION RESET (N : INTEGER) RETURN INTEGER IS
     BEGIN
          INIT := IDENT_INT (N);
          RETURN N;
     END RESET;

BEGIN
     TEST ( "C37010B", "CHECK THAT EXPRESSIONS IN AN INDEX " &
                       "CONSTRAINT OR DISCRIMINANT CONSTRAINT " &
                       "ARE EVALUATED WHEN THE COMPONENT " &
                       "DECLARATION IS ELABORATED EVEN IF SOME " &
                       "BOUNDS OR DISCRIMINANTS ARE GIVEN BY " &
                       "A DISCRIMINANT OF AN ENCLOSING RECORD TYPE" );

     DECLARE
          
          TYPE REC1 (D : INTEGER) IS 
               RECORD
                    W1 : R (D1 => INIT, D2 => D);
                    X1 : ARR (INIT .. D);
                    Y1 : ACCR (D, INIT);
                    Z1 : ACCA (D .. INIT);
               END RECORD;
          
          INT1 : INTEGER := RESET (10);

          R1 : REC1 (D => 4);

     BEGIN
          IF R1.W1.D1 /= 5 THEN 
               FAILED ( "INCORRECT VALUE FOR R1.W1.D1" );
          END IF;
     
          IF R1.W1.D2 /= 4 THEN 
               FAILED ( "INCORRECT VALUE FOR R1.W1.D2" );
          END IF;

          IF R1.X1'FIRST /= 5 THEN
               FAILED ( "INCORRECT VALUE FOR R1.X1'FIRST" );
          END IF;

          IF R1.X1'LAST /= 4 THEN
               FAILED ( "INCORRECT VALUE FOR R1.X1'LAST" );
          END IF;

          BEGIN
               R1.Y1 := NEW R (4, 5);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R1.Y1" );
          END;

          BEGIN
               R1.Z1 := NEW ARR (4 .. 5);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R1.Z1" );
          END;
                    
     END;

     DECLARE
          
          TYPE REC2 (D : INTEGER) IS
               RECORD
                    CASE D IS
                         WHEN 1 =>
                              NULL;
                         WHEN 2 =>
                              NULL;
                         WHEN OTHERS =>
                              W2 : R (D1 => D, D2 => INIT);
                              X2 : ARR (D .. INIT);
                              Y2 : ACCR (INIT, D);
                              Z2 : ACCA (D .. INIT);
                    END CASE;                     
               END RECORD;

          INT2 : INTEGER := RESET (20);

          R2 : REC2 (D => 6);

     BEGIN
          IF R2.W2.D1 /= 6 THEN 
               FAILED ( "INCORRECT VALUE FOR R2.W2.D1" );
          END IF;

          IF R2.W2.D2 /= 10 THEN 
               FAILED ( "INCORRECT VALUE FOR R2.W2.D2" );
          END IF;

          IF R2.X2'FIRST /= 6 THEN
               FAILED ( "INCORRECT VALUE FOR R2.X2'FIRST" );
          END IF;

          IF R2.X2'LAST /= 10 THEN
               FAILED ( "INCORRECT VALUE FOR R2.X2'LAST" );
          END IF;

          BEGIN
               R2.Y2 := NEW R (10, 6);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R2.Y2" );
          END;

          BEGIN
               R2.Z2 := NEW ARR (6 .. 10);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R2.Z2" );
          END;
                    
     END;

     RESULT;
END C37010B;
