-- C37102B.ADA

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
-- CHECK THAT, FOR A RECORD TYPE, THE IDENTIFIER FOR A DISCRIMINANT 
-- CAN BE USED AS A SELECTED COMPONENT IN AN INDEX OR DISCRIMINANT 
-- CONSTRAINT, AS THE NAME OF A DISCRIMINANT IN A DISCRIMINANT 
-- SPECIFICATION, AND AS THE PARAMETER NAME IN A FUNCTION CALL IN A 
-- DISCRIMINANT OR INDEX CONSTRAINT.

-- R.WILLIAMS 8/25/86

WITH REPORT; USE REPORT;
PROCEDURE C37102B IS
     
BEGIN
     TEST ( "C37102B", "CHECK THAT, FOR A RECORD TYPE, THE " &
                       "IDENTIFIER FOR A DISCRIMINANT CAN BE USED " &
                       "AS A SELECTED COMPONENT IN AN INDEX OR " &
                       "DISCRIMINANT CONSTRAINT, AS THE NAME OF A " &
                       "DISCRIMINANT IN A DISCRIMINANT " &
                       "SPECIFICATION, AND AS THE PARAMETER NAME " &
                       "IN A FUNCTION CALL IN A DISCRIMINANT OR " &
                       "INDEX CONSTRAINT" );

     DECLARE

          FUNCTION F (D : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (D);
          END F;

          PACKAGE P IS

               TYPE D IS NEW INTEGER;
          
               TYPE REC1 IS
                    RECORD
                         D : INTEGER := IDENT_INT (1);
                    END RECORD;

               G : REC1;

               TYPE REC2 (D : INTEGER := 3) IS
                    RECORD
                         NULL;
                    END RECORD;

               H : REC2 (IDENT_INT (5));

               TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;

               TYPE Q (D : INTEGER := 0) IS
                    RECORD
                         J : REC2 (D => H.D);
                         K : ARR (G.D .. F (D => 5));
                         L : REC2 (F (D => 4));
                    END RECORD;
               
          END P;

          USE P;

     BEGIN
          DECLARE
               R : Q;
          
          BEGIN
               IF R.J.D /= 5 THEN
                    FAILED ( "INCORRECT VALUE FOR R.J" );
               END IF;
               
               IF R.K'FIRST /= 1 THEN
                    FAILED ( "INCORRECT VALUE FOR R.K'FIRST" );
               END IF;

               IF R.K'LAST /= 5 THEN
                    FAILED ( "INCORRECT VALUE FOR R.K'LAST" );
               END IF;

               IF R.L.D /= 4 THEN
                    FAILED ( "INCORRECT VALUE FOR R.L" );
               END IF;
          END;
        
     END;

     RESULT;
END C37102B;
