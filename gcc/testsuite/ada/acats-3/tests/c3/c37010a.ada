-- C37010A.ADA

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
-- CHECK THAT EXPRESSIONS IN CONSTRAINTS OF COMPONENT DECLARATIONS ARE
-- EVALUATED IN THE ORDER THE COMPONENTS APPEAR.

-- R.WILLIAMS 8/22/86

WITH REPORT; USE REPORT;
PROCEDURE C37010A IS
     
     TYPE R (D : INTEGER) IS
             RECORD
                  NULL;
             END RECORD;

     TYPE ACCR IS ACCESS R;

     TYPE ARR IS ARRAY (POSITIVE RANGE <> ) OF INTEGER;

     TYPE ACCA IS ACCESS ARR;

     BUMP : INTEGER := 0;

     FUNCTION F RETURN INTEGER IS
     BEGIN
          BUMP := BUMP + 1;
          RETURN BUMP;
     END;          

BEGIN
     TEST ( "C37010A", "CHECK THAT EXPRESSIONS IN CONSTRAINTS OF " &
                       "COMPONENT DECLARATIONS ARE EVALUATED IN " &
                       "THE ORDER THE COMPONENTS APPEAR" );

     DECLARE
          
          TYPE REC1 IS 
               RECORD
                    A1 : R (D => F);
                    B1 : STRING (1 .. F);
                    C1 : ACCR (F);
                    D1 : ACCA (1 .. F);
               END RECORD;
               
          R1 : REC1;

     BEGIN
          IF R1.A1.D /= 1 THEN 
               FAILED ( "INCORRECT VALUE FOR R1.A1.D" );
          END IF;
     
          IF R1.B1'LAST /= 2 THEN
               FAILED ( "INCORRECT VALUE FOR R1.B1'LAST" );
          END IF;
     
          BEGIN
               R1.C1 := NEW R'(D => 3);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R1.C1" );
          END;

          BEGIN
               R1.D1 := NEW ARR (1 .. 4);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R1.D1" );
          END;

     END;

     BUMP := 0;
          
     DECLARE
          
          TYPE REC2 (I : INTEGER) IS
               RECORD
                    CASE I IS
                         WHEN 1 =>
                              NULL;
                         WHEN OTHERS =>
                              A2 : R (D => F);
                              B2 : ARR (1 .. F);
                              C2 : ACCR (F);
                              D2 : ACCA (1 .. F);
                    END CASE;                     
               END RECORD;

          R2 : REC2 (IDENT_INT (2));

     BEGIN
          
          IF R2.A2.D /= 1 THEN 
               FAILED ( "INCORRECT VALUE FOR R2.A2.D" );
          END IF;

          IF R2.B2'LAST /= 2 THEN
               FAILED ( "INCORRECT VALUE FOR R2.B2'LAST" );
          END IF;

          BEGIN
               R2.C2 := NEW R (D => 3);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R2.C2" );
          END;
 
          BEGIN
               R2.D2 := NEW ARR (1 .. 4);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "INCORRECT VALUE FOR R2.D2" );
          END;                         
          
     END;
          
     RESULT;
END C37010A;
