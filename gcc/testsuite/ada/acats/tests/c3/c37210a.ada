-- C37210A.ADA

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
-- CHECK THAT THE EXPRESSION IN A DISCRIMINANT ASSOCIATION WITH MORE 
-- THAN ONE NAME IS EVALUATED ONCE FOR EACH NAME.

-- R.WILLIAMS 8/28/86

WITH REPORT; USE REPORT;
PROCEDURE C37210A IS

     BUMP : INTEGER := IDENT_INT (0);

     FUNCTION F RETURN INTEGER IS
     BEGIN
          BUMP := BUMP + 1;
          RETURN BUMP;
     END F;

     FUNCTION CHECK (STR : STRING) RETURN INTEGER IS
     BEGIN
          IF BUMP /= 2 THEN
               FAILED ( "INCORRECT DISCRIMINANT VALUES FOR " & STR);
          END IF;
          BUMP := IDENT_INT (0);
          RETURN 5;
     END CHECK;
          
BEGIN
     TEST ( "C37210A", "CHECK THAT THE EXPRESSION IN A " &
                       "DISCRIMINANT ASSOCIATION WITH MORE THAN " &
                       "ONE NAME IS EVALUATED ONCE FOR EACH NAME" );

     DECLARE
          TYPE REC (D1, D2 : INTEGER) IS
               RECORD 
                    NULL;
               END RECORD;

          R : REC (D1 | D2 => F);

          I1 : INTEGER := CHECK ( "R" );

          TYPE ACC IS ACCESS REC;

          AC : ACC (D1 | D2 => F);
     
          I2 : INTEGER := CHECK ( "AC" );
          
          PACKAGE PKG IS
               TYPE PRIV (D1, D2 : INTEGER) IS PRIVATE; 
               TYPE PACC IS ACCESS PRIV;

               TYPE LIM (D1, D2 : INTEGER) IS LIMITED PRIVATE;
               TYPE LACC IS ACCESS LIM;

          PRIVATE
               TYPE PRIV (D1, D2 : INTEGER) IS 
                    RECORD
                         NULL;
                    END RECORD;

               TYPE LIM (D1, D2 : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG;                                  
          
          USE PKG;

     BEGIN

          DECLARE
               P  : PRIV (D1 | D2 => F);
               
               I1 : INTEGER := CHECK ( "P" );

               PA : PACC (D1 | D2 => F);

               I2 : INTEGER := CHECK ( "PA" );

               L  : LIM (D1 | D2 => F);
          
               I3 : INTEGER := CHECK ( "L" );

               LA : LACC (D1 | D2 => F);

               I : INTEGER;
          BEGIN
               I := CHECK ( "LA" );
          END;
     END;

     RESULT;
END C37210A;
