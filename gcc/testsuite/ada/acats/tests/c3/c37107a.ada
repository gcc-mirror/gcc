-- C37107A.ADA

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
-- CHECK THAT A DEFAULT DISCRIMINANT EXPRESSION NEED NOT BE STATIC AND
-- IS EVALUATED ONLY WHEN NEEDED.

-- R.WILLIAMS 8/25/86
-- GMT        6/29/87  ADDED INTEGER ARGUMENT TO THE FUNCTION F.


WITH REPORT; USE REPORT;
PROCEDURE C37107A IS

     FUNCTION  F (  B : BOOLEAN;
                    I : INTEGER  ) RETURN INTEGER IS
     BEGIN
          IF NOT B THEN
               FAILED ( "DEFAULT DISCRIMINANT EVALUATED " &
                        "UNNECESSARILY - "                &
                        INTEGER'IMAGE(I)  );
          END IF;

          RETURN IDENT_INT (1);
     END F;

BEGIN
     TEST ( "C37107A", "CHECK THAT A DEFAULT DISCRIMINANT " &
                       "EXPRESSION NEED NOT BE STATIC AND IS " &
                       "EVALUATED ONLY WHEN NEEDED" );

     DECLARE
          TYPE REC1 (  D : INTEGER := F (TRUE,1)  ) IS
               RECORD
                    NULL;
               END RECORD;

          R1 : REC1;

          TYPE REC2 (  D : INTEGER := F (FALSE,2)  ) IS
               RECORD
                    NULL;
               END RECORD;

          R2 : REC2 (D => 0);

     BEGIN
          IF R1.D /= 1 THEN
               FAILED ( "INCORRECT VALUE FOR R1.D" );
          END IF;

          IF R2.D /= 0 THEN
               FAILED ( "INCORRECT VALUE FOR R2.D" );
          END IF;
     END;

     DECLARE

          PACKAGE PRIV IS
               TYPE REC3 (  D : INTEGER := F (TRUE,3)  ) IS PRIVATE;
               TYPE REC4 (  D : INTEGER := F (FALSE,4) ) IS PRIVATE;

          PRIVATE
               TYPE REC3 (  D : INTEGER := F (TRUE,3)  ) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE REC4 (  D : INTEGER := F (FALSE,4)  ) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PRIV;

          USE PRIV;

     BEGIN
          DECLARE
               R3 : REC3;
               R4 : REC4 (D => 0);

          BEGIN
               IF R3.D /= 1 THEN
                    FAILED ( "INCORRECT VALUE FOR R3.D" );
               END IF;

               IF R4.D /= 0 THEN
                    FAILED ( "INCORRECT VALUE FOR R4.D" );
               END IF;
          END;

     END;

     DECLARE

          PACKAGE LPRIV IS
               TYPE  REC5
                     (  D : INTEGER := F (TRUE,5)  ) IS LIMITED PRIVATE;
               TYPE  REC6
                     (  D : INTEGER := F (FALSE,6) ) IS LIMITED PRIVATE;

          PRIVATE
               TYPE REC5 (  D : INTEGER := F (TRUE,5)  ) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE REC6 (  D : INTEGER := F (FALSE,6) ) IS
                    RECORD
                         NULL;
                    END RECORD;
          END LPRIV;

          USE LPRIV;

     BEGIN
          DECLARE
               R5 : REC5;
               R6 : REC6 (D => 0);

          BEGIN
               IF R5.D /= 1 THEN
                    FAILED ( "INCORRECT VALUE FOR R5.D" );
               END IF;

               IF R6.D /= 0 THEN
                    FAILED ( "INCORRECT VALUE FOR R6.D" );
               END IF;
          END;

     END;

     RESULT;
END C37107A;
