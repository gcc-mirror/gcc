-- C47006A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A FIXED POINT 
-- TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE VALUE OF THE 
-- OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47006A IS

     TYPE FIXED IS DELTA 0.5 RANGE -5.0 .. 5.0;

BEGIN

     TEST( "C47006A", "WHEN THE TYPE MARK IN A QUALIFIED " &
                      "EXPRESSION DENOTES A FIXED POINT TYPE, " &
                      "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
                      "WITHIN THE RANGE OF THE TYPE MARK" );

     DECLARE  

          SUBTYPE SFIXED IS FIXED RANGE -2.0 .. 2.0;

          FUNCTION IDENT (X : FIXED) RETURN FIXED IS
          BEGIN
               IF EQUAL (3, 3) THEN
                    RETURN X;
               ELSE
                    RETURN 0.0;
               END IF;
          END IDENT;

     BEGIN
          IF SFIXED'(IDENT (-5.0)) = -2.0 THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SFIXED - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SFIXED - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE SFIXED" );
     END;

     DECLARE  

          TYPE NFIX IS NEW FIXED;
          SUBTYPE SNFIX IS NFIX RANGE -2.0 .. 2.0;

          FUNCTION IDENT (X : NFIX) RETURN NFIX IS
          BEGIN
               RETURN NFIX (IDENT_INT (INTEGER (X)));
          END IDENT;

     BEGIN
          IF SNFIX'(IDENT (-5.0)) = -2.0 THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SNFIX - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SNFIX - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE SNFIX" );
     END;

     RESULT;
END C47006A;
