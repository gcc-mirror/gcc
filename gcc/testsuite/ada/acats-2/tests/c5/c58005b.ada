-- C58005B.ADA

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
-- CHECK THAT WHEN A GENERIC FUNCTION IS READY TO RETURN CONTROL TO ITS 
--    INVOKER THE CONSTRAINTS ON THE RETURN VALUES ARE CHECKED, AND THAT
--    CONSTRAINT ERROR  IS THEN RAISED IF AND ONLY IF THE CONSTRAINTS
--    ARE NOT SATISFIED.

-- THIS TEST CHECKS THAT THE EXCEPTION IS RAISED UNDER THE APPROPRIATE
--    CONDITIONS; IT ALSO CHECKS THE IDENTITY OF THE EXCEPTION.  THE
--    PRECISE MOMENT AND PLACE THE EXCEPTION IS RAISED IS TESTED
--    ELSEWHERE.

-- SPS 3/10/83
-- JBG 9/13/83
-- AH  8/29/86  ADDED CALLS TO "FAILED" AFTER "IF" STATEMENTS.

WITH REPORT;
PROCEDURE  C58005B  IS

     USE  REPORT;

BEGIN

     TEST( "C58005B" , "CHECK THAT EXCEPTIONS ARE RAISED BY A RETURN"  &
                       " STATEMENT IF AND ONLY IF THE CONSTRAINTS ARE" &
                       " VIOLATED" );


     DECLARE
          SUBTYPE I1 IS INTEGER RANGE -10..90;
          SUBTYPE I2 IS INTEGER RANGE 1..10;

          GENERIC
          FUNCTION FN1 ( X : I1 ) RETURN I2;

          FUNCTION  FN1( X : I1 )
                    RETURN  I2  IS
          BEGIN
               RETURN X;
          END  FN1;

          FUNCTION F1 IS NEW FN1;

     BEGIN

          BEGIN
               IF F1(IDENT_INT(0)) IN I2 THEN
                    FAILED( "EXCEPTION NOT RAISED  -  1A" );
               ELSE
                    FAILED( "EXCEPTION NOT RAISED  -  1B" );
               END IF;          
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED( "WRONG EXCEPTION RAISED  -  1" );
          END;

          BEGIN
               IF F1(IDENT_INT(11)) IN I2 THEN
                    FAILED( "EXCEPTION NOT RAISED  -  2A" );
               ELSE
                    FAILED( "EXCEPTION NOT RAISED  -  2B" );
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED( "WRONG EXCEPTION RAISED  -  2" );
          END;

     END;

     RESULT;

END C58005B;
