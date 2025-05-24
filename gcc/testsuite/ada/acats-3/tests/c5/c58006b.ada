-- C58006B.ADA

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
-- CHECK THAT IF THE EVALUATION OF A RETURN STATEMENT'S EXPRESSION
-- RAISES AN EXCEPTION, THE EXCEPTION CAN BE HANDLED WITHIN THE BODY OF
-- THE FUNCTION. 

-- CHECKS GENERIC FUNCTIONS.

-- SPS 3/8/83
-- JBG 9/13/83

WITH REPORT;
PROCEDURE  C58006B  IS

     USE  REPORT;

BEGIN

     TEST( "C58006B" , "CHECK THAT EXCEPTION RAISED BY A RETURN" &
                       " STATEMENT CAN BE HANDLED LOCALLY" );


     DECLARE
          SUBTYPE I1 IS INTEGER RANGE -10..90;
          SUBTYPE I2 IS INTEGER RANGE 1..10; 

          GENERIC
          FUNCTION FN1 (X : I1) RETURN I2;

          GENERIC
          FUNCTION FN2 (X : I1) RETURN I2;

          GENERIC
          FUNCTION FN3 (X : I1) RETURN I2;

          FUNCTION  FN1( X : I1 )
                    RETURN  I2  IS
          BEGIN
               RETURN  0;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    COMMENT ("EXCEPTION RAISED - F1");
                    RETURN 1;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FN1");
          END  FN1;

          FUNCTION  FN2( X : I1 )
                    RETURN  I2  IS
          BEGIN
               RETURN  X + IDENT_INT(0);
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    COMMENT ("EXCEPTION RAISED - F2");
                    RETURN 1;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FN2");
          END  FN2;

          FUNCTION  FN3( X : I1 )
                    RETURN  I2  IS
               HUNDRED : INTEGER RANGE -100..100 := IDENT_INT(100);
          BEGIN
               RETURN  HUNDRED;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    COMMENT ("EXCEPTION RAISED - F3");
                    RETURN 1;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FN3");
          END  FN3;

          FUNCTION F1 IS NEW FN1;
          FUNCTION F2 IS NEW FN2;
          FUNCTION F3 IS NEW FN3;

     BEGIN

          BEGIN
               IF F1( 0 ) /= IDENT_INT(1) THEN
                    FAILED ("NO EXCEPTION RAISED - F1( 0 )");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION PROPAGATED - F1( 0 )");
          END;

          BEGIN
               IF F2( 0 ) /= IDENT_INT(1) THEN
                    FAILED ("NO EXCEPTION RAISED - F2( 0 )");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION PROPAGATED - F2( 0 )");
          END;

          BEGIN
               IF F2(11 ) /= IDENT_INT(1) THEN
                    FAILED ("NO EXCEPTION RAISED - F2(11 )");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION PROPAGATED - F2(11 )");
          END;

          BEGIN
               IF F3( 0 ) /= IDENT_INT(1) THEN
                    FAILED ("NO EXCEPTION RAISED - F3( 0 )");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION PROPAGATED - F3( 0 )");
          END;

     END;

     RESULT;

END C58006B;
