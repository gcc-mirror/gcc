-- C58005A.ADA

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
-- CHECK THAT WHEN A FUNCTION IS READY TO RETURN CONTROL TO ITS INVOKER
--    THE CONSTRAINTS ON THE RETURN VALUES ARE CHECKED, AND THAT
--     CONSTRAINT ERROR  IS THEN RAISED IF AND ONLY IF THE CONSTRAINTS
--    ARE NOT SATISFIED.

-- THIS TEST CHECKS THAT THE EXCEPTION IS RAISED UNDER THE APPROPRIATE
--    CONDITIONS; IT ALSO CHECKS THE IDENTITY OF THE EXCEPTION.  THE
--    PRECISE MOMENT AND PLACE THE EXCEPTION IS RAISED IS TESTED
--    ELSEWHERE.


-- RM 05/14/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE  C58005A  IS

     USE  REPORT ;

     INTVAR  :  INTEGER ;

BEGIN

     TEST( "C58005A" , "CHECK THAT EXCEPTIONS ARE RAISED BY A RETURN"  &
                       " STATEMENT IF AND ONLY IF THE CONSTRAINTS ARE" &
                       " VIOLATED" );


     DECLARE
          SUBTYPE I1 IS INTEGER RANGE -10..90;
          SUBTYPE I2 IS INTEGER RANGE 1..10;
          FUNCTION  FN1( X : I1 )
                    RETURN  I2  IS
          BEGIN
               RETURN  0 ;
          END  FN1 ;

          FUNCTION  FN2( X : I1 )
                    RETURN  I2  IS
          BEGIN
               RETURN  X + IDENT_INT(0) ;
          END  FN2 ;

          FUNCTION  FN3( X : I1  )
                    RETURN  I2  IS
               HUNDRED : INTEGER RANGE -100..100 := IDENT_INT(100) ;
          BEGIN
               RETURN  HUNDRED - 90 ;
          END  FN3 ;

     BEGIN

          INTVAR := 0 ;

          BEGIN
               INTVAR := FN1( 0 ) + INTVAR ;  -- EXCEPTION.
               FAILED( "EXCEPTION NOT RAISED  -  1" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR => INTVAR := INTVAR + 10 ;
               WHEN OTHERS => FAILED( "WRONG EXCEPTION RAISED  -  1" ) ;
          END ;

          BEGIN
               INTVAR := FN2( 1 ) + INTVAR ; -- 10+1=11 -- NO EXCEPTION.
               INTVAR := INTVAR + 100 ;   -- 11+100=111
          EXCEPTION
               WHEN OTHERS => FAILED( "EXCEPTION RAISED  -  2" ) ;
          END ;

          BEGIN
               INTVAR := FN2(11 ) + INTVAR ;  -- EXCEPTION.
               FAILED( "EXCEPTION NOT RAISED  -  3" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR => INTVAR := INTVAR + 10 ; -- 121
               WHEN OTHERS => FAILED( "WRONG EXCEPTION RAISED  -  3" ) ;
          END ;

          BEGIN
               INTVAR := FN3( 0 ) + INTVAR ;--121+10=131 --NO EXCEPTION.
               INTVAR := INTVAR + 1000 ;-- 131+1000=1131
          EXCEPTION
               WHEN OTHERS => FAILED( "EXCEPTION RAISED  -  4" ) ;
          END ;


     END ;


     IF  INTVAR /= 1131  THEN
          FAILED("WRONG FLOW OF CONTROL" );
     END IF;


     RESULT ;


END C58005A;
