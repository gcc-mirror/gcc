-- C83E02A.ADA

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
-- CHECK THAT WITHIN THE BODY OF A SUBPROGRAM A FORMAL PARAMETER CAN BE
--    USED DIRECTLY IN A RANGE CONSTRAINT, A DISCRIMINANT CONSTRAINT,
--    AND AN INDEX CONSTRAINT.

--    RM    8 JULY 1980


WITH REPORT;
PROCEDURE  C83E02A  IS

     USE REPORT;

     Z : INTEGER := 0 ;

     PROCEDURE  P1 ( A , B : INTEGER;  C : IN OUT INTEGER ) IS
          X : INTEGER RANGE A+1..1+B ;
     BEGIN
          X := A + 1 ;
          C := X * B + B * X * A ;         -- 4*3+3*4*3=48
     END ;

     PROCEDURE  P2 ( A , B : INTEGER;  C : IN OUT INTEGER ) IS
          TYPE  T (MAX : INTEGER)  IS
               RECORD
                    VALUE : INTEGER RANGE 1..3 ;
               END RECORD ;
          X : T(A);
     BEGIN
          X := ( MAX => 4 , VALUE => B ) ; -- ( 4 , 3 )
          C := 10*C + X.VALUE + 2 ;        -- 10*48+3+2=485
     END ;

     FUNCTION  F3 ( A , B : INTEGER )  RETURN INTEGER  IS
          TYPE  TABLE  IS  ARRAY( A..B ) OF INTEGER ;
          X : TABLE ;
          Y : ARRAY( A..B ) OF INTEGER ;
     BEGIN
          X(A) := A ;                      -- 5
          Y(B) := B ;                      -- 6
          RETURN X(A)-Y(B)+4 ;             -- 3
     END ;


BEGIN

     TEST( "C83E02A" , "CHECK THAT WITHIN THE BODY OF A SUBPROGRAM " &
                       " A FORMAL PARAMETER CAN BE USED DIRECTLY IN" &
                       " A RANGE CONSTRAINT, A DISCRIMINANT CONSTRAINT"&
                       ", AND AN INDEX CONSTRAINT" ) ;

     P1 ( 3 , 3 , Z );                     --  Z  BECOMES  48
     P2 ( 4 , F3( 5 , 6 ) , Z );           --  Z  BECOMES 485

     IF  Z /= 485  THEN
          FAILED( "ACCESSING ERROR OR COMPUTATION ERROR" );
     END IF;

     RESULT;

END C83E02A;
