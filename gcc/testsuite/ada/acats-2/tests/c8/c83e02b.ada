-- C83E02B.ADA

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
--    USED IN AN EXCEPTION HANDLER.

--    RM   10 JULY 1980


WITH REPORT;
PROCEDURE  C83E02B  IS

     USE REPORT;

     Z : INTEGER := 0 ;

     PROCEDURE  P1 ( A , B : INTEGER;  C : IN OUT INTEGER ) IS
          E : EXCEPTION ;
     BEGIN
          RAISE  E ;
          FAILED( "FAILURE TO RAISE  E " );
     EXCEPTION
          WHEN  E  =>
               C := A + B ;
          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED" );
     END ;


BEGIN

     TEST( "C83E02B" , "CHECK THAT WITHIN THE BODY OF A SUBPROGRAM " &
                       " A FORMAL PARAMETER CAN BE USED IN AN EXCEP" &
                       "TION HANDLER" ) ;

     P1 ( 3 , 14 , Z );

     IF  Z /= 17  THEN
          FAILED( "ACCESSING ERROR OR COMPUTATION ERROR" );
     END IF;

     RESULT;

END C83E02B;
