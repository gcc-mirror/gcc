-- C83E03A.ADA

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
-- CHECK THAT A FORMAL PARAMETER IN A NAMED PARAMETER ASSOCIATION
--    IS NOT CONFUSED WITH AN ACTUAL PARAMETER IDENTIFIER  HAVING THE
--    SAME SPELLING.


--    RM    23 JULY 1980


WITH REPORT;
PROCEDURE  C83E03A  IS

     USE REPORT;

     P : INTEGER RANGE 1..23 := 17 ;
     FLOW_INDEX : INTEGER := 0 ;

BEGIN

     TEST( "C83E03A" , "CHECK THAT A FORMAL PARAMETER IN A NAMED" &
                       " PARAMETER ASSOCIATION  IS NOT CONFUSED" &
                       " WITH AN ACTUAL PARAMETER HAVING THE" &
                       " SAME SPELLING" );

     DECLARE

          PROCEDURE  BUMP  IS
          BEGIN
               FLOW_INDEX := FLOW_INDEX + 1 ;
          END BUMP ;

          PROCEDURE  P1 ( P : INTEGER )  IS
          BEGIN
               IF  P = 17  THEN  BUMP ; END IF ;
          END ;

          FUNCTION  F1 ( P : INTEGER ) RETURN INTEGER  IS
          BEGIN
               RETURN  P ;
          END ;

     BEGIN

          P1 ( P );
          P1 ( P => P );

          IF  F1 ( P + 1 )      = 17 + 1  THEN  BUMP ;  END IF;
          IF  F1 ( P => P + 1 ) = 17 + 1  THEN  BUMP ;  END IF;

     END ;

     IF  FLOW_INDEX /= 4  THEN
          FAILED( "INCORRECT ACCESSING OR INCORRECT FLOW" );
     END IF;

     RESULT;

END C83E03A;
