-- A55B13A.ADA

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
-- USING A  CASE_STATEMENT , CHECK THAT IF  L , R  ARE LITERALS
--    OF TYPE  T  (INTEGER, BOOLEAN, CHARACTER, USER-DEFINED
--    ENUMERATION TYPE)  THE SUBTYPE BOUNDS ASSOCIATED WITH A
--    LOOP OF THE FORM
--               FOR  I  IN  L..R  LOOP
--    ARE THE SAME AS THOSE FOR THE CORRESPONDING LOOP OF THE FORM
--               FOR  I  IN  T RANGE L..R  LOOP   .


-- RM 04/07/81
-- SPS 3/2/83
-- JBG 8/21/83
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

WITH REPORT ;
PROCEDURE  A55B13A  IS

     USE REPORT ;

BEGIN

     TEST("A55B13A" , "CHECK THAT THE SUBTYPE OF A LOOP PARAMETER"   &
                      " IN A LOOP OF THE FORM  'FOR  I  IN "         &
                      " LITERAL_L .. LITERAL_R  LOOP'  IS CORRECTLY" &
                      " DETERMINED" );

     DECLARE

          TYPE  ENUMERATION  IS  ( A,B,C,D,MIDPOINT,E,F,G,H );
          ONE   :  CONSTANT  :=  1 ;
          FIVE  :  CONSTANT  :=  5 ;


     BEGIN


          FOR  I  IN  1..5  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL ;
                    WHEN  2 | 4      =>  NULL ;
               END CASE;

          END LOOP;


          FOR  I  IN  REVERSE  ONE .. FIVE  LOOP

               CASE  I  IS
                    WHEN  1 | 3 | 5  =>  NULL ;
                    WHEN  2 | 4      =>  NULL ;
               END CASE;

          END LOOP;


          FOR  I  IN  REVERSE FALSE..TRUE  LOOP

               CASE  I  IS
                    WHEN  FALSE  =>  NULL ;
                    WHEN  TRUE   =>  NULL ;
               END CASE;

          END LOOP;


          FOR  I  IN  CHARACTER'('A') .. ASCII.DEL  LOOP

               CASE  I  IS
                    WHEN  CHARACTER'('A')..CHARACTER'('U')  =>  NULL ;
                    WHEN  CHARACTER'('V')..ASCII.DEL  =>  NULL ;
               END CASE;

          END LOOP;


          FOR  I  IN  CHARACTER'('A')..CHARACTER'('H')  LOOP

               CASE  I  IS
                    WHEN  CHARACTER'('A')..CHARACTER'('D')  =>  NULL ;
                    WHEN  CHARACTER'('E')..CHARACTER'('H')  =>  NULL ;
               END CASE;

          END LOOP;


          FOR  I  IN  REVERSE B..H  LOOP

               CASE  I  IS
                    WHEN  B..D      =>  NULL ;
                    WHEN  E..H      =>  NULL ;
                    WHEN  MIDPOINT  =>  NULL ;
               END CASE;

          END LOOP;


     END ;


     RESULT ;


END A55B13A ;
