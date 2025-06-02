-- C54A07A.ADA

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
-- CHECK THAT A VARIABLE USED AS A CASE EXPRESSION IS NOT CONSIDERED
--    LOCAL TO THE CASE STATEMENT.  IN PARTICULAR, CHECK THAT THE
--    VARIABLE CAN BE ASSIGNED A NEW VALUE, AND THE ASSIGNMENT TAKES
--    EFFECT IMMEDIATELY (I.E. THE CASE STATEMENT DOES NOT USE A
--    COPY OF THE CASE EXPRESSION).


-- RM 01/21/80


WITH REPORT ;
PROCEDURE  C54A07A  IS

     USE REPORT ;

BEGIN

     TEST("C54A07A" , "CHECK THAT A VARIABLE USED AS A CASE" &
                      " EXPRESSION IS NOT CONSIDERED LOCAL TO" &
                      " THE CASE STATEMENT" );

     DECLARE   -- A
     BEGIN

B1 :      DECLARE

               TYPE  VARIANT_REC( DISCR : BOOLEAN := TRUE )  IS
                    RECORD
                         A , B : INTEGER ;
                         CASE  DISCR  IS
                              WHEN  TRUE   =>  P , Q : CHARACTER ;
                              WHEN  FALSE  =>  X , Y : INTEGER   ;
                         END CASE;
                    END RECORD ;

               V : VARIANT_REC := ( TRUE , 1 , 2 ,
                                           IDENT_CHAR( 'P' )   ,
                                           IDENT_CHAR( 'Q' )   );

          BEGIN

               IF  EQUAL( 3 , 7 )  THEN  V := ( FALSE , 3 , 4 , 7 , 8 );
               END IF;

               CASE  V.DISCR  IS

                    WHEN  TRUE    =>  

                         IF  ( V.P /= 'P'  OR
                               V.Q /= 'Q'  )
                         THEN  FAILED( "WRONG VALUES  -  1" );
                         END IF;

                         B1.V  :=  ( FALSE , 3 , 4 ,
                                             IDENT_INT( 5 )   ,
                                             IDENT_INT( 6 )   );

                         IF  V.DISCR  THEN FAILED( "WRONG DISCR." );
                         END IF;

                         IF  ( V.X /= 5  OR
                               V.Y /= 6  )
                         THEN  FAILED( "WRONG VALUES  -  2" );
                         END IF;

                    WHEN  FALSE   =>  
                         FAILED( "WRONG BRANCH IN CASE STMT." );

               END CASE;

          EXCEPTION

               WHEN  OTHERS  =>  FAILED("EXCEPTION RAISED");

          END B1 ;

     EXCEPTION

          WHEN  OTHERS  =>  FAILED( "EXCEPTION RAISED BY DECLARATIONS");

     END ;    -- A


     RESULT ;


END C54A07A ;
