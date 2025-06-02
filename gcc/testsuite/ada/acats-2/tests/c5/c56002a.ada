-- C56002A.ADA

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
-- CHECK THAT BLOCKS CAN HAVE DECLARATIVE PARTS AND THAT
--    THE EFFECT OF THESE DECLARATIONS IS LIMITED TO THE BLOCKS
--    IN WHICH THEY OCCUR.


-- RM 04/16/81
-- SPS 3/4/83

WITH REPORT;
PROCEDURE  C56002A  IS

     USE  REPORT ;

BEGIN

     TEST( "C56002A" , "BLOCKS CAN HAVE DECLARATIVE PARTS AND"         &
                       " THE EFFECT OF THESE DECLARATIONS IS LIMITED"  &
                       " TO THE BLOCKS IN WHICH THEY OCCUR" )          ;

     DECLARE

          FIRST   :  CONSTANT INTEGER := IDENT_INT( 1) ;
          SECOND  :  CONSTANT INTEGER := IDENT_INT( 2) ;
          THIRD   :  CONSTANT INTEGER := IDENT_INT( 3) ;
          FOURTH  :  CONSTANT INTEGER := IDENT_INT( 4) ;
          FIFTH   :  CONSTANT INTEGER := IDENT_INT( 5) ;
          TENTH   :  CONSTANT INTEGER := IDENT_INT(10) ;
          ZEROTH  :  CONSTANT INTEGER := IDENT_INT( 0) ;

     BEGIN

          IF   FIRST   /= 1  OR
               SECOND  /= 2  OR
               THIRD   /= 3  OR
               FOURTH  /= 4  OR
               FIFTH   /= 5  OR
               TENTH   /=10  OR
               ZEROTH  /= 0
          THEN
               FAILED( "WRONG VALUES  -  1" );
          END IF;

          DECLARE

               TYPE  ENUM   IS  ( AMINUS , A,B,C,D,E,  F,G,H,I,J );

               FIRST   :  CONSTANT ENUM := A ;
               SECOND  :  CONSTANT ENUM := B ;
               THIRD   :  CONSTANT ENUM := C ;
               FOURTH  :  CONSTANT ENUM := D ;
               FIFTH   :  CONSTANT ENUM := E ;
               TENTH   :  CONSTANT ENUM := J ;
               ZEROTH  :  CONSTANT ENUM := AMINUS ;

          BEGIN

               IF   FIRST   /= ENUM'VAL( IDENT_INT( 1 ) )  OR
                    SECOND  /= ENUM'VAL( IDENT_INT( 2 ) )  OR
                    THIRD   /= ENUM'VAL( IDENT_INT( 3 ) )  OR
                    FOURTH  /= ENUM'VAL( IDENT_INT( 4 ) )  OR
                    FIFTH   /= ENUM'VAL( IDENT_INT( 5 ) )  OR
                    TENTH   /= ENUM'VAL( IDENT_INT(10 ) )  OR
                    ZEROTH  /= ENUM'VAL( IDENT_INT( 0 ) )
               THEN
                    FAILED( "WRONG VALUES  -  2" );
               END IF;

          END ;

          IF   FIRST   /= 1  OR
               SECOND  /= 2  OR
               THIRD   /= 3  OR
               FOURTH  /= 4  OR
               FIFTH   /= 5  OR
               TENTH   /=10  OR
               ZEROTH  /= 0
          THEN
               FAILED( "WRONG VALUES  -  3" );
          END IF;

          DECLARE

               FIRST   :  CONSTANT CHARACTER := 'A' ;
               SECOND  :  CONSTANT CHARACTER := 'B' ;
               THIRD   :  CONSTANT CHARACTER := 'C' ;
               FOURTH  :  CONSTANT CHARACTER := 'D' ;
               FIFTH   :  CONSTANT CHARACTER := 'E' ;
               TENTH   :  CONSTANT CHARACTER := 'J' ;
               ZEROTH  :  CONSTANT CHARACTER := '0' ;--ZERO < ANY LETTER

          BEGIN

               IF   FIRST   /= IDENT_CHAR( 'A' )  OR
                    SECOND  /= IDENT_CHAR( 'B' )  OR
                    THIRD   /= IDENT_CHAR( 'C' )  OR
                    FOURTH  /= IDENT_CHAR( 'D' )  OR
                    FIFTH   /= IDENT_CHAR( 'E' )  OR
                    TENTH   /= IDENT_CHAR( 'J' )  OR
                    ZEROTH  /= IDENT_CHAR( '0' )
               THEN
                    FAILED( "WRONG VALUES  -  4" );
               END IF;

          END ;

          IF   FIRST   /= 1  OR
               SECOND  /= 2  OR
               THIRD   /= 3  OR
               FOURTH  /= 4  OR
               FIFTH   /= 5  OR
               TENTH   /=10  OR
               ZEROTH  /= 0
          THEN
               FAILED( "WRONG VALUES  -  5" );
          END IF;


     END ;


     RESULT ;


END  C56002A ;
