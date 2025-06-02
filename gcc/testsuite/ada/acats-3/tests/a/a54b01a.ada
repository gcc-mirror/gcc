-- A54B01A.ADA

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
-- CHECK THAT IF A CASE EXPRESSION IS A CONSTANT, VARIABLE,
--    TYPE CONVERSION, OR QUALIFIED EXPRESSION,
--    AND THE SUBTYPE OF THE
--    EXPRESSION IS STATIC, AN  'OTHERS'  CAN BE OMITTED IF ALL
--    VALUES IN THE SUBTYPE'S RANGE ARE COVERED.


-- RM 01/23/80
-- SPS 10/26/82
-- SPS 2/1/83

WITH REPORT ;
PROCEDURE  A54B01A  IS

     USE REPORT ;

BEGIN                                                       

     TEST("A54B01A" , "CHECK THAT IF" &
                      " THE SUBTYPE OF A CASE EXPRESSION IS STATIC," &
                      " AN  'OTHERS'  CAN BE OMITTED IF ALL" &
                      " VALUES IN THE SUBTYPE'S RANGE ARE COVERED" );

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --    I.   CONSTANTS
     --
     --    II.  STATIC SUBRANGES
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (B)    QUALIFIED EXPRESSIONS
     --         (C)    TYPE CONVERSIONS

     DECLARE  -- CONSTANTS
          T : CONSTANT BOOLEAN := TRUE;
          FIVE : CONSTANT INTEGER := IDENT_INT(5);
     BEGIN

          CASE  FIVE  IS
               WHEN  INTEGER'FIRST..4  =>  NULL ;
               WHEN  5                 =>  NULL ; 
               WHEN  6 .. INTEGER'LAST =>  NULL ; 
          END CASE;                                         
                                                            
          CASE  T  IS
               WHEN  TRUE              =>  NULL ;
               WHEN  FALSE             =>  NULL ;
          END CASE;

     END ;


     DECLARE   --  STATIC SUBRANGES
          
          SUBTYPE  STAT  IS  INTEGER RANGE 1..5 ;
          I   : INTEGER RANGE 1..5 ;
          J   : STAT ;
          BOOL: BOOLEAN := FALSE ;
          CHAR: CHARACTER := 'U' ;
          TYPE  ENUMERATION  IS  ( FIRST,SECOND,THIRD,FOURTH,FIFTH );
          ENUM: ENUMERATION := THIRD ;


     BEGIN

          I  :=  IDENT_INT( 2 );
          J  :=  IDENT_INT( 2 );

          CASE  I  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
          END CASE;

          CASE  BOOL  IS
               WHEN  TRUE   =>  NULL ;
               WHEN  FALSE  =>  NULL ;
          END CASE;

          CASE  STAT'( 2 )  IS
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
          END CASE;

          CASE  STAT( J )  IS
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
          END CASE;


     END ;     --  STATIC SUBRANGES

     RESULT ;


END A54B01A ;
