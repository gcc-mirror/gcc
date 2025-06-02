-- A54B02A.ADA

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
-- CHECK THAT IF A CASE EXPRESSION IS A VARIABLE, CONSTANT, TYPE 
--    CONVERSION, ATTRIBUTE (IN PARTICULAR 'FIRST AND 'LAST),
--    FUNCTION INVOCATION, QUALIFIED EXPRESSION, OR A PARENTHESIZED
--    EXPRESSION HAVING ONE OF THESE FORMS, AND THE SUBTYPE OF THE
--    EXPRESSION IS NON-STATIC, AN  'OTHERS'  CAN BE OMITTED IF ALL
--    VALUES IN THE BASE TYPE'S RANGE ARE COVERED.

-- RM 01/27/80
-- SPS 10/26/82
-- SPS 2/2/83
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

WITH REPORT ;
PROCEDURE  A54B02A  IS

     USE REPORT ;

BEGIN                                                       

     TEST("A54B02A" , "CHECK THAT IF THE" &
                      " SUBTYPE OF A CASE EXPRESSION IS NON-STATIC," &
                      " AN  'OTHERS'  CAN BE OMITTED IF ALL" &
                      " VALUES IN THE BASE TYPE'S RANGE ARE COVERED" );

     -- THE TEST CASES APPEAR IN THE FOLLOWING ORDER:
     --
     --         (A)    VARIABLES (INTEGER , BOOLEAN)
     --         (B)    CONSTANTS (INTEGER, BOOLEAN)
     --         (C)    ATTRIBUTES ('FIRST, 'LAST)    
     --         (D)    FUNCTION CALLS
     --         (E)    QUALIFIED EXPRESSIONS
     --         (F)    TYPE CONVERSIONS
     --         (G)    PARENTHESIZED EXPRESSIONS OF THE ABOVE KINDS


     DECLARE   --  NON-STATIC RANGES
          
          SUBTYPE  STAT   IS  INTEGER RANGE 1..50 ;
          SUBTYPE  DYN    IS  STAT    RANGE 1..IDENT_INT( 5 ) ;
          I   : STAT RANGE 1..IDENT_INT( 5 );
          J   : DYN ;
          SUBTYPE  DYNCHAR  IS
                CHARACTER RANGE ASCII.NUL .. IDENT_CHAR('Q');
          SUBTYPE  STATCHAR  IS
                DYNCHAR RANGE 'A' .. 'C' ;
          CHAR: DYNCHAR := 'F' ;
          TYPE  ENUMERATION  IS  ( A,B,C,D,E,F,G,H,K,L,M,N );
          SUBTYPE  STATENUM  IS
                ENUMERATION RANGE  A .. L ;
          SUBTYPE  DYNENUM  IS
                STATENUM  RANGE  A .. ENUMERATION'VAL(IDENT_INT(5));
          ENUM: DYNENUM := B ;
          CONS : CONSTANT DYN := 3;

          FUNCTION  FF  RETURN DYN  IS
          BEGIN
               RETURN  2 ;
          END  FF ;

     BEGIN

          I  :=  IDENT_INT( 2 );
          J  :=  IDENT_INT( 2 );

          CASE  I  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;

          CASE  J  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;

          CASE CONS IS
               WHEN  INTEGER'FIRST..INTEGER'LAST  =>  NULL;
          END CASE;

          CASE DYN'FIRST IS
               WHEN INTEGER'FIRST..0  =>  NULL;
               WHEN 1..INTEGER'LAST  =>  NULL;
          END CASE;

          CASE STATCHAR'LAST IS
               WHEN CHARACTER'FIRST..'A'  =>  NULL;
               WHEN 'B'..CHARACTER'LAST  =>  NULL;
          END CASE;

          CASE  FF  IS
               WHEN  4..5  =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
               WHEN  1..3  =>  NULL ;
          END CASE;

          CASE  DYN'( 2 )  IS
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
          END CASE;

          CASE  DYN( J )  IS
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;


          CASE  ( CHAR )  IS
               WHEN  ASCII.NUL .. 'P'  =>  NULL ;
               WHEN  'Q'               =>  NULL ; 
               WHEN  'R' .. 'Y'        =>  NULL ; 
               WHEN  'Z' .. CHARACTER'LAST  =>  NULL ;
          END CASE;

          CASE  ( ENUM )  IS
               WHEN  A | C | E  =>  NULL ;
               WHEN  B | D      =>  NULL ;
               WHEN  F .. L     =>  NULL ;
               WHEN  M .. N     =>  NULL ;
          END CASE;

          CASE  ( FF )  IS
               WHEN  1 | 3 | 5  =>  NULL ;
               WHEN  2 | 4      =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;

          CASE  ( DYN'( I ) )  IS
               WHEN  4..5  =>  NULL ;
               WHEN  1..3  =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;

          CASE  ( DYN( 2 ) )  IS
               WHEN  5 | 2..4  =>  NULL ;
               WHEN  1         =>  NULL ;
               WHEN  INTEGER'FIRST..0 | 6..INTEGER'LAST  =>  NULL ;
          END CASE;

          CASE (CONS) IS
               WHEN 1..100  =>  NULL;
               WHEN INTEGER'FIRST..0  =>  NULL;
               WHEN 101..INTEGER'LAST  =>  NULL;
          END CASE;

          CASE (DYNCHAR'LAST) IS
               WHEN 'B'..'Y'  =>  NULL;
               WHEN CHARACTER'FIRST..'A'  =>  NULL;
               WHEN 'Z'..CHARACTER'LAST  =>  NULL;
          END CASE;

     END;


     RESULT ;


END A54B02A ;
