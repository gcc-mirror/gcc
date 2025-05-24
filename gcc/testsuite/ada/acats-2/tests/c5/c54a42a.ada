-- C54A42A.ADA

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
-- CHECK THAT A  CASE_STATEMENT  MAY HANDLE A LARGE NUMBER OF
--    POTENTIAL VALUES GROUPED INTO A SMALL NUMBER OF ALTERNATIVES
--    AND THAT EACH TIME THE APPROPRIATE ALTERNATIVE IS EXECUTED.

-- (OPTIMIZATION TEST.)


-- RM 03/24/81
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.


WITH REPORT;
PROCEDURE  C54A42A  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42A" , "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
                       " A LARGE NUMBER OF POTENTIAL VALUES GROUPED"   &
                       " INTO A SMALL NUMBER OF ALTERNATIVES" );

     DECLARE

          STATCON : CONSTANT CHARACTER := 'B' ;
          STATVAR :          CHARACTER := 'Q' ;
          DYNCON  : CONSTANT CHARACTER := IDENT_CHAR( 'Y' );
          DYNVAR  :          CHARACTER := IDENT_CHAR( 'Z' );

     BEGIN

          CASE  CHARACTER'('A')  IS
               WHEN  ASCII.NUL .. 'A'  =>  NULL ;
               WHEN  'B'               =>  FAILED( "WRONG ALTERN. A2" );
               WHEN  'P'               =>  FAILED( "WRONG ALTERN. A3" );
               WHEN  'Y'               =>  FAILED( "WRONG ALTERN. A4" );
               WHEN  'Z' .. ASCII.DEL  =>  FAILED( "WRONG ALTERN. A5" );
               WHEN  OTHERS            =>  FAILED( "WRONG ALTERN. A6" );
          END CASE;

          CASE  STATCON  IS
               WHEN  ASCII.NUL .. 'A'  =>  FAILED( "WRONG ALTERN. B1" );
               WHEN  'B'               =>  NULL ;
               WHEN  'P'               =>  FAILED( "WRONG ALTERN. B3" );
               WHEN  'Y'               =>  FAILED( "WRONG ALTERN. B4" );
               WHEN  'Z' .. ASCII.DEL  =>  FAILED( "WRONG ALTERN. B5" );
               WHEN  OTHERS            =>  FAILED( "WRONG ALTERN. B6" );
          END CASE;

          CASE  STATVAR  IS
               WHEN  ASCII.NUL .. 'A'  =>  FAILED( "WRONG ALTERN. C1" );
               WHEN  'B'               =>  FAILED( "WRONG ALTERN. C2" );
               WHEN  'P'               =>  FAILED( "WRONG ALTERN. C3" );
               WHEN  'Y'               =>  FAILED( "WRONG ALTERN. C4" );
               WHEN  'Z' .. ASCII.DEL  =>  FAILED( "WRONG ALTERN. C5" );
               WHEN  OTHERS            =>  NULL ;
          END CASE;

          CASE  DYNCON  IS
               WHEN  ASCII.NUL .. 'A'  =>  FAILED( "WRONG ALTERN. D1" );
               WHEN  'B'               =>  FAILED( "WRONG ALTERN. D2" );
               WHEN  'P'               =>  FAILED( "WRONG ALTERN. D3" );
               WHEN  'Y'               =>  NULL ;
               WHEN  'Z' .. ASCII.DEL  =>  FAILED( "WRONG ALTERN. D5" );
               WHEN  OTHERS            =>  FAILED( "WRONG ALTERN. D6" );
          END CASE;

          CASE  DYNVAR  IS
               WHEN  ASCII.NUL .. 'A'  =>  FAILED( "WRONG ALTERN. E1" );
               WHEN  'B'               =>  FAILED( "WRONG ALTERN. E2" );
               WHEN  'P'               =>  FAILED( "WRONG ALTERN. E3" );
               WHEN  'Y'               =>  FAILED( "WRONG ALTERN. E4" );
               WHEN  'Z' .. ASCII.DEL  =>  NULL ;
               WHEN  OTHERS            =>  FAILED( "WRONG ALTERN. E6" );
          END CASE;

     END ;


     DECLARE

          NUMBER  : CONSTANT           := -100 ;
          LITEXPR : CONSTANT           := 0 * NUMBER  + 16 ;
          STATCON : CONSTANT INTEGER   := +100 ;
          DYNVAR  :          INTEGER   := IDENT_INT( 102 ) ;
          DYNCON  : CONSTANT INTEGER   := IDENT_INT(  17 ) ;

     BEGIN

          CASE  INTEGER'(-102)  IS
               WHEN  INTEGER'FIRST..-101 => NULL ;
               WHEN  -100                => FAILED("WRONG ALTERN. F2");
               WHEN  17                  => FAILED("WRONG ALTERN. F2");
               WHEN  100                 => FAILED("WRONG ALTERN. F4");
               WHEN  101..INTEGER'LAST   => FAILED("WRONG ALTERN. F5");
               WHEN  OTHERS              => FAILED("WRONG ALTERN. F6");
          END CASE;

          CASE  IDENT_INT(NUMBER)  IS
               WHEN  INTEGER'FIRST..-101 => FAILED("WRONG ALTERN. G1");
               WHEN  -100                => NULL ;
               WHEN  17                  => FAILED("WRONG ALTERN. G3");
               WHEN  100                 => FAILED("WRONG ALTERN. G4");
               WHEN  101..INTEGER'LAST   => FAILED("WRONG ALTERN. G5");
               WHEN  OTHERS              => FAILED("WRONG ALTERN. G6");
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  INTEGER'FIRST..-101 => FAILED("WRONG ALTERN. H1");
               WHEN  -100                => FAILED("WRONG ALTERN. H2");
               WHEN  17                  => FAILED("WRONG ALTERN. H3");
               WHEN  100                 => FAILED("WRONG ALTERN. H4");
               WHEN  101..INTEGER'LAST   => FAILED("WRONG ALTERN. H5");
               WHEN  OTHERS              => NULL ;
          END CASE;

          CASE  STATCON  IS
               WHEN  INTEGER'FIRST..-101 => FAILED("WRONG ALTERN. I1");
               WHEN  -100                => FAILED("WRONG ALTERN. I2");
               WHEN  17                  => FAILED("WRONG ALTERN. I3");
               WHEN  100                 => NULL ;
               WHEN  101..INTEGER'LAST   => FAILED("WRONG ALTERN. I5");
               WHEN  OTHERS              => FAILED("WRONG ALTERN. I6");
          END CASE;

          CASE  DYNVAR   IS
               WHEN  INTEGER'FIRST..-101 => FAILED("WRONG ALTERN. J1");
               WHEN  -100                => FAILED("WRONG ALTERN. J2");
               WHEN  17                  => FAILED("WRONG ALTERN. J3");
               WHEN  100                 => FAILED("WRONG ALTERN. J4");
               WHEN  101..INTEGER'LAST   => NULL ;
               WHEN  OTHERS              => FAILED("WRONG ALTERN. J6");
          END CASE;

          CASE  DYNCON  IS
               WHEN  INTEGER'FIRST..-101 => FAILED("WRONG ALTERN. K1");
               WHEN  -100                => FAILED("WRONG ALTERN. K2");
               WHEN  17                  => NULL ;
               WHEN  100                 => FAILED("WRONG ALTERN. K4");
               WHEN  101..INTEGER'LAST   => FAILED("WRONG ALTERN. K5");
               WHEN  OTHERS              => FAILED("WRONG ALTERN. K6");
          END CASE;
     END ;


     RESULT ;


END  C54A42A ;
