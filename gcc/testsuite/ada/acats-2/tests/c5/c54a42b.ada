-- C54A42B.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES A SMALL RANGE OF
--    POTENTIAL VALUES GROUPED INTO A SMALL NUMBER OF ALTERNATIVES.

-- (OPTIMIZATION TEST -- JUMP TABLE.)


-- RM 03/26/81
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.


WITH REPORT;
PROCEDURE  C54A42B  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42B" , "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
                       " A SMALL NUMBER OF POTENTIAL VALUES GROUPED"   &
                       " INTO A SMALL NUMBER OF ALTERNATIVES" );

     DECLARE

          STATCON : CONSTANT CHARACTER RANGE 'A'..'K' := 'J' ;
          STATVAR :          CHARACTER RANGE 'A'..'K' := 'A' ;
          DYNCON  : CONSTANT CHARACTER RANGE 'A'..'K' :=IDENT_CHAR('K');
          DYNVAR  :          CHARACTER RANGE 'A'..'K' :=IDENT_CHAR('G');

     BEGIN

          CASE  STATVAR  IS
               WHEN  'B' | 'E'      => FAILED( "WRONG ALTERNATIVE A1" );
               WHEN  'J' | 'C'      => FAILED( "WRONG ALTERNATIVE A2" );
               WHEN  'F'            => FAILED( "WRONG ALTERNATIVE A3" );
               WHEN  'D' | 'H'..'I' => FAILED( "WRONG ALTERNATIVE A4" );
               WHEN  'G'            => FAILED( "WRONG ALTERNATIVE A5" );
               WHEN  OTHERS         => NULL ;
          END CASE;

          CASE  CHARACTER'('B')  IS
               WHEN  'B' | 'E'      => NULL ;
               WHEN  'J' | 'C'      => FAILED( "WRONG ALTERNATIVE B2" );
               WHEN  'F'            => FAILED( "WRONG ALTERNATIVE B3" );
               WHEN  'D' | 'H'..'I' => FAILED( "WRONG ALTERNATIVE B4" );
               WHEN  'G'            => FAILED( "WRONG ALTERNATIVE B5" );
               WHEN  OTHERS         => FAILED( "WRONG ALTERNATIVE B6" );
          END CASE;

          CASE  DYNVAR   IS
               WHEN  'B' | 'E'      => FAILED( "WRONG ALTERNATIVE C1" );
               WHEN  'J' | 'C'      => FAILED( "WRONG ALTERNATIVE C2" );
               WHEN  'F'            => FAILED( "WRONG ALTERNATIVE C3" );
               WHEN  'D' | 'H'..'I' => FAILED( "WRONG ALTERNATIVE C4" );
               WHEN  'G'            => NULL ;
               WHEN  OTHERS         => FAILED( "WRONG ALTERNATIVE C6" );
          END CASE;

          CASE  IDENT_CHAR(STATCON)  IS
               WHEN  'B' | 'E'      => FAILED( "WRONG ALTERNATIVE D1" );
               WHEN  'J' | 'C'      => NULL ;
               WHEN  'F'            => FAILED( "WRONG ALTERNATIVE D3" );
               WHEN  'D' | 'H'..'I' => FAILED( "WRONG ALTERNATIVE D4" );
               WHEN  'G'            => FAILED( "WRONG ALTERNATIVE D5" );
               WHEN  OTHERS         => FAILED( "WRONG ALTERNATIVE D6" );
          END CASE;

          CASE  DYNCON   IS
               WHEN  'B' | 'E'      => FAILED( "WRONG ALTERNATIVE E1" );
               WHEN  'J' | 'C'      => FAILED( "WRONG ALTERNATIVE E2" );
               WHEN  'F'            => FAILED( "WRONG ALTERNATIVE E3" );
               WHEN  'D' | 'H'..'I' => FAILED( "WRONG ALTERNATIVE E4" );
               WHEN  'G'            => FAILED( "WRONG ALTERNATIVE E5" );
               WHEN  OTHERS         => NULL ;
          END CASE;

     END ;


     DECLARE

          NUMBER  : CONSTANT                      := 1 ;
          LITEXPR : CONSTANT                      := NUMBER + 5 ;
          STATCON : CONSTANT INTEGER RANGE 0..10  := 9 ;
          DYNVAR  :          INTEGER RANGE 0..10  := IDENT_INT( 10 );
          DYNCON  : CONSTANT INTEGER RANGE 0..10  := IDENT_INT(  2 );

     BEGIN

          CASE  INTEGER'(0)  IS
               WHEN  1 | 4           =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  9 | 2           =>  FAILED("WRONG ALTERNATIVE F2");
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE F4");
               WHEN  6               =>  FAILED("WRONG ALTERNATIVE F5");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  INTEGER'(NUMBER)   IS
               WHEN  1 | 4           =>  NULL ;
               WHEN  9 | 2           =>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE G4");
               WHEN  6               =>  FAILED("WRONG ALTERNATIVE G5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE G6");
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  1 | 4           =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  9 | 2           =>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE H4");
               WHEN  6               =>  NULL ;
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE H6");
          END CASE;

          CASE  STATCON  IS
               WHEN  1 | 4           =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  9 | 2           =>  NULL ;
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE I3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE I4");
               WHEN  6               =>  FAILED("WRONG ALTERNATIVE I5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE I6");
          END CASE;

          CASE  DYNVAR   IS
               WHEN  1 | 4           =>  FAILED("WRONG ALTERNATIVE J1");
               WHEN  9 | 2           =>  FAILED("WRONG ALTERNATIVE J2");
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE J4");
               WHEN  6               =>  FAILED("WRONG ALTERNATIVE J5");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  DYNCON  IS
               WHEN  1 | 4           =>  FAILED("WRONG ALTERNATIVE K1");
               WHEN  9 | 2           =>  NULL ;
               WHEN  5               =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  3 | 7..8        =>  FAILED("WRONG ALTERNATIVE K4");
               WHEN  6               =>  FAILED("WRONG ALTERNATIVE K5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE K6");
          END CASE;

     END ;


     RESULT ;


END  C54A42B ;
