-- C54A42E.ADA

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
-- CHECK THAT A  CASE_STATEMENT  CORRECTLY HANDLES A SMALL RANGE OF
--    POTENTIAL VALUES OF TYPE INTEGER, SITUATED FAR FROM  0  AND
--    GROUPED INTO A SMALL NUMBER OF ALTERNATIVES.

-- (OPTIMIZATION TEST -- BIASED JUMP TABLE.)


-- RM 03/26/81


WITH REPORT;
PROCEDURE  C54A42E  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42E" , "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
                       " A SMALL, FAR RANGE OF POTENTIAL VALUES OF" &
                       " TYPE INTEGER" );

     DECLARE

          NUMBER  : CONSTANT                           := 4001 ;
          LITEXPR : CONSTANT                           := NUMBER + 5 ;
          STATCON : CONSTANT INTEGER RANGE 4000..4010  := 4009 ;
          DYNVAR  :          INTEGER RANGE 4000..4010  :=
                                                      IDENT_INT( 4010 );
          DYNCON  : CONSTANT INTEGER RANGE 4000..4010  :=
                                                      IDENT_INT( 4002 );

     BEGIN

          CASE  INTEGER'(4000)  IS
               WHEN  4001 | 4004     =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  4009 | 4002     =>  FAILED("WRONG ALTERNATIVE F2");
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE F4");
               WHEN  4006            =>  FAILED("WRONG ALTERNATIVE F5");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  IDENT_INT(NUMBER)  IS
               WHEN  4001 | 4004     =>  NULL ;
               WHEN  4009 | 4002     =>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE G4");
               WHEN  4006            =>  FAILED("WRONG ALTERNATIVE G5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE G6");
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  4001 | 4004     =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  4009 | 4002     =>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE H4");
               WHEN  4006            =>  NULL ;
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE H6");
          END CASE;

          CASE  STATCON  IS
               WHEN  4001 | 4004     =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  4009 | 4002     =>  NULL ;
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE I3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE I4");
               WHEN  4006            =>  FAILED("WRONG ALTERNATIVE I5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE I6");
          END CASE;

          CASE  DYNVAR   IS
               WHEN  4001 | 4004     =>  FAILED("WRONG ALTERNATIVE J1");
               WHEN  4009 | 4002     =>  FAILED("WRONG ALTERNATIVE J2");
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE J4");
               WHEN  4006            =>  FAILED("WRONG ALTERNATIVE J5");
               WHEN  OTHERS          =>  NULL ;

          END CASE;

          CASE  DYNCON  IS
               WHEN  4001 | 4004     =>  FAILED("WRONG ALTERNATIVE K1");
               WHEN  4009 | 4002     =>  NULL ;
               WHEN  4005            =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  4003 |
                     4007..4008      =>  FAILED("WRONG ALTERNATIVE K4");
               WHEN  4006            =>  FAILED("WRONG ALTERNATIVE K5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE K6");
          END CASE;

     END ;


     RESULT ;


END  C54A42E ;
