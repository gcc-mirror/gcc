-- C54A42D.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES A FEW ALTERNATIVES
--    COVERING A LARGE RANGE OF INTEGERS.


-- (OPTIMIZATION TEST.)


-- RM 03/30/81


WITH REPORT;
PROCEDURE  C54A42D  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42D" , "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
                       " A FEW ALTERNATIVES COVERING A LARGE RANGE" &
                       " OF INTEGERS" );

     DECLARE

          NUMBER  : CONSTANT          := 2000 ;
          LITEXPR : CONSTANT          := NUMBER + 2000 ;
          STATCON : CONSTANT INTEGER  := 2001 ;
          DYNVAR  :          INTEGER  := IDENT_INT( 0 );
          DYNCON  : CONSTANT INTEGER  := IDENT_INT( 1 );

     BEGIN

          CASE  INTEGER'(-4000)  IS
               WHEN  1..2000         =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  INTEGER'FIRST..0=>  NULL ;
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  2002..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE F4");
          END CASE;

          CASE  INTEGER'(NUMBER)   IS
               WHEN  1..2000         =>  NULL ;
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  2002..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE G4");
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  1..2000         =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  2002..INTEGER'LAST=>NULL ;
          END CASE;

          CASE  STATCON  IS
               WHEN  1..2000         =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE I2");
               WHEN  2001            =>  NULL ;
               WHEN  2002..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE I4");
          END CASE;

          CASE  DYNVAR   IS
               WHEN  1..2000         =>  FAILED("WRONG ALTERNATIVE J1");
               WHEN  INTEGER'FIRST..0=>  NULL ;
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  2002..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE J4");
          END CASE;

          CASE  DYNCON   IS
               WHEN  1..2000         =>  NULL ;
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE K2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  2002..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE K4");
          END CASE;

     END ;


     RESULT ;


END  C54A42D ;
