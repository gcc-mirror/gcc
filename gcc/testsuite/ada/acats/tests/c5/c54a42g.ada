-- C54A42G.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES SEVERAL NON-CONTIGUOUS
--    RANGES OF INTEGERS COVERED BY A SINGLE  'OTHERS'  ALTERNATIVE.


-- (OPTIMIZATION TEST.)


-- RM 03/30/81


WITH REPORT;
PROCEDURE  C54A42G  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42G" , "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
                       " SEVERAL NON-CONTIGUOUS RANGES OF INTEGERS"    &
                       " COVERED BY A SINGLE  'OTHERS'  ALTERNATIVE"  );

     DECLARE

          NUMBER  : CONSTANT          := 2000 ;
          LITEXPR : CONSTANT          := NUMBER + 2000 ;
          STATCON : CONSTANT INTEGER  := 2002 ;
          DYNVAR  :          INTEGER  := IDENT_INT( 0 );
          DYNCON  : CONSTANT INTEGER  := IDENT_INT( 1 );

     BEGIN

          CASE  INTEGER'(-4000)  IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  INTEGER'FIRST..0=>  NULL ;
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE F4");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE F5");
          END CASE;

          CASE  IDENT_INT(NUMBER)   IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE G1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE G4");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  2100..INTEGER'LAST=>NULL ;
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE H5");
          END CASE;

          CASE  IDENT_INT(STATCON)  IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE I2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE I3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE I4");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  DYNVAR   IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE J1");
               WHEN  INTEGER'FIRST..0=>  NULL ;
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE J4");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE J5");
          END CASE;

          CASE  DYNCON   IS
               WHEN  100..1999       =>  FAILED("WRONG ALTERNATIVE K1");
               WHEN  INTEGER'FIRST..0=>  FAILED("WRONG ALTERNATIVE K2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE K4");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  IDENT_INT( -3900 )  IS
               WHEN  -3000..1999     =>  FAILED("WRONG ALTERNATIVE X1");
               WHEN  INTEGER'FIRST..
                           -4000     =>  FAILED("WRONG ALTERNATIVE X2");
               WHEN  2001            =>  FAILED("WRONG ALTERNATIVE X3");
               WHEN  2100..INTEGER'LAST=>FAILED("WRONG ALTERNATIVE X4");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

     END ;


     RESULT ;


END  C54A42G ;
