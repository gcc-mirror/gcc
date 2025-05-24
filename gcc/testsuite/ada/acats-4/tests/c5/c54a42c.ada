-- C54A42C.ADA

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
-- CHECK THAT A  CASE_STATEMENT  CORRECTLY HANDLES A SPARSE SET OF
--    POTENTIAL VALUES (OF TYPE INTEGER) IN A LARGE RANGE.

--    (OPTIMIZATION TEST)


-- RM 03/26/81


WITH REPORT;
PROCEDURE  C54A42C  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42C" , "TEST THAT A  CASE_STATEMENT HANDLES CORRECTLY" &
                       " A SPARSE SET OF POTENTIAL VALUES IN A LARGE"  &
                       " RANGE" );

     DECLARE

          NUMBER  : CONSTANT                           := 1001 ;
          LITEXPR : CONSTANT                           := NUMBER + 998 ;
          STATCON : CONSTANT INTEGER RANGE  1..INTEGER'LAST :=    1000 ;
          DYNVAR  :          INTEGER RANGE  1..INTEGER'LAST :=
                                           IDENT_INT( INTEGER'LAST-50 );
          DYNCON  : CONSTANT INTEGER RANGE  1..INTEGER'LAST :=
                                           IDENT_INT( 1000 );

     BEGIN

          CASE  INTEGER'( NUMBER )  IS
               WHEN  1 .. 10         =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  1000            =>  FAILED("WRONG ALTERNATIVE F2");
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE F4");
               WHEN  INTEGER'LAST-100 ..
                     INTEGER'LAST    =>  FAILED("WRONG ALTERNATIVE F5");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  IDENT_INT( 10 )  IS
               WHEN  1 .. 10         =>  NULL ;
               WHEN  1000            =>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE G4");
               WHEN  INTEGER'LAST -100 ..
                     INTEGER'LAST    =>  FAILED("WRONG ALTERNATIVE G5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE G6");
          END CASE;

          CASE  IDENT_INT(LITEXPR)  IS
               WHEN  1 .. 10         =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  1000            =>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE H4");
               WHEN  INTEGER'LAST -100 ..
                     INTEGER'LAST    =>  FAILED("WRONG ALTERNATIVE H5");
               WHEN  OTHERS          =>  NULL ;
          END CASE;

          CASE  STATCON  IS
               WHEN  1 .. 10         =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  1000            =>  NULL ;
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE I3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE I4");
               WHEN  INTEGER'LAST -100 ..
                     INTEGER'LAST    =>  FAILED("WRONG ALTERNATIVE I5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE I6");
          END CASE;

          CASE  DYNVAR   IS
               WHEN  1 .. 10         =>  FAILED("WRONG ALTERNATIVE J1");
               WHEN  1000            =>  FAILED("WRONG ALTERNATIVE J2");
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE J4");
               WHEN  INTEGER'LAST -100 ..
                     INTEGER'LAST    =>  NULL ;
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE J6");
          END CASE;

          CASE  DYNCON  IS
               WHEN  1 .. 10         =>  FAILED("WRONG ALTERNATIVE K1");
               WHEN  1000            =>  NULL ;
               WHEN  2000            =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  4000 .. 4100    =>  FAILED("WRONG ALTERNATIVE K4");
               WHEN  INTEGER'LAST -100 ..
                     INTEGER'LAST    =>  FAILED("WRONG ALTERNATIVE K5");
               WHEN  OTHERS          =>  FAILED("WRONG ALTERNATIVE K6");
          END CASE;

     END ;


     RESULT ;


END  C54A42C ;
