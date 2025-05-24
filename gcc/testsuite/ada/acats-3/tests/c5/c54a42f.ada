-- C54A42F.ADA

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
-- CHECK THAT A  CASE_STATEMENT CORRECTLY HANDLES SEVERAL SMALL,
--    NON-CONTIGUOUS RANGES OF INTEGERS COVERED BY A SINGLE  'OTHERS'
--    ALTERNATIVE.


-- (OPTIMIZATION TEST.)


-- RM 03/31/81


WITH REPORT;
PROCEDURE  C54A42F  IS

     USE  REPORT ;

BEGIN

     TEST( "C54A42F" , "TEST THAT A  CASE_STATEMENT CORRECTLY HANDLES" &
                       " SEVERAL SMALL, NON-CONTIGUOUS ENUMERATION"    &
                       " RANGES COVERED BY A SINGLE  'OTHERS'  "       &
                       " ALTERNATIVE"  );

     DECLARE

          TYPE  DAY  IS (SUN , MON , TUE , WED , THU , FRI ,  SAT );

          DYNVAR2 :          DAY  := MON ;
          STATVAR :          DAY  := TUE ;
          STATCON : CONSTANT DAY  := WED ;
          DYNVAR  :          DAY  := THU ;
          DYNCON  : CONSTANT DAY  :=     DAY'VAL( IDENT_INT(5) ); -- FRI

     BEGIN

          IF EQUAL(1,289) THEN
               DYNVAR  := SUN ;
               DYNVAR2 := SUN ;
          END IF;

          CASE  SUN  IS        --  SUN
               WHEN  THU       =>  FAILED("WRONG ALTERNATIVE F1");
               WHEN  SUN       =>  NULL ;
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE F3");
               WHEN  TUE..WED  =>  FAILED("WRONG ALTERNATIVE F4");
               WHEN  OTHERS    =>  FAILED("WRONG ALTERNATIVE F5");
          END CASE;

          CASE  DYNVAR2   IS   --  MON
               WHEN  THU       =>  FAILED("WRONG ALTERNATIVE G1");
               WHEN  SUN       =>  FAILED("WRONG ALTERNATIVE G2");
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE G3");
               WHEN  TUE..WED  =>  FAILED("WRONG ALTERNATIVE G4");
               WHEN  OTHERS    =>  NULL ;
          END CASE;

          CASE  STATVAR  IS    --  TUE
               WHEN  THU       =>  FAILED("WRONG ALTERNATIVE H1");
               WHEN  SUN       =>  FAILED("WRONG ALTERNATIVE H2");
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE H3");
               WHEN  TUE..WED  =>  NULL ;
               WHEN  OTHERS    =>  FAILED("WRONG ALTERNATIVE H5");
          END CASE;

          CASE  STATCON  IS    --  WED
               WHEN  THU       =>  FAILED("WRONG ALTERNATIVE I1");
               WHEN  SUN       =>  FAILED("WRONG ALTERNATIVE I2");
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE I3");
               WHEN  TUE..WED  =>  NULL ;
               WHEN  OTHERS    =>  FAILED("WRONG ALTERNATIVE I5");
          END CASE;

          CASE  DYNVAR   IS    --  THU
               WHEN  THU       =>  NULL ;
               WHEN  SUN       =>  FAILED("WRONG ALTERNATIVE J2");
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE J3");
               WHEN  TUE..WED  =>  FAILED("WRONG ALTERNATIVE J4");
               WHEN  OTHERS    =>  FAILED("WRONG ALTERNATIVE J5");
          END CASE;

          CASE  DYNCON   IS    --  FRI
               WHEN  THU       =>  FAILED("WRONG ALTERNATIVE K1");
               WHEN  SUN       =>  FAILED("WRONG ALTERNATIVE K2");
               WHEN  SAT       =>  FAILED("WRONG ALTERNATIVE K3");
               WHEN  TUE..WED  =>  FAILED("WRONG ALTERNATIVE K4");
               WHEN  OTHERS    =>  NULL ;
          END CASE;

          CASE  DAY'SUCC( DYNCON )  IS   --  SAT
               WHEN THU       =>  FAILED("WRONG ALTERNATIVE L1");
               WHEN SUN       =>  FAILED("WRONG ALTERNATIVE L2");
               WHEN SAT       =>  NULL ;
               WHEN TUE..WED  =>  FAILED("WRONG ALTERNATIVE L4");
               WHEN OTHERS    =>  FAILED("WRONG ALTERNATIVE L5");
          END CASE;
     END ;


     RESULT ;


END  C54A42F ;
