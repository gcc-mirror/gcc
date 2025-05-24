-- C83F03C1.ADA

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
-- SEPARATELY COMPILED PACKAGE FOR USE WITH  C83F03C2M

-- THIS PACKAGE IS A FULL-FLEDGED COMPILATION UNIT (AS OPPOSED TO
--    BEING A SUBUNIT; SUBUNITS ARE TESTED IN  C83F03D0M ,
--    C83F03D1 ). THE PRESENT FILE CONTAINS THE BODY OF THE PACKAGE.

-- FOR THIS FILE, THE FILE NAME AND THE UNIT NAME ARE NOT THE SAME.


--    RM    05 SEPTEMBER 1980
-- PWN 11/30/94 ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.


PACKAGE BODY  C83F03C0  IS

     Y4 : INTEGER := 200 ;

     TYPE  T4  IS  ( G , H , I ) ;

     PROCEDURE  BUMP  IS
     BEGIN
          FLOW_INDEX := FLOW_INDEX + 1 ;
     END BUMP ;

     PROCEDURE REQUIRE_BODY IS
     BEGIN
       NULL;
     END;

     PACKAGE BODY  P  IS
     BEGIN

          GOTO  T3 ;

          BUMP ;
          BUMP ;

          <<T3>>   BUMP ;  GOTO  T4 ;
          BUMP ;
          <<LABEL_IN_OUTER>>   BUMP ;  GOTO  ENDING ;
          BUMP ;
          <<Y3>>   BUMP ;  GOTO  Y4 ;
          BUMP ;
          <<Y4>>   BUMP ;  GOTO  LABEL_IN_OUTER ;
          BUMP ;
          <<T4>>   BUMP ;  GOTO  Y3 ;
          BUMP ;

          << ENDING >>  NULL;

     END P ;

BEGIN

     << LABEL_IN_OUTER >>  NULL ;

END  C83F03C0 ;
