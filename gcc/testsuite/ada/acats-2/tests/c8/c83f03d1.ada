-- C83F03D1.ADA

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
-- SEPARATELY COMPILED PACKAGE BODY FOR USE WITH  C83F03D0M


--    RM    08 SEPTEMBER 1980
--    JRK   14 NOVEMBER  1980



SEPARATE (C83F03D0M)
PACKAGE BODY  C83F03D1  IS

     Y4 : INTEGER := 200 ;

     TYPE  T4  IS  ( G , H , I ) ;

     PROCEDURE  BUMP  IS
     BEGIN
          FLOW_INDEX := FLOW_INDEX + 1 ;
     END BUMP ;

     PACKAGE BODY  P  IS
     BEGIN

          GOTO  X1 ;

          BUMP ;
          BUMP ;

          <<LABEL_IN_MAIN>>   BUMP ;  GOTO  T3 ;
          BUMP ;
          <<T1>>   BUMP ;  GOTO  Z ;
          BUMP ;
          <<Y1>>   BUMP ;  GOTO  LABEL_IN_MAIN ;
          BUMP ;
          <<X1>>   BUMP ;  GOTO  T1 ;
          BUMP ;
          <<Z>>    BUMP ;  GOTO  Y1 ;
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

END C83F03D1 ;
