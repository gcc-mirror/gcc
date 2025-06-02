-- A74106B.ADA

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
-- CHECK THAT A FULL DECLARATION FOR A PRIVATE TYPE OR FOR A LIMITED
--    PRIVATE TYPE CAN BE GIVEN IN TERMS OF ANY SCALAR TYPE, ARRAY TYPE,
--    RECORD TYPE (WITH OR WITHOUT DISCRIMINANTS), ACCESS TYPE (WITH
--    OR WITHOUT DISCRIMINANTS), OR ANY TYPE DERIVED FROM ANY OF THE
--    ABOVE.

-- PART B: TYPES INVOLVING FLOATING-POINT DATA.


-- RM 05/08/81


WITH REPORT;
PROCEDURE  A74106B  IS

     USE  REPORT;

BEGIN

     TEST( "A74106B" , "CHECK THAT PRIVATE TYPES AND LIMITED PRIVATE " &
                       "TYPES CAN BE DEFINED IN TERMS OF " &
                       "FLOATING-POINT TYPES" );

     DECLARE

          PACKAGE  P0  IS
               TYPE  F0  IS  PRIVATE;
          PRIVATE
               TYPE  F0  IS  NEW FLOAT;
          END  P0;

          PACKAGE  P1  IS
               USE   P0;
               TYPE  F1  IS  PRIVATE;
               TYPE  F2  IS  PRIVATE;
               TYPE  F3  IS  PRIVATE;
               TYPE  F4  IS  PRIVATE;
               TYPE  F5  IS  PRIVATE;
               TYPE  F6  IS  PRIVATE;
               TYPE  F7  IS  PRIVATE;
               TYPE  F8  IS  PRIVATE;
               TYPE  F9  IS  PRIVATE;
               TYPE  FA  IS  PRIVATE;
               TYPE  FB  IS  PRIVATE;
               TYPE  FC  IS  PRIVATE;
               TYPE  FD(I : INTEGER)  IS  PRIVATE;
               TYPE  NF  IS  NEW  FLOAT;
               TYPE  ARR_F             IS  ARRAY(1..2) OF FLOAT;
               TYPE  ACC_F             IS  ACCESS FLOAT;
               TYPE  REC_F             IS RECORD F : FLOAT; END RECORD;
               TYPE  D_REC_F(I : INTEGER := 1) IS 
                    RECORD F : FLOAT; END RECORD;
          PRIVATE
               TYPE  FY(B : BOOLEAN)  IS  RECORD G : FLOAT; END RECORD;
               TYPE  FC  IS  NEW  F0;
               TYPE  F1  IS  DIGITS 3;
               TYPE  F2  IS  NEW FLOAT DIGITS 4;
               TYPE  F3  IS  NEW  NF;
               TYPE  F4  IS  ARRAY(1..2) OF FLOAT;
               TYPE  F5  IS  NEW ARR_F;
               TYPE  F6  IS  ACCESS FLOAT;
               TYPE  F7  IS  NEW ACC_F;
               TYPE  F8  IS  RECORD F : FLOAT; END RECORD;
               TYPE  F9  IS  NEW REC_F;
               TYPE  FA  IS  ACCESS FD;
               TYPE  FB  IS  ACCESS D_REC_F;
               TYPE  FD(I : INTEGER)  IS  RECORD G : FLOAT; END RECORD;

          END  P1;

     BEGIN

          NULL;

     END;


     DECLARE

          PACKAGE  P0  IS
               TYPE  F0  IS  LIMITED PRIVATE;
          PRIVATE
               TYPE  F0  IS  NEW FLOAT;
          END  P0;

          PACKAGE  P1  IS
               USE   P0;
               TYPE  F1  IS  LIMITED PRIVATE;
               TYPE  F2  IS  LIMITED PRIVATE;
               TYPE  F3  IS  LIMITED PRIVATE;
               TYPE  F4  IS  LIMITED PRIVATE;
               TYPE  F5  IS  LIMITED PRIVATE;
               TYPE  F6  IS  LIMITED PRIVATE;
               TYPE  F7  IS  LIMITED PRIVATE;
               TYPE  F8  IS  LIMITED PRIVATE;
               TYPE  F9  IS  LIMITED PRIVATE;
               TYPE  FA  IS  LIMITED PRIVATE;
               TYPE  FB  IS  LIMITED PRIVATE;
               TYPE  FC  IS  LIMITED PRIVATE;
               TYPE  FD(I : INTEGER)  IS  LIMITED PRIVATE;
               TYPE  NF  IS  NEW  FLOAT;
               TYPE  ARR_F             IS  ARRAY(1..2) OF FLOAT;
               TYPE  ACC_F             IS  ACCESS FLOAT;
               TYPE  REC_F             IS RECORD F : FLOAT; END RECORD;
               TYPE  D_REC_F(I : INTEGER := 1) IS 
                    RECORD F : FLOAT; END RECORD;
          PRIVATE
               TYPE  FY(B : BOOLEAN)  IS  RECORD G : FLOAT; END RECORD;
               TYPE  FC  IS  NEW  F0;
               TYPE  F1  IS  DIGITS 3;
               TYPE  F2  IS  NEW FLOAT DIGITS 4;
               TYPE  F3  IS  NEW  NF;
               TYPE  F4  IS  ARRAY(1..2) OF FLOAT;
               TYPE  F5  IS  NEW ARR_F;
               TYPE  F6  IS  ACCESS FLOAT;
               TYPE  F7  IS  NEW ACC_F;
               TYPE  F8  IS  RECORD F : FLOAT; END RECORD;
               TYPE  F9  IS  NEW REC_F;
               TYPE  FA  IS  ACCESS FD;
               TYPE  FB  IS  ACCESS D_REC_F;
               TYPE  FD(I : INTEGER)  IS  RECORD G : FLOAT; END RECORD;

          END  P1;

     BEGIN

          NULL;

     END;


     RESULT;


END A74106B;
