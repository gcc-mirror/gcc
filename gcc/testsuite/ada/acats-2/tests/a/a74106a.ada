-- A74106A.ADA

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

-- PART A: TYPES NOT INVOLVING FLOATING-POINT DATA OR FIXED-POINT DATA.


-- RM 05/13/81


WITH REPORT;
PROCEDURE  A74106A  IS

     USE  REPORT;

BEGIN

     TEST( "A74106A" , "CHECK THAT PRIVATE TYPES AND LIMITED PRIVATE " &
                       "TYPES CAN BE DEFINED IN TERMS OF " &
                       "VARIOUS OTHER TYPES" );

     DECLARE

          TYPE  ENUM  IS  ( A , B , C , D );

          PACKAGE  P0  IS
               TYPE  T0  IS  PRIVATE;
          PRIVATE
               TYPE  T0  IS  NEW INTEGER;
          END  P0;

          PACKAGE  P1  IS
               USE   P0;
               TYPE  T1  IS  PRIVATE;
               TYPE  T2  IS  PRIVATE;
               TYPE  T3  IS  PRIVATE;
               TYPE  T4  IS  PRIVATE;
               TYPE  T5  IS  PRIVATE;
               TYPE  T6  IS  PRIVATE;
               TYPE  T7  IS  PRIVATE;
               TYPE  T8  IS  PRIVATE;
               TYPE  T9  IS  PRIVATE;
               TYPE  TA  IS  PRIVATE;
               TYPE  TB  IS  PRIVATE;
               TYPE  TC  IS  PRIVATE;
               TYPE  TD(I : INTEGER)  IS  PRIVATE;
               TYPE  NT  IS  NEW  ENUM;
               TYPE  ARR_T            IS  ARRAY(1..2) OF BOOLEAN;
               TYPE  ACC_T            IS  ACCESS CHARACTER;
               TYPE  REC_T            IS RECORD T : BOOLEAN; END RECORD;
               TYPE  D_REC_T(I : INTEGER := 1)  IS 
                    RECORD T : ENUM; END RECORD;
          PRIVATE
               TYPE  TY(B : BOOLEAN)  IS  
                    RECORD G : BOOLEAN; END RECORD;
               TYPE  TC  IS  NEW  T0;
               TYPE  T1  IS  RANGE 1..100;
               TYPE  T2  IS  NEW CHARACTER RANGE 'A'..'Z';
               TYPE  T3  IS  NEW  NT;
               TYPE  T4  IS  ARRAY(1..2) OF INTEGER;
               TYPE  T5  IS  NEW ARR_T;
               TYPE  T6  IS  ACCESS ENUM;
               TYPE  T7  IS  NEW ACC_T;
               TYPE  T8  IS  
                    RECORD T : CHARACTER; END RECORD;
               TYPE  T9  IS  NEW REC_T;
               TYPE  TA  IS  ACCESS TD;
               TYPE  TB  IS  ACCESS D_REC_T;
               TYPE  TD(I : INTEGER)  IS  
                    RECORD G : BOOLEAN; END RECORD;

          END  P1;

     BEGIN

          NULL;

     END;


     DECLARE

          TYPE  ENUM  IS  ( A , B , C , D );

          PACKAGE  P0  IS
               TYPE  T0  IS  LIMITED PRIVATE;
          PRIVATE
               TYPE  T0  IS  NEW ENUM;
          END  P0;

          PACKAGE  P1  IS
               USE   P0;
               TYPE  T1  IS  LIMITED PRIVATE;
               TYPE  T2  IS  LIMITED PRIVATE;
               TYPE  T3  IS  LIMITED PRIVATE;
               TYPE  T4  IS  LIMITED PRIVATE;
               TYPE  T5  IS  LIMITED PRIVATE;
               TYPE  T6  IS  LIMITED PRIVATE;
               TYPE  T7  IS  LIMITED PRIVATE;
               TYPE  T8  IS  LIMITED PRIVATE;
               TYPE  T9  IS  LIMITED PRIVATE;
               TYPE  TA  IS  LIMITED PRIVATE;
               TYPE  TB  IS  LIMITED PRIVATE;
               TYPE  TC  IS  LIMITED PRIVATE;
               TYPE  TD(I : INTEGER)  IS  LIMITED PRIVATE;
               TYPE  NT  IS  NEW  ENUM;
               TYPE  ARR_T           IS  ARRAY(1..2) OF BOOLEAN;
               TYPE  ACC_T           IS  ACCESS CHARACTER;
               TYPE  REC_T           IS  RECORD T : BOOLEAN; END RECORD;
               TYPE  D_REC_T(I : INTEGER := 1)  IS 
                    RECORD T : ENUM; END RECORD;
          PRIVATE
               TYPE  TY(B : BOOLEAN)  IS  
                    RECORD G : BOOLEAN; END RECORD;
               TYPE  TC  IS  NEW  T0;
               TYPE  T1  IS  RANGE 1..100;
               TYPE  T2  IS  NEW CHARACTER RANGE 'A'..'Z';
               TYPE  T3  IS  NEW  NT;
               TYPE  T4  IS  ARRAY(1..2) OF INTEGER;
               TYPE  T5  IS  NEW ARR_T;
               TYPE  T6  IS  ACCESS ENUM;
               TYPE  T7  IS  NEW ACC_T;
               TYPE  T8  IS  RECORD T : CHARACTER; END RECORD;
               TYPE  T9  IS  NEW REC_T;
               TYPE  TA  IS  ACCESS TD;
               TYPE  TB  IS  ACCESS D_REC_T;
               TYPE  TD(I : INTEGER)  IS  
                    RECORD G : BOOLEAN; END RECORD;

          END  P1;

     BEGIN

          NULL;

     END;


     RESULT;


END A74106A;
