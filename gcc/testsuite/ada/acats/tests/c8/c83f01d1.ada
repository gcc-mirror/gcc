-- C83F01D1.ADA

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
-- SEPARATELY COMPILED PACKAGE BODY FOR USE WITH  C83F01D0M


--    RM    13 AUGUST 1980
--    RM    29 AUGUST 1980



SEPARATE (C83F01D0M)
PACKAGE BODY  C83F01D1  IS

     Y4 : INTEGER := 200 ;

     PACKAGE BODY  P  IS
     BEGIN

          X1 :=  NOT X1  AND  Y1  AND  Y3 ;
          Z  :=  Z + T1 ;
          Y2 := X2 * Y2 ;
          Y4 := X2 * Y4 ;

          -- ALL 4 ASSIGNMENTS ARE TESTED IN THE MAIN PROGRAM (RATHER
          --    THAN HERE) TO PRECLUDE FALSE NEGATIVES (WHERE THE LACK
          --    OF ELABORATION-TIME ERROR MESSAGES SIMPLY MEANS THAT THE
          --    PACKAGE WAS NOT ELABORATED).

          -- INCORRECT INTERPRETATIONS IN THE FIRST TWO
          --    ASSIGNMENTS  MANIFEST THEMSELVES AT
          --    COMPILE TIME AS TYPE ERRORS.

     END P ;

END C83F01D1 ;
