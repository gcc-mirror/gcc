-- C41201D.ADA

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
-- FOR SLICED COMPONENTS OF THE FORM F(...), CHECK THAT 
--   THE REQUIREMENT FOR A ONE-DIMENSIONAL ARRAY AND THE
--   TYPE OF THE INDEX ARE USED TO RESOLVE AN OVERLOADING OF F.

-- WKB 8/11/81
-- JBG 10/12/81
-- SPS 11/1/82

WITH REPORT;
PROCEDURE C41201D IS

     USE REPORT;

     TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
     SUBTYPE T1 IS T(1..10);
     TYPE T2 IS ARRAY (1..10, 1..10) OF INTEGER;
     TT : T(1..3);

     SUBTYPE U1 IS T(1..10);
     TYPE U2 IS (MON,TUE,WED,THU,FRI);
     SUBTYPE SU2 IS U2 RANGE MON .. THU;
     TYPE U3 IS ARRAY (SU2) OF INTEGER;
     UU : T(1..3);

     TYPE V IS ARRAY (INTEGER RANGE <> ) OF BOOLEAN;
     SUBTYPE V1 IS V(1..10);
     SUBTYPE V2 IS T(1..10);
     VV : V(2..5);

     FUNCTION F RETURN T1 IS
     BEGIN
          RETURN (1,1,1,1,5,6,7,8,9,10);
     END F;

     FUNCTION F RETURN T2 IS
     BEGIN
          RETURN (1..10 => (1,2,3,4,5,6,7,8,9,10));
     END F;

     FUNCTION G RETURN U1 IS
     BEGIN
          RETURN (3,3,3,3,5,6,7,8,9,10);
     END G;

     FUNCTION G RETURN U3 IS
     BEGIN
          RETURN (0,1,2,3);
     END G;

     FUNCTION H RETURN V1 IS
     BEGIN
          RETURN (1|3..10 => FALSE, 2 => IDENT_BOOL(TRUE));
     END H;

     FUNCTION H RETURN V2 IS
     BEGIN
          RETURN (1..10 => 5);
     END H;

BEGIN

     TEST ("C41201D", "WHEN SLICING FUNCTION RESULTS, TYPE OF " &
                      "RESULT IS USED FOR OVERLOADING RESOLUTION");

     IF F(1..3) /=
        F(IDENT_INT(2)..IDENT_INT(4)) THEN -- NUMBER OF DIMENSIONS.
          FAILED ("WRONG VALUE - 1");
     END IF;

     IF G(1..3) /=
        G(IDENT_INT(2)..IDENT_INT(4)) THEN -- INDEX TYPE.
          FAILED ("WRONG VALUE - 2");
     END IF;

     IF NOT IDENT_BOOL(H(2..3)(2)) THEN    -- COMPONENT TYPE.
          FAILED ("WRONG VALUE - 3");
     END IF;

     RESULT;

END C41201D;
