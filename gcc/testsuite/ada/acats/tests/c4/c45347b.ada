-- C45347B.ADA

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
-- CHECK THAT CATENATION IS DEFINED FOR ARRAY TYPES AS COMPONENT TYPES.

-- JWC 11/15/85

WITH REPORT; USE REPORT;

PROCEDURE C45347B IS

BEGIN

     TEST ("C45347B", "CHECK THAT CATENATION IS DEFINED " &
                      "FOR ARRAY TYPES AS COMPONENT TYPES");

     DECLARE

          TYPE ARR IS ARRAY (1 .. 2) OF INTEGER;
          TYPE A IS ARRAY ( INTEGER RANGE <>) OF ARR;

          AR1 : ARR := (4,1);
          AR2 : ARR := (1,1);

          A1 : A(1 .. 2) := ((1,1), (2,1));
          A2 : A(1 .. 2) := ((3,1), (4,1));
          A3 : A(1 .. 4) := ((1,1), (2,1), (3,1), (4,1));
          A4 : A(1 .. 4);
          A5 : A(1 .. 4) := ((4,1), (3,1), (2,1), (1,1));

     BEGIN

          A4 := A1 & A2;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAYS OF ARRAYS");
          END IF;

          A4 := A5;

          A4 := A1 & A2(1) & AR1;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAY OF ARRAYS " &
                       "WITH ARRAYS");
          END IF;

          A4 := A5;

          A4 := AR2 & (A1(2) & A2);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAYS WITH ARRAYS " &
                       "OF ARRAYS");
          END IF;

          A4 := A5;

          A4 := A'(AR2 & A1(2)) & A'(A2(1) & AR1);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAYS");
          END IF;

     END;

     RESULT;

END C45347B;
