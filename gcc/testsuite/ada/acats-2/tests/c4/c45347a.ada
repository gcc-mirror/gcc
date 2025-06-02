-- C45347A.ADA

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
-- CHECK THAT CATENATION IS DEFINED FOR RECORD TYPES AS COMPONENT TYPES.

-- JWC 11/15/85

WITH REPORT; USE REPORT;

PROCEDURE C45347A IS

BEGIN

     TEST ("C45347A", "CHECK THAT CATENATION IS DEFINED " &
                      "FOR RECORD TYPES AS COMPONENT TYPES");

     DECLARE

          TYPE REC IS
               RECORD
                    X : INTEGER;
               END RECORD;

          SUBTYPE INT IS INTEGER RANGE 1 .. 4;
          TYPE A IS ARRAY ( INT RANGE <>) OF REC;

          R1 : REC := (X => 4);
          R2 : REC := (X => 1);

          A1 : A(1 .. 2) := ((X => 1), (X => 2));
          A2 : A(1 .. 2) := ((X => 3), (X => 4));
          A3 : A(1 .. 4) := ((X => 1), (X => 2), (X => 3), (X => 4));
          A4 : A(1 .. 4);
          A5 : A(1 .. 4) := ((X => 4), (X => 3), (X => 2), (X => 1));

     BEGIN

          A4 := A1 & A2;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR TWO ARRAYS OF " &
                       "RECORDS");
          END IF;

          A4 := A5;

          A4 := A1 & A2(1) & R1;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAY OF RECORD, " &
                       "AND RECORDS");
          END IF;

          A4 := A5;

          A4 := R2 & (A1(2) & A2);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR RECORDS, " &
                       "AND ARRAY OF RECORDS");
          END IF;

          A4 := A5;

          A4 := R2 & A1(2) & (A2(1) & R1);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR RECORDS");
          END IF;

     END;

     RESULT;

END C45347A;
