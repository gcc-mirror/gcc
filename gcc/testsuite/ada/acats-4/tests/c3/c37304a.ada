-- C37304A.ADA

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
-- CHECK THAT ALL FORMS OF CHOICE ARE PERMITTED IN A VARIANT_PART,
-- AND, IN PARTICULAR, THAT FORMS LIKE ST RANGE L..R, AND ST ARE 
-- PERMITTED.
 
-- ASL 7/31/81
--  RM 8/26/82
-- SPS 1/21/83

WITH REPORT;
PROCEDURE C37304A IS
 
     USE REPORT;
     
BEGIN

     TEST("C37304A","ALL FORMS OF CHOICE ALLOWED IN A VARIANT_PART");

     DECLARE

          TYPE T IS RANGE 1 .. 10;
          C5 : CONSTANT T := 5;
          SUBTYPE S1 IS T RANGE 1 .. 5;
          SUBTYPE S2 IS T RANGE C5 + 1 .. 7;
          SUBTYPE SN IS T RANGE C5 + 4 .. C5 - 4 + 7;  -- NULL RANGE.
          SUBTYPE S10 IS T RANGE C5 + 5 .. T'LAST;

          TYPE VREC( DISC : T := 8 ) IS
               RECORD
                    CASE DISC IS
                         WHEN SN                       -- 9..8
                         | S1 RANGE 1 .. 0             -- 1..0
                         | S2 RANGE C5 + 2 .. C5 + 1   -- 7..6
                         | 3 .. 2                      -- 3..2
                              => NULL;

                         WHEN  S1 RANGE 4 .. C5        -- 4..5
                         | S1 RANGE C5 - 4 .. C5 / 2   -- 1..2
                         | 3 .. 1 + C5 MOD 3           -- 3..3
                         | SN                          -- 9..8
                         | S1 RANGE 5 .. C5 - 1        -- 5..4
                         | 6 .. 7                      -- 6..7
                         | S10                         -- 10..10
                         | 9                           -- 9
                         | S10 RANGE 10 .. 9           -- 10..9
                              => NULL;

                         WHEN C5 + C5 - 2 .. 8         -- 8
                              => NULL;

                    END CASE;
               END RECORD;
 
          V : VREC;

     BEGIN

          IF EQUAL(3,3) THEN
               V := (DISC => 5);
          END IF;
          IF V.DISC /= 5 THEN
               FAILED ("ASSIGNMENT FAILED");
          END IF;

     END;
 
     RESULT;

END C37304A;
