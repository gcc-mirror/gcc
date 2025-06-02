-- C35502F.TST

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
-- CHECK THAT IMAGE AND VALUE ATTRIBUTES ARE CORRECT FOR A FORMAL
-- DISCRETE TYPE WHOSE ACTUAL PARAMETER IS AN ENUMERATED TYPE
-- WITH THE LONGEST POSSIBLE IDENTIFIER AS ONE CONSTANT.

-- PWB  03/05/86
-- DWC  07/22/87     -- ADDED THE CONSTANT STRING 'STR'.

WITH REPORT; USE REPORT;

PROCEDURE C35502F IS

     -- BIG_ID1 IS AN IDENTIFIER OF MAXIMUM LENGTH.
     TYPE ENUM IS ( EVAL1,
$BIG_ID1
                  );

     -- BIG_STRING1 & BIG_STRING2 YIELDS BIG_ID.
     STR1 : CONSTANT STRING :=
$BIG_STRING1;
     STR2 : CONSTANT STRING :=
$BIG_STRING2;
     STR : CONSTANT STRING := STR1 & STR2;

     GENERIC
          TYPE FORMAL IS (<>);
     PROCEDURE GEN_PROC;

     PROCEDURE GEN_PROC IS
     BEGIN
          VALUE_CHECK:
          BEGIN
               IF FORMAL'VALUE (STR) /= FORMAL'LAST THEN
                    FAILED ("VALUE OF LONG STRING NOT LONG IDENTIFIER");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN CHECKING " &
                            "VALUE ATTRIBUTE");
          END VALUE_CHECK;

          IMAGE_CHECK:
          BEGIN
               IF FORMAL'IMAGE (FORMAL'LAST) /= STR
               THEN
                    FAILED ("IMAGE OF LONG IDENTIFIER NOT LONG STRING");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN CHECKING " &
                            "IMAGE ATTRIBUTE");
          END IMAGE_CHECK;

     END GEN_PROC;

     PROCEDURE TEST_PROC IS NEW GEN_PROC (ENUM);

BEGIN   -- C35502F

     TEST ("C35502F", "IMAGE AND VALUE ATTRIBUTES FOR A FORMAL " &
                      "DISCRETE TYPE WITH ONE ACTUAL VALUE HAVING " &
                      "LONGEST POSSIBLE IDENTIFIER");
     TEST_PROC;
     RESULT;

END C35502F;
