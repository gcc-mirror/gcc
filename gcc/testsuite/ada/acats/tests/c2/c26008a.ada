-- C26008A.ADA

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
-- CHECK THAT UPPER AND LOWER CASE LETTERS ARE DISTINCT WITHIN STRING
-- LITERALS.

-- JRK 12/12/79
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

WITH REPORT;
PROCEDURE C26008A IS

        USE REPORT;

BEGIN
        TEST ("C26008A", "UPPER/LOWER CASE ARE DISTINCT IN STRING " &
              "LITERALS");

        IF CHARACTER'('a') = 'A'  THEN
                FAILED ("LOWER CASE NOT DISTINCT FROM UPPER IN " &
                        "CHARACTER LITERALS");
        END IF;

        IF STRING'("abcde") = "ABCDE" THEN
                FAILED ("LOWER CASE NOT DISTINCT FROM UPPER IN " &
                        "STRING LITERALS");
        END IF;

        RESULT;
END C26008A;
