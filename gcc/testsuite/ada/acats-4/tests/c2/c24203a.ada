-- C24203A.ADA

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
-- CHECK THAT BASED INTEGER LITERALS WITH BASES 2 THROUGH 16 ALL
-- YIELD CORRECT VALUES.

-- JRK 12/12/79
-- JRK 10/27/80
-- JWC 6/28/85   RENAMED FROM C24103A.ADA

WITH REPORT;
PROCEDURE C24203A IS

        USE REPORT;

        I : INTEGER := 200;

BEGIN
        TEST ("C24203A", "VALUES OF BASED INTEGER LITERALS");

        IF 2#11# /= 3 THEN
                FAILED ("INCORRECT VALUE FOR BASE 2 INTEGER");
        END IF;

        IF 3#22# /= 8 THEN
                FAILED ("INCORRECT VALUE FOR BASE 3 INTEGER");
        END IF;

        IF 4#33# /= 15 THEN
                FAILED ("INCORRECT VALUE FOR BASE 4 INTEGER");
        END IF;

        IF 5#44# /= 24 THEN
                FAILED ("INCORRECT VALUE FOR BASE 5 INTEGER");
        END IF;

        IF 6#55# /= 35 THEN
                FAILED ("INCORRECT VALUE FOR BASE 6 INTEGER");
        END IF;

        IF 7#66# /= 48 THEN
                FAILED ("INCORRECT VALUE FOR BASE 7 INTEGER");
        END IF;

        IF 8#77# /= 63 THEN
                FAILED ("INCORRECT VALUE FOR BASE 8 INTEGER");
        END IF;

        IF 9#88# /= 80 THEN
                FAILED ("INCORRECT VALUE FOR BASE 9 INTEGER");
        END IF;

        IF 10#99# /= 99 THEN
                FAILED ("INCORRECT VALUE FOR BASE 10 INTEGER");
        END IF;

        IF 11#AA# /= 120 THEN
                FAILED ("INCORRECT VALUE FOR BASE 11 INTEGER");
        END IF;

        IF 12#BB# /= 143 THEN
                FAILED ("INCORRECT VALUE FOR BASE 12 INTEGER");
        END IF;

        IF 13#CC# /= 168 THEN
                FAILED ("INCORRECT VALUE FOR BASE 13 INTEGER");
        END IF;

        IF 14#DD# /= 195 THEN
                FAILED ("INCORRECT VALUE FOR BASE 14 INTEGER");
        END IF;

        IF 15#EE# /= 224 THEN
                FAILED ("INCORRECT VALUE FOR BASE 15 INTEGER");
        END IF;

        IF 16#FF# /= 255 THEN
                FAILED ("INCORRECT VALUE FOR BASE 16 INTEGER");
        END IF;

        ----------------------------------------

        IF 7#66#E1 /= 336 THEN
                FAILED ("INCORRECT VALUE FOR BASE 7 INTEGER " &
                        "WITH EXPONENT");
        END IF;

        RESULT;
END C24203A;
