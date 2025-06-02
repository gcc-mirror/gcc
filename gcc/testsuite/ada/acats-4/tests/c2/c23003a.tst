-- C23003A.TST

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
-- CHECK THAT VARIABLE IDENTIFIERS CAN BE AS LONG AS THE MAXIMUM LENGTH
-- IDENTIFIER PERMITTED AND THAT ALL CHARACTERS ARE SIGNIFICANT.

-- JRK 12/12/79
-- JRK 1/11/80
-- JWC 6/28/85   RENAMED TO -AB
-- KAS 12/04/95 CHANGED "INPUT LINE LENGTH" TO "LENGTH IDENTIFIER"

WITH REPORT;
PROCEDURE C23003A IS

        USE REPORT;

BEGIN
        TEST ("C23003A", "MAXIMUM LENGTH VARIABLE IDENTIFIERS");

        -- BIG_ID1 AND BIG_ID2 ARE TWO MAXIMUM LENGTH IDENTIFIERS THAT
        -- DIFFER ONLY IN THEIR LAST CHARACTER.

        DECLARE
$BIG_ID1
                                        -- BIG_ID1
                        : INTEGER := 1;
        BEGIN
                DECLARE
$BIG_ID2
                                                -- BIG_ID2
                                : INTEGER := 2;
                BEGIN

                        IF
$BIG_ID1
                                                -- BIG_ID1
                                +
$BIG_ID2
                                                -- BIG_ID2
                                        /= 3 THEN
                                FAILED ("IDENTIFIERS AS LONG AS " &
                                        "MAXIMUM INPUT LINE LENGTH " &
                                        "NOT PERMITTED OR NOT " &
                                        "DISTINGUISHED BY DISTINCT " &
                                        "SUFFIXES");
                        END IF;

                END;
        END;

        -- BIG_ID3 AND BIG_ID4 ARE TWO MAXIMUM LENGTH IDENTIFIERS THAT
        -- DIFFER ONLY IN THEIR MIDDLE CHARACTER.

        DECLARE
$BIG_ID3
                                        -- BIG_ID3
                        : INTEGER := 3;
        BEGIN
                DECLARE
$BIG_ID4
                                                -- BIG_ID4
                                : INTEGER := 4;
                BEGIN

                        IF
$BIG_ID3
                                                -- BIG_ID3
                                +
$BIG_ID4
                                                -- BIG_ID4
                                        /= 7 THEN
                                FAILED ("IDENTIFIERS AS LONG AS " &
                                        "MAXIMUM INPUT LINE LENGTH " &
                                        "NOT PERMITTED OR NOT " &
                                        "DISTINGUISHED BY DISTINCT " &
                                        "MIDDLES");
                        END IF;

                END;
        END;

        RESULT;
END C23003A;
