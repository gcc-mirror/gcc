-- C24003C.ADA

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
-- CHECK THAT LEADING ZEROES IN INTEGRAL PARTS AND TRAILING ZEROES IN
-- FRACTIONAL PARTS OF FIXED POINT LITERALS ARE IGNORED.

-- JRK 12/12/79
-- JRK 12/16/80
-- TBN 10/21/85     RENAMED FROM C24003C.TST AND FIXED LINE LENGTH.
-- DTN 11/12/91     DELETED SUBPART (B).  CHANGED EXTENSION FROM '.TST' 
--                  TO '.ADA'.

WITH REPORT;
PROCEDURE C24003C IS

        USE REPORT;

        TYPE FIXED IS DELTA 1.0 RANGE 0.0 .. 1000.0;
        FX : FIXED := 69.0E1;

BEGIN

        TEST ("C24003C", "LEADING/TRAILING ZEROES IN " &
              "FIXED POINT LITERALS");

        IF 000000000000000000000000000000000000000069.0E1 /= FX THEN
                FAILED ("LEADING ZEROES IN INTEGRAL PART OF FIXED " &
                        "POINT LITERAL NOT IGNORED");
        END IF;

        IF 69.0000000000000000000000000000000000000000E1 /= FX THEN
                           -- MIGHT RAISE NUMERIC_ERROR AT COMPILE-TIME.
                FAILED ("TRAILING ZEROES IN FRACTIONAL PART OF " &
                        "FIXED POINT LITERAL NOT IGNORED");
        END IF;

        IF 0000000000000000000000000000000000000000690.00000 /= FX THEN
                FAILED ("LEADING/TRAILING ZEROES IN MANTISSA OF " &
                        "FIXED POINT LITERAL NOT IGNORED");
        END IF;

        IF 69.0E00000000000000000000000000000000000000001 /= FX THEN
                FAILED ("LEADING ZEROES IN EXPONENT OF FIXED " &
                        "POINT LITERAL NOT IGNORED");
        END IF;

        IF 16#00000000000000000000000000000000000000002B.2#E1 /= FX THEN
                FAILED ("LEADING ZEROES IN BASED FIXED POINT " &
                        "LITERAL NOT IGNORED");
        END IF;

        IF 16#2B.20000000000000000000000000000000000000000#E1 /= FX THEN
                FAILED ("TRAILING ZEROES IN BASED FIXED POINT " &
                        "LITERAL NOT IGNORED");
        END IF;

        RESULT;
END C24003C;
