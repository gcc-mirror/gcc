-- C24003A.ADA

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
-- CHECK THAT LEADING ZEROES IN INTEGRAL PARTS OF INTEGER LITERALS
-- ARE IGNORED.

-- JRK 12/12/79
-- JRK 12/16/80
-- TBN 10/16/85     RENAMED FROM C24003A.TST AND FIXED LINE LENGTH.
-- DTN 11/12/91     DELETED SUBPART (B).  CHANGED EXTENSION FROM '.TST' 
--                  TO '.ADA'.

WITH REPORT;
PROCEDURE C24003A IS

        USE REPORT;

BEGIN
        TEST ("C24003A", "LEADING ZEROES IN INTEGER LITERALS");
        
        IF 0000000000000000000000000000000000000000247 /= 247 THEN
                FAILED ("LEADING ZEROES IN INTEGER LITERALS NOT " &
                        "IGNORED");
        END IF;

        IF 35E00000000000000000000000000000000000000001 /= 350 THEN
                FAILED ("LEADING ZEROES IN EXPONENTS NOT IGNORED");
        END IF;

        IF 000000000000000000000000000000000000000016#FF# /= 255 THEN
                FAILED ("LEADING ZEROES IN BASES NOT IGNORED");
        END IF;

        IF 16#0000000000000000000000000000000000000000FF# /= 255 THEN
                FAILED ("LEADING ZEROES IN BASED INTEGER LITERALS " &
                        "NOT IGNORED");
        END IF;

        RESULT;
END C24003A;
