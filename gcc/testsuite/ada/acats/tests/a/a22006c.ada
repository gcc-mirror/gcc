


-- A22006C.ADA

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
-- CHECK THAT A COMPILATION MAY BE PRECEDED BY EXTRA LINES
-- (INCLUDING LINES TERMINATED BY FORMAT EFFECTORS OTHER
--  THAN HORIZONTAL TABULATION).

-- NOTE: THIS FILE BEGINS WITH:
--       1) AN EMPTY LINE
--       2) A CARRIAGE RETURN CHARACTER       (ASCII 13. = 0D HEX)
--       3) A CARRIAGE RETURN CHARACTER       (ASCII 13. = 0D HEX)
--       4) A VERTICAL TABULATION CHARACTER   (ASCII 11. = 0B HEX)
--       5) A LINE FEED CHARACTER             (ASCII 10. = 0A HEX)
--       6) A LINE FEED CHARACTER             (ASCII 10. = 0A HEX)
--       7) A FORM FEED CHARACTER             (ASCII 12. = 0C HEX)

-- PWB  2/13/86

WITH REPORT;
USE  REPORT;

PROCEDURE A22006C IS
BEGIN
     TEST ("A22006C", "CHECK THAT A COMPILATION CAN BE PRECEDED " &
                      "BY EXTRA LINES");
     RESULT;
END A22006C;
