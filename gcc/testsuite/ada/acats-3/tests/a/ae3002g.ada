-- AE3002G.ADA

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
-- CHECK THAT FILE_MODE IS VISIBLE AND HAS LITERALS IN_FILE AND
-- OUT_FILE.  ASLO CHECK THAT TYPE_SET IS VISIBLE AND HAS LITERALS
-- LOWER_CASE AND UPPER_CASE.

-- TBN  10/3/86

WITH TEXT_IO; USE TEXT_IO;
WITH REPORT; USE REPORT;
PROCEDURE AE3002G IS

     TEMP_FILE : FILE_TYPE;
     MODE : FILE_MODE := IN_FILE;
     LETTERS : TYPE_SET := LOWER_CASE;

BEGIN
     TEST ("AE3002G", "CHECK THAT FILE_MODE AND TYPE_SET ARE VISIBLE " &
                      "AND CHECK THEIR LITERALS");

     MODE := OUT_FILE;
     LETTERS := UPPER_CASE;

     RESULT;
END AE3002G;
