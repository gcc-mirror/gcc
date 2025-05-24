-- C54A23A.ADA

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
-- CHECK THAT CASE CHOICES MAY BE CONSTANT NAMES

-- DAT 3/18/81
-- SPS 4/7/82

WITH REPORT; USE REPORT;

PROCEDURE C54A23A IS

     C1 : CONSTANT INTEGER := 1;
     C2 : CONSTANT INTEGER := 2;
     C3 : CONSTANT INTEGER := 3;

BEGIN
     TEST ("C54A23A", "CASE CHOICES MAY BE CONSTANTS");

     CASE IDENT_INT (C3) IS
          WHEN C1 | C2
               => FAILED ("WRONG CASE CHOICE 1");
          WHEN 3 => NULL;
          WHEN OTHERS => FAILED ("WRONG CASE CHOICE 2");
     END CASE;

     RESULT;
END C54A23A;
