-- C54A24A.ADA

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
-- CHECK THAT NULL SUBRANGE CHOICES MAY OCCUR IN CASE STATEMENT, WITH
-- OUT-OF-BOUNDS RANGE BOUNDS, AND WHERE VACUOUS CHOICES ARE NULL.
-- CHECK THAT AN UNNEEDED OTHERS CHOICE IS PERMITTED.

-- DAT 1/29/81
-- JBG 8/21/83

WITH REPORT;
PROCEDURE C54A24A IS

     USE REPORT;

     TYPE T IS RANGE 1 .. 1010;
     SUBTYPE ST IS T RANGE 5 .. 7;

     V : ST := 6;

BEGIN
     TEST ("C54A24A", "CHECK NULL CASE SUBRANGE CHOICES, WITH " &
           "OUTRAGEOUS BOUNDS");

     CASE V IS
          WHEN -1000 .. -1010 => NULL;
          WHEN T RANGE -5 .. -6 => NULL;
          WHEN 12 .. 11 | ST RANGE 1000 .. 99 => NULL;
          WHEN ST RANGE -99 .. -999 => NULL;
          WHEN ST RANGE 6 .. 6 => V := V - 1;
          WHEN T RANGE ST'BASE'LAST .. ST'BASE'FIRST => NULL;
          WHEN 5 | 7 => NULL;
          WHEN ST RANGE T'BASE'LAST .. T'BASE'FIRST => NULL;
          WHEN T'BASE'LAST .. T'BASE'FIRST => NULL;
          WHEN OTHERS => V := V + 1;
     END CASE;
     IF V /= 5 THEN
          FAILED ("IMPROPER CASE EXECUTION");
     END IF;

     RESULT;
END C54A24A;
