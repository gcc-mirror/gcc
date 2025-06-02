-- C54A24B.ADA

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
-- OBJECTIVE:
--     CHECK THAT NULL SUBTYPE RANGES ARE ACCEPTABLE CASE CHOICES,
--     WHERE THE BOUNDS ARE BOTH OUT OF THE SUBRANGE'S RANGE, AND
--     WHERE VACUOUS CHOICES HAVE NON-NULL STATEMENT SEQUENCES.
--     CHECK THAT AN UNNEEDED OTHERS CLAUSE IS PERMITTED.

-- HISTORY:
--     DAT 01/29/81 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT;
PROCEDURE C54A24B IS

     USE REPORT;

     TYPE C IS NEW CHARACTER RANGE 'A' .. 'D';
     X : C := 'B';

BEGIN
     TEST ("C54A24B", "NULL CASE CHOICE SUBRANGES WITH VALUES " &
           "OUTSIDE SUBRANGE");

     CASE X IS
          WHEN C RANGE C'BASE'LAST .. C'BASE'FIRST
               | C RANGE 'Z' .. ' ' => X := 'A';
          WHEN C => NULL;
          WHEN OTHERS => X := 'C';
     END CASE;
     IF X /= 'B' THEN
          FAILED ("WRONG CASE EXECUTION");
     END IF;

     RESULT;
END C54A24B;
