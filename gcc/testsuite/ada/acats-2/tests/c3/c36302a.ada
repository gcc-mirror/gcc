-- C36302A.ADA

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
-- CHECK THAT A STRING VARIABLE MAY BE DECLARED WITH AN INDEX
-- STARTING WITH AN INTEGER GREATER THAN 1.

-- DAT 2/17/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE C36302A IS

     USE REPORT;

     S5 : STRING (5 .. 10);
     SX : STRING (INTEGER'LAST - 5 .. INTEGER'LAST);

BEGIN
     TEST ("C36302A", "STRING VARIABLE INDICES NEEDN'T START AT 1");

     IF S5'FIRST /= 5
     OR S5'LAST /= 10
     OR S5'LENGTH /= 6
     OR SX'FIRST /= INTEGER'LAST - 5
     OR SX'LAST /= INTEGER'LAST
     OR SX'LENGTH /= 6
     THEN
          FAILED ("WRONG STRING ATTRIBUTES");
     END IF;

     RESULT;
END C36302A;
