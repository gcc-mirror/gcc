-- CB2006A.ADA

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
-- CHECK THAT LOCAL VARIABLES AND PARAMETERS OF A SUBPROGRAM,
-- OR PACKAGE ARE ACCESSIBLE WITHIN A HANDLER.

-- DAT 4/13/81
-- SPS 3/23/83

WITH REPORT; USE REPORT;

PROCEDURE CB2006A IS

     I : INTEGER RANGE 0 .. 1;

     PACKAGE P IS
          V2 : INTEGER := 2;
     END P;

     PROCEDURE PR (J : IN OUT INTEGER) IS
          K : INTEGER := J;
     BEGIN
          I := K;
          FAILED ("CONSTRAINT_ERROR NOT RAISED 1");
     EXCEPTION
          WHEN OTHERS =>
               J := K + 1;
     END PR;

     PACKAGE BODY P IS
          L : INTEGER := 2;
     BEGIN
          TEST ("CB2006A", "LOCAL VARIABLES ARE ACCESSIBLE IN"
               & " HANDLERS");

          I := 1;
          I := I + 1;
          FAILED ("CONSTRAINT_ERROR NOT RAISED 2");
     EXCEPTION
          WHEN OTHERS =>
               PR (L);
               IF L /= V2 + 1 THEN
                    FAILED ("WRONG VALUE IN LOCAL VARIABLE");
               END IF;
     END P;
BEGIN

     RESULT;
END CB2006A;
