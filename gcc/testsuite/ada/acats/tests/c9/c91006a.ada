-- C91006A.ADA

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
-- CHECK THAT IN A TASK SPECIFICATION ENTRY DECLARATIONS ARE ELABORATED
-- WHEN THE SPECIFICATION IS ELABORATED, AND IN TEXTUAL ORDER.

-- WEI  3/04/82
-- BHS  7/13/84
-- TBN 12/17/85     RENAMED FROM C910AHA-B.ADA;
--                  ADDED DECLARATIONS OF FIRST AND LAST.
-- PWB  5/15/86     MOVED DECLARATIONS OF FIRST, TASK T1, AND LAST
--                  INTO A DECLARE/BEGIN/END BLOCK.

WITH REPORT; USE REPORT;
PROCEDURE C91006A IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     INDEX : INTEGER RANGE 0..5 := 0;
     SPYNUMB : STRING(1..5) := (1..5 => ' ');

     FUNCTION FINIT_POS (DIGT: IN ARG) RETURN NATURAL IS
          TEMP : STRING(1..2);
     BEGIN
          TEMP := ARG'IMAGE(DIGT);
          INDEX := INDEX + 1;
          SPYNUMB(INDEX) := TEMP(2);
          RETURN DIGT;
     END FINIT_POS;

BEGIN
     TEST ("C91006A", "CHECK THAT IN A TASK SPEC, ELABORATION IS IN " &
                      "TEXTUAL ORDER");
     DECLARE

          FIRST : INTEGER := FINIT_POS (1);

          TASK T1 IS
               ENTRY E2 (NATURAL RANGE 1 .. FINIT_POS (2));
               ENTRY E3 (NATURAL RANGE 1 .. FINIT_POS (3));
               ENTRY E4 (NATURAL RANGE 1 .. FINIT_POS (4));
          END T1;

          LAST : INTEGER := FINIT_POS (5);

          TASK BODY T1 IS
          BEGIN
               NULL;
          END T1;

     BEGIN
           NULL;
     END;

     IF SPYNUMB /= "12345" THEN 
          FAILED ("TASK SPEC T1 NOT ELABORATED IN TEXTUAL ORDER");
          COMMENT ("ACTUAL ORDER WAS: " & SPYNUMB);
     END IF;

     RESULT;

END C91006A;
