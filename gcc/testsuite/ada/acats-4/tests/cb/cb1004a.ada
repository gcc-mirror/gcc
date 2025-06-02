-- CB1004A.ADA

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
-- CHECK THAT EXCEPTIONS DECLARED IN RECURSIVE PROCEDURES ARE NOT
--    REPLICATED ANEW FOR EACH RECURSIVE ACTIVATION OF THE PROCEDURE.

-- DCB 03/30/80
-- JRK 11/17/80
-- SPS 3/23/83

WITH REPORT;
PROCEDURE CB1004A IS

     USE REPORT;

     FLOW_COUNT : INTEGER := 0;

     PROCEDURE P1(SWITCH1 : IN INTEGER) IS

          E1 : EXCEPTION;

          PROCEDURE P2 IS

          BEGIN
               FLOW_COUNT := FLOW_COUNT + 1;   -- 3
               P1(2);
               FAILED("EXCEPTION NOT PROPAGATED");

          EXCEPTION
               WHEN E1 =>
                    FLOW_COUNT := FLOW_COUNT + 1;   -- 6
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED");
          END P2;

     BEGIN
          FLOW_COUNT := FLOW_COUNT + 1;   -- 2   -- 4
          IF SWITCH1 = 1 THEN
               P2;
          ELSIF SWITCH1 = 2 THEN
               FLOW_COUNT := FLOW_COUNT + 1;   -- 5
               RAISE E1;
               FAILED("EXCEPTION NOT RAISED");
          END IF;
     END P1;

BEGIN
     TEST("CB1004A","CHECK THAT EXCEPTIONS ARE NOT RECURSIVELY " &
                    "REPLICATED");

     FLOW_COUNT := FLOW_COUNT + 1;   -- 1
     P1(1);

     IF FLOW_COUNT /= 6 THEN
          FAILED("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED("EXCEPTION HANDLED IN WRONG SCOPE");
          RESULT;
END CB1004A;
