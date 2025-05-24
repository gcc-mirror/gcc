-- C95033B.ADA

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
-- CHECK THAT EXECUTION OF AN ENTRY CALL STARTS WITH THE EVALUATION OF
-- ANY ENTRY INDEX, FOLLOWED BY THE EVALUATION OF ANY EXPRESSION IN
-- THE PARAMETER LIST.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C950BHA-B.ADA

WITH REPORT;
 USE REPORT;
PROCEDURE C95033B IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     FUNCTION FINIT_POS (DIGT: IN ARG) RETURN NATURAL IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
          RETURN DIGT;
     END FINIT_POS;

     TASK T1 IS
          ENTRY E1 (NATURAL RANGE 1 .. 2) (P1 : IN NATURAL);
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1 (1) (P1 : IN NATURAL);
     END T1;

BEGIN

     TEST ("C95033B", "EVALUATION OF ENTRY INDEX AND OF " &
                      "EXPRESSIONS IN PARAMETER LIST");

     T1.E1 (FINIT_POS (1)) (FINIT_POS (2));
     IF SPYNUMB /= 12 THEN
          FAILED ("ENTRY INDEX NOT EVALUATED FIRST");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C95033B;
