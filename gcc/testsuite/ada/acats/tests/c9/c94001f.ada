-- C94001F.ADA

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
-- CHECK THAT A TASK IS ALSO COMPLETED IF AN EXCEPTION IS RAISED BY
-- THE EXECUTION OF ITS SEQUENCE OF STATEMENTS.
-- THIS MUST HOLD FOR BOTH CASES WHERE A HANDLER IS PRESENT OR NOT.
--   VERSION WITHOUT EXCEPTION HANDLER.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C940AGB-B.ADA

WITH REPORT;
 USE REPORT;
PROCEDURE C94001F IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

BEGIN

     TEST ("C94001F", "TASK COMPLETION BY EXCEPTION -- NO HANDLER");

BLOCK:
     DECLARE

          TASK T1;

          TASK BODY T1 IS
               TYPE I1 IS RANGE 0 .. 1;
               OBJ_I1 : I1;
          BEGIN
               OBJ_I1 := I1(IDENT_INT(2));  -- CONSTRAINT_ERROR.
               PSPY_NUMB (1);
          END T1;

     BEGIN
          NULL;          -- WAIT FOR TERMINATION.
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("PROPAGATED CONSTRAINT_ERROR OUT OF TASK");
          WHEN TASKING_ERROR =>
               FAILED ("RAISED TASKING_ERROR");
          WHEN OTHERS =>
               FAILED ("RAISED OTHER EXCEPTION");
     END BLOCK;

     IF SPYNUMB /= 0 THEN
          FAILED ("TASK T1 NOT COMPLETED AFTER EXCEPTION IN SEQUENCE " &
                  "OF STATEMENTS");
     END IF;

     RESULT;

END C94001F;
