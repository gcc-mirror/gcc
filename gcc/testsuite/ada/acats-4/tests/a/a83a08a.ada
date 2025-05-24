-- A83A08A.ADA

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
--     A STATEMENT LABEL DECLARED OUTSIDE A BLOCK CAN HAVE THE SAME
--     IDENTIFIER AS AN ENTITY DECLARED IN THE BLOCK, AND A GOTO
--     STATEMENT USING THE LABEL IS LEGAL OUTSIDE THE BLOCK.

-- HISTORY:
--     PMW 09/20/88  CREATED ORIGINAL TEST.

WITH REPORT;  USE REPORT;
WITH SYSTEM;

PROCEDURE A83A08A IS

     PASSES : INTEGER := 0;

BEGIN
     TEST ("A83A08A", "A STATEMENT LABEL DECLARED OUTSIDE A BLOCK " &
                      "CAN HAVE THE SAME IDENTIFIER AS AN ENTITY " &
                      "DECLARED IN THE BLOCK, AND A GOTO STATEMENT " &
                      "USING THE LABEL IS LEGAL OUTSIDE THE BLOCK");

     GOTO LBLS;

     <<LBL>>

     DECLARE
          LBL : INTEGER := 1;
     BEGIN
          LBL := IDENT_INT (LBL);
          PASSES := PASSES + 1;
     END;

     <<LBLS>>

     BEGIN
          DECLARE
               TYPE STUFF IS (LBL, LBL_ONE, LBL_TWO);
               ITEM : STUFF := LBL;

               FUNCTION LBLS (ITEM : STUFF) RETURN BOOLEAN IS
               BEGIN
                    <<LBL_2>>
                    CASE ITEM IS
                         WHEN LBL => RETURN TRUE;
                         WHEN LBL_ONE => PASSES := PASSES + 1;
                         WHEN LBL_TWO => RETURN FALSE;
                    END CASE;
                    IF PASSES < 2 THEN
                         PASSES := PASSES + 1;
                         GOTO LBL_2;
                    ELSE
                         RETURN TRUE;
                    END IF;
               END LBLS;

          BEGIN
               CASE PASSES IS
                    WHEN 0 => ITEM := LBL;
                    WHEN 1 => ITEM := LBL_ONE;
                    WHEN OTHERS => ITEM := LBL_TWO;
               END CASE;
               IF NOT LBLS (ITEM) THEN
                    COMMENT ("IRRELEVANT");
               END IF;
          END;
     END;


     IF PASSES > 1 THEN
          GOTO ENOUGH;
     END IF;
     GOTO LBL;

     <<ENOUGH>>

     RESULT;

END A83A08A;
