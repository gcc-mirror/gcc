-- A85013B.ADA

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
-- CHECK THAT:

--   A) A SUBPROGRAM OR ENTRY CAN BE RENAMED WITHIN ITS OWN BODY.

--   B) THE NEW NAME OF A SUBPROGRAM CAN BE USED IN A RENAMING
--      DECLARATION.

-- EG  02/22/84

WITH REPORT;

PROCEDURE A85013B IS

     USE REPORT;

BEGIN

     TEST("A85013B","CHECK THAT A SUBPROGRAM CAN BE RENAMED WITHIN " &
                    "ITS OWN BODY AND THAT THE NEW NAME CAN BE USED" &
                    " IN A RENAMING DECLARATION");

     DECLARE

          PROCEDURE PROC1 (A : BOOLEAN) IS
               PROCEDURE PROC2 (B : BOOLEAN := FALSE) RENAMES PROC1;
               PROCEDURE PROC3 (C : BOOLEAN := FALSE) RENAMES PROC2;
          BEGIN
               IF A THEN
                    PROC3;
               END IF;
          END PROC1;

     BEGIN

          PROC1 (TRUE);

     END;

     DECLARE

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
               PROCEDURE E1 RENAMES E;
               PROCEDURE E2 RENAMES E1;
          BEGIN
               ACCEPT E DO
                    DECLARE
                         PROCEDURE E3 RENAMES E;
                         PROCEDURE E4 RENAMES E3;
                    BEGIN
                         NULL;
                    END;
               END E;
          END T;

     BEGIN
          T.E;
     END;

     RESULT;

END A85013B;
