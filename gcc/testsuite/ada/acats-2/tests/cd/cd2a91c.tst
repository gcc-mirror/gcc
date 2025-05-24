-- CD2A91C.TST

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
--     CHECK THAT A SIZE SPECIFICATION FOR A TASK TYPE CAN BE GIVEN IN
--     THE VISIBLE OR PRIVATE PART OF A PACKAGE.

-- MACRO SUBSTITUTION:
--     $TASK_SIZE IS THE NUMBER OF BITS NEEDED BY THE IMPLEMENTATION TO
--     HOLD ANY POSSIBLE OBJECT OF THE TASK TYPE "BASIC_TYPE".

-- HISTORY:
--     BCB 09/08/87  CREATED ORIGINAL TEST.
--     RJW 05/12/89  MODIFIED CHECKS INVOLVING 'SIZE ATTRIBUTE.
--                   REMOVED APPLICABILTY CRITERIA.
--     DTN 11/20/91  DELETED SUBPARTS (B and C).

WITH REPORT; USE REPORT;
PROCEDURE CD2A91C IS

     BASIC_SIZE : CONSTANT := $TASK_SIZE;

     VAL : INTEGER := 1;

     TASK TYPE BASIC_TYPE IS
          ENTRY HERE(NUM : IN OUT INTEGER);
     END BASIC_TYPE;

     FOR BASIC_TYPE'SIZE USE BASIC_SIZE;

     BASIC_TASK : BASIC_TYPE;

     PACKAGE P IS
          TASK TYPE TASK_IN_P IS
               ENTRY HERE(NUM : IN OUT INTEGER);
          END TASK_IN_P;
          FOR TASK_IN_P'SIZE USE BASIC_SIZE;
          TASK TYPE ALT_TASK_IN_P IS
               ENTRY HERE(NUM : IN OUT INTEGER);
          END ALT_TASK_IN_P;
     PRIVATE
          FOR ALT_TASK_IN_P'SIZE USE BASIC_SIZE;
     END P;

     USE P;

     ALT_TASK : ALT_TASK_IN_P;
     IN_TASK : TASK_IN_P;

     TASK BODY BASIC_TYPE IS
     BEGIN
          SELECT
               ACCEPT HERE(NUM : IN OUT INTEGER) DO
                     NUM := 0;
               END HERE;
          OR
               TERMINATE;
          END SELECT;
     END BASIC_TYPE;

     PACKAGE BODY P IS
          TASK BODY TASK_IN_P IS
          BEGIN
               SELECT
                    ACCEPT HERE(NUM : IN OUT INTEGER) DO
                          NUM := 0;
                    END HERE;
               OR
                    TERMINATE;
               END SELECT;
          END TASK_IN_P;
          TASK BODY ALT_TASK_IN_P IS
          BEGIN
               SELECT
                    ACCEPT HERE(NUM : IN OUT INTEGER) DO
                          NUM := 0;
                    END HERE;
               OR
                    TERMINATE;
               END SELECT;
          END ALT_TASK_IN_P;
     END P;

BEGIN
     TEST ("CD2A91C", "CHECK THAT A SIZE SPECIFICATION FOR A TASK " &
                      "TYPE CAN BE GIVEN IN THE VISIBLE OR PRIVATE " &
                      "PART OF A PACKAGE");

     BASIC_TASK.HERE(VAL);

     IF VAL /= IDENT_INT (0) THEN
          FAILED ("INCORRECT RESULTS FROM ENTRY CALL - 1");
     END IF;

     VAL := 1;

     ALT_TASK.HERE(VAL);
 
     IF VAL /= IDENT_INT (0) THEN
          FAILED ("INCORRECT RESULTS FROM ENTRY CALL - 2");
     END IF;
 
     VAL := 1;

     IN_TASK.HERE(VAL);

     IF VAL /= IDENT_INT (0) THEN
          FAILED ("INCORRECT RESULTS FROM ENTRY CALL - 3");
     END IF;


     RESULT;
END CD2A91C;
