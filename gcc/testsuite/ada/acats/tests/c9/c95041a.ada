-- C95041A.ADA

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
--     CHECK THAT AN ENTRY FAMILY INDEX CAN BE SPECIFIED WITH THE FORM
--     A'RANGE.

-- HISTORY:
--     DHH 03/17/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C95041A IS

     GLOBAL_A, GLOBAL_B : INTEGER;
     GLOBAL_C, GLOBAL_D : INTEGER;
     TYPE COLOR IS (RED, BLUE, YELLOW);
     TYPE ARR IS ARRAY(COLOR RANGE RED .. BLUE) OF BOOLEAN;
     ARRY : ARR;

     TASK CHECK IS
          ENTRY CHECK_LINK(ARR'RANGE)(I : INTEGER);
     END CHECK;

     TASK CHECK_OBJ IS
          ENTRY CHECK_OBJ_LINK(ARRY'RANGE)(I : INTEGER);
     END CHECK_OBJ;

     TASK BODY CHECK IS
     BEGIN
               ACCEPT CHECK_LINK(RED)(I : INTEGER) DO
                    GLOBAL_A := IDENT_INT(I);
               END;

               ACCEPT CHECK_LINK(BLUE)(I : INTEGER) DO
                    GLOBAL_B := IDENT_INT(I);
               END;
     END CHECK;

     TASK BODY CHECK_OBJ IS
     BEGIN
               ACCEPT CHECK_OBJ_LINK(RED)(I : INTEGER) DO
                    GLOBAL_C := IDENT_INT(I);
               END;

               ACCEPT CHECK_OBJ_LINK(BLUE)(I : INTEGER) DO
                    GLOBAL_D := IDENT_INT(I);
               END;
     END CHECK_OBJ;

BEGIN
     TEST("C95041A", "CHECK THAT AN ENTRY FAMILY INDEX CAN BE " &
                     "SPECIFIED WITH THE FORM A'RANGE");
     CHECK.CHECK_LINK(RED)(10);
     CHECK.CHECK_LINK(BLUE)(5);

     CHECK_OBJ.CHECK_OBJ_LINK(RED)(10);
     CHECK_OBJ.CHECK_OBJ_LINK(BLUE)(5);

     IF GLOBAL_A /= IDENT_INT(10) THEN
          FAILED("ENTRY CHECK_LINK(RED) HAS INCORRECT VALUE");
     END IF;

     IF GLOBAL_B /= IDENT_INT(5) THEN
          FAILED("ENTRY CHECK_LINK(BLUE) HAS INCORRECT VALUE");
     END IF;

     IF GLOBAL_C /= IDENT_INT(10) THEN
          FAILED("ENTRY CHECK_LINK(RED) HAS INCORRECT VALUE");
     END IF;

     IF GLOBAL_D /= IDENT_INT(5) THEN
          FAILED("ENTRY CHECK_LINK(BLUE) HAS INCORRECT VALUE");
     END IF;

     RESULT;
END C95041A;
