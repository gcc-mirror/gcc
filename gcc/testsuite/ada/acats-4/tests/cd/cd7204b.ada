-- CD7204B.ADA

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
--     CHECK THAT THE PREFIX OF THE 'POSITION, 'LAST_BIT, AND 'FIRST_BIT
--     ATTRIBUTES CAN DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES
--     RETURN APPROPRIATE VALUES WHEN A RECORD REPRESENTATION CLAUSE IS
--     NOT PRESENT.

-- HISTORY:
--     BCB 09/14/87  CREATED ORIGINAL TEST.
--     RJW 02/08/88  REVISED SO THAT TEST PASSES IF BOOLEAN'SIZE = 1.
--     RJW 05/31/90  CORRECTED COMPARISONS INVOLVING SIZES.
--     LDC 10/04/90  ADDED CHECK FOR 'POSITION.

WITH REPORT;  USE REPORT;

PROCEDURE CD7204B IS

     TYPE BASIC_REC IS RECORD
          CHECK_INT : INTEGER := 5;
          CHECK_BOOL : BOOLEAN := TRUE;
     END RECORD;

     CHECK_REC : BASIC_REC;

BEGIN

     TEST ("CD7204B", "CHECK THAT THE PREFIX OF THE 'POSITION, " &
                      "'LAST_BIT, AND 'FIRST_BIT ATTRIBUTES CAN " &
                      "DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES " &
                      "RETURN APPROPRIATE VALUES WHEN A RECORD " &
                      "REPRESENTATION CLAUSE IS NOT PRESENT");

     IF CHECK_REC.CHECK_INT'FIRST_BIT >= CHECK_REC.CHECK_INT'LAST_BIT
          THEN FAILED ("INCORRECT VALUES FOR FIRST_BIT OR LAST_BIT " &
                       "OF CHECK_INT");
     END IF;

     IF (CHECK_REC.CHECK_INT'LAST_BIT - CHECK_REC.CHECK_INT'FIRST_BIT
          + 1) < INTEGER'SIZE THEN
          FAILED ("INCORRECT SIZE FOR CHECK_INT");
     END IF;

     IF CHECK_REC.CHECK_BOOL'POSITION <= CHECK_REC.CHECK_INT'POSITION
          THEN FAILED ("INCORRECT VALUE FOR 'POSITION OF CHECK_INT " &
                       "OR CHECK_BOOL");
     END IF;

     IF CHECK_REC.CHECK_INT'POSITION >= CHECK_REC.CHECK_BOOL'POSITION
          THEN FAILED ("INCORRECT VALUE FOR 'POSITION OF CHECK_INT " &
                       "OR CHECK_BOOL - 2");
     END IF;

     IF CHECK_REC.CHECK_BOOL'FIRST_BIT > CHECK_REC.CHECK_BOOL'LAST_BIT
          THEN FAILED ("INCORRECT VALUE FOR FIRST_BIT OR LAST_BIT " &
                       "OF CHECK_BOOL");
     END IF;

     IF (CHECK_REC.CHECK_BOOL'LAST_BIT - CHECK_REC.CHECK_BOOL'FIRST_BIT
          + 1) < BOOLEAN'SIZE THEN
          FAILED ("INCORRECT SIZE FOR CHECK_BOOL");
     END IF;

     RESULT;

END CD7204B;
