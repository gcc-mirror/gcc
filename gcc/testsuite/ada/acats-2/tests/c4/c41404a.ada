-- C41404A.ADA

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
-- CHECK THAT THE PREFIX OF THE ARRAY ATTRIBUTES CAN BE THE VALUE OF AN
-- IMAGE ATTRIBUTE.

-- JBG 6/1/85
-- PWB 2/3/86  CORRECTED COMPARISON VALUES FOR 'LAST AND 'LENGTH.

WITH REPORT; USE REPORT;
PROCEDURE C41404A IS

     TYPE ENUM IS (ONE, FOUR, 'C');

BEGIN

     TEST ("C41404A", "CHECK WHEN PREFIX OF AN ATTRIBUTE IS 'IMAGE");

     IF ENUM'IMAGE(FOUR)'LENGTH /= IDENT_INT(4) THEN
          FAILED ("WRONG VALUE FOR LENGTH - ENUM");
     END IF;

     IF ENUM'IMAGE('C')'LENGTH /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LENGTH - ENUM: 'C'");
     END IF;

     IF INTEGER'IMAGE(IDENT_INT(56))'LENGTH /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LENGTH - INTEGER: 56");
     END IF;

     IF CHARACTER'IMAGE(IDENT_CHAR('B'))'LENGTH /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LENGTH - CHAR: 'B'");
     END IF;

     IF ENUM'IMAGE(FOUR)'FIRST /= IDENT_INT(1) THEN
          FAILED ("WRONG VALUE FOR FIRST - ENUM");
     END IF;

     IF ENUM'IMAGE('C')'FIRST(1) /= IDENT_INT(1) THEN
          FAILED ("WRONG VALUE FOR FIRST - ENUM: 'C'");
     END IF;

     IF INTEGER'IMAGE(IDENT_INT(56))'FIRST /= IDENT_INT(1) THEN
          FAILED ("WRONG VALUE FOR FIRST - INTEGER: 56");
     END IF;

     IF CHARACTER'IMAGE(IDENT_CHAR('B'))'FIRST /= IDENT_INT(1) THEN
          FAILED ("WRONG VALUE FOR FIRST - CHAR: 'B'");
     END IF;

     IF ENUM'IMAGE(FOUR)'LAST /= IDENT_INT(4) THEN
          FAILED ("WRONG VALUE FOR LAST - ENUM");
     END IF;

     IF ENUM'IMAGE('C')'LAST(1) /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LAST - ENUM: 'C'");
     END IF;

     IF INTEGER'IMAGE(IDENT_INT(-56))'LAST /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LAST - INTEGER: -56");
     END IF;

     IF CHARACTER'IMAGE(IDENT_CHAR('B'))'LAST /= IDENT_INT(3) THEN
          FAILED ("WRONG VALUE FOR LAST - CHAR: 'B'");
     END IF;

     DECLARE

          FOUR_VAR : STRING(ENUM'IMAGE(FOUR)'RANGE);
          C_VAR    : STRING(ENUM'IMAGE('C')'RANGE);
          VAR_101  : STRING(INTEGER'IMAGE(IDENT_INT(101))'RANGE);
          CHAR_VAR : STRING(CHARACTER'IMAGE(IDENT_CHAR('B'))'RANGE);

     BEGIN

          IF FOUR_VAR'FIRST /= 1 OR
             FOUR_VAR'LAST  /= 4 OR
             FOUR_VAR'LENGTH /= 4 THEN
               FAILED ("FOUR_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
                       INTEGER'IMAGE(FOUR_VAR'FIRST) & ".  LAST IS" &
                       INTEGER'IMAGE(FOUR_VAR'LAST) & ".  LENGTH IS" &
                       INTEGER'IMAGE(FOUR_VAR'LENGTH));
          END IF;

          IF C_VAR'FIRST /= 1 OR
             C_VAR'LAST  /= 3 OR
             C_VAR'LENGTH /= 3 THEN
               FAILED ("C_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
                       INTEGER'IMAGE(C_VAR'FIRST) & ".  LAST IS" &
                       INTEGER'IMAGE(C_VAR'LAST) & ".  LENGTH IS" &
                       INTEGER'IMAGE(C_VAR'LENGTH));
          END IF;

          IF VAR_101'FIRST /= 1 OR
             VAR_101'LAST  /= 4 OR
             VAR_101'LENGTH /= 4 THEN
               FAILED ("VAR_101 ATTRIBUTES INCORRECT.  FIRST IS" &
                       INTEGER'IMAGE(VAR_101'FIRST) & ".  LAST IS" &
                       INTEGER'IMAGE(VAR_101'LAST) & ".  LENGTH IS" &
                       INTEGER'IMAGE(VAR_101'LENGTH));
          END IF;

          IF CHAR_VAR'FIRST /= 1 OR
             CHAR_VAR'LAST  /= 3 OR
             CHAR_VAR'LENGTH /= 3 THEN
               FAILED ("CHAR_VAR ATTRIBUTES INCORRECT.  FIRST IS" &
                       INTEGER'IMAGE(CHAR_VAR'FIRST) & ".  LAST IS" &
                       INTEGER'IMAGE(CHAR_VAR'LAST) & ".  LENGTH IS" &
                       INTEGER'IMAGE(CHAR_VAR'LENGTH));
          END IF;

     END;

     RESULT;
END C41404A;
