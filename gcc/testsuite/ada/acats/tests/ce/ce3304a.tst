-- CE3304A.TST

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
--     CHECK THAT USE_ERROR IS RAISED BY A CALL TO SET_LINE_LENGTH
--     OR TO SET_PAGE_LENGTH WHEN THE SPECIFIED VALUE IS INAPPROPRIATE
--     FOR THE EXTERNAL FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO IMPLEMENTATIONS THAT SATISFY THE
--     FOLLOWING CONDITIONS:
--       1) TEXT FILES ARE SUPPORTED
--       2) EITHER BY DEFAULT OR BY USE OF THE "FORM" PARAMETER TO
--          THE CREATE PROCEDURE, A TEXT FILE CAN BE CREATED FOR
--          WHICH AT LEAST ONE OF THE FOLLOWING CONDITIONS HOLDS:
--           A)  THERE IS A VALUE OF TYPE TEXT_IO.COUNT THAT IS NOT
--               AN APPROPRIATE LINE-LENGTH FOR THE FILE,
--             OR
--           B)  THERE IS A VALUE OF TYPE TEXT_IO.COUNT THAT IS NOT
--               AN APPROPRIATE PAGE-LENGTH FOR THE FILE.

-- MACRO SUBSTITUTIONS:
--     FOR THE MACRO SYMBOL "$FORM_STRING," SUBSTITUTE A STRING LITERAL
--     SPECIFIYING THAT THE EXTERNAL FILE MEETS BOTH OF THE CONDITIONS
--     (A) AND (B) ABOVE.  IF IT IS NOT POSSIBLE TO SATISFY BOTH
--     CONDITIONS, THEN SUBSTITUTE A STRING LITERAL SPECIFYING THAT THE
--     EXTERNAL FILE SATISFIES ONE OF THE CONDITIONS.  IF IT IS NOT
--     POSSIBLE TO SATISFY EITHER CONDITION, THEN SUBSTITUE THE NULL
--     STRING ("").
--     FOR THE MACRO SYMBOL "$INAPPROPRIATE_LINE_LENGTH," SUBSTITUTE
--     A LITERAL OF TYPE COUNT THAT IS INAPPROPRIATE AS THE LINE-LENGTH
--     FOR THE EXTERNAL FILE.  IF THERE IS NO SUCH VALUE, THEN USE -1.
--     FOR THE MACRO SYMBOL "$INAPPROPRIATE_PAGE_LENGTH," SUBSTITUTE
--     A LITERAL OF TYPE COUNT THAT IS INAPPROPRIATE AS THE PAGE-LENGTH
--     FOR THE EXTERNAL FILE.  IF THERE IS NO SUCH VALUE, THEN USE -1.

-- HISTORY:
--     PWB 07/07/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3304A IS

     FILE1,
     FILE2,
     FILE3 : FILE_TYPE;

     LINE_LENGTH_SHOULD_WORK,
     PAGE_LENGTH_SHOULD_WORK : BOOLEAN;

     INCOMPLETE : EXCEPTION;

     TEST_VALUE : COUNT;

BEGIN

     TEST ("CE3304A", "CHECK THAT USE_ERROR IS RAISED IF A CALL TO " &
                      "SET_LINE_LENGTH OR SET_PAGE_LENGTH SPECIFIES " &
                      "A VALUE THAT IS INAPPROPRIATE FOR THE " &
                      "EXTERNAL FILE");

     BEGIN     -- CHECK WHETHER TEXT FILES ARE SUPPORTED.

          CREATE(FILE1, OUT_FILE, LEGAL_FILE_NAME(1),
                 FORM => $FORM_STRING);
          PUT_LINE(FILE1, "AAA");
          CLOSE(FILE1);

     EXCEPTION

          WHEN USE_ERROR | NAME_ERROR =>
               NOT_APPLICABLE ("CREATION OF TEXT FILES NOT SUPPORTED");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED AT INITIAL CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN  -- CHECK INAPPROPRIATE LINE LENGTH.

          BEGIN   -- IS THERE AN INAPPROPRIATE VALUE?
               TEST_VALUE :=
                         COUNT(IDENT_INT($INAPPROPRIATE_LINE_LENGTH));
               IF NOT EQUAL (INTEGER(TEST_VALUE),
                             INTEGER(TEST_VALUE)) THEN
                    COMMENT ("OPTIMIZATION DEFEATED" &
                              COUNT'IMAGE(TEST_VALUE));
               END IF;
               LINE_LENGTH_SHOULD_WORK := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    LINE_LENGTH_SHOULD_WORK := FALSE;
                    COMMENT("THERE IS NO INAPPROPRIATE LINE LENGTH");
          END;

          IF LINE_LENGTH_SHOULD_WORK THEN
               BEGIN
                    CREATE(FILE2, OUT_FILE, LEGAL_FILE_NAME(2),
                           FORM => $FORM_STRING);
                    SET_LINE_LENGTH(FILE2, $INAPPROPRIATE_LINE_LENGTH);
                    FAILED("NO EXCEPTION FOR INAPPROPRIATE LINE " &
                           "LENGTH");
               EXCEPTION
                    WHEN USE_ERROR =>
                         IF NOT IS_OPEN(FILE2) THEN
                              FAILED ("FILE NOT OPENED -- LINE LENGTH");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED FOR " &
                                 "INAPPROPRIATE LINE LENGTH");
               END;
          END IF;
     END;

-----------------------------------------------------------------------

     BEGIN  -- CHECK INAPPROPRIATE PAGE LENGTH.

          BEGIN   -- IS THERE AN INAPPROPRIATE VALUE?
               TEST_VALUE :=
                         COUNT(IDENT_INT($INAPPROPRIATE_PAGE_LENGTH));
               IF NOT EQUAL (INTEGER(TEST_VALUE),
                             INTEGER(TEST_VALUE)) THEN
                    COMMENT ("OPTIMIZATION DEFEATED" &
                             COUNT'IMAGE(TEST_VALUE));
               END IF;
               PAGE_LENGTH_SHOULD_WORK := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    PAGE_LENGTH_SHOULD_WORK := FALSE;
                    COMMENT("THERE IS NO INAPPROPRIATE PAGE LENGTH");
          END;

          IF PAGE_LENGTH_SHOULD_WORK THEN
               BEGIN
                    CREATE(FILE3, OUT_FILE, LEGAL_FILE_NAME(3),
                           FORM => $FORM_STRING);
                    SET_PAGE_LENGTH(FILE3, $INAPPROPRIATE_PAGE_LENGTH);
                    FAILED("NO EXCEPTION FOR INAPPROPRIATE PAGE " &
                           "LENGTH");
               EXCEPTION
                    WHEN USE_ERROR =>
                         IF NOT IS_OPEN(FILE3) THEN
                              FAILED ("FILE NOT OPENED -- PAGE LENGTH");
                         END IF;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED FOR " &
                                 "INAPPROPRIATE PAGE LENGTH");
               END;
          END IF;
     END;

     IF NOT (PAGE_LENGTH_SHOULD_WORK OR LINE_LENGTH_SHOULD_WORK) THEN
          NOT_APPLICABLE("NO INAPPROPRIATE VALUES FOR EITHER LINE " &
                         "LENGTH OR PAGE LENGTH");
     END IF;

     BEGIN     -- CLEAN UP FILES.

          IF IS_OPEN(FILE1) THEN
               CLOSE(FILE1);
          END IF;

          IF IS_OPEN(FILE2) THEN
               CLOSE(FILE2);
          END IF;

          IF IS_OPEN(FILE3) THEN
               CLOSE(FILE3);
          END IF;

     EXCEPTION
          WHEN USE_ERROR =>
               COMMENT("FILES NOT DELETED");
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;
END CE3304A;
