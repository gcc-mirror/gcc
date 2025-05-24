-- CE3103A.ADA

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
--     CHECK THAT THE PAGE AND LINE LENGTH OF TEXT FILES ARE ZERO
--     AFTER A CREATE, OPEN, OR RESET TO OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILE.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     SPS 01/18/83
--     EG  11/02/84
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/13/87  REVISED TEST TO INCLUDE CASES TO RESET THE FILE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3103A IS

     SUBTEST : EXCEPTION;
     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     ZERO : CONSTANT COUNT := COUNT(IDENT_INT(0));
     TWO  : CONSTANT COUNT := COUNT (IDENT_INT(2));
     FIVE : CONSTANT COUNT := COUNT (IDENT_INT(5));

BEGIN

     TEST ("CE3103A" , "CHECK THAT PAGE AND LINE LENGTH " &
                       "ARE SET TO ZERO AFTER CREATE, " &
                       "OPEN, OR RESET");

BEGIN

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     IF LINE_LENGTH (FILE) /= ZERO THEN
          FAILED ("LINE_LENGTH FOR CREATE IS NOT ZERO");
     END IF;
     IF PAGE_LENGTH (FILE) /= ZERO THEN
          FAILED ("PAGE_LENGTH FOR CREATE IS NOT ZERO");
     END IF;

     SET_LINE_LENGTH (FILE, TWO);
     SET_PAGE_LENGTH (FILE, FIVE);

     PUT_LINE (FILE, "HI");

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED; TEXT OPEN");
               RAISE INCOMPLETE;
     END;

     IF LINE_LENGTH (FILE) /= ZERO THEN
          FAILED ("LINE_LENGTH FOR OPEN IS NOT ZERO");
     END IF;
     IF PAGE_LENGTH (FILE) /= ZERO THEN
          FAILED ("PAGE_LENGTH FOR OPEN IS NOT ZERO");
     END IF;

     SET_LINE_LENGTH (FILE, TWO);
     SET_PAGE_LENGTH (FILE, TWO);

     PUT_LINE (FILE, "HI");

     BEGIN
          BEGIN
               RESET (FILE, OUT_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    RAISE SUBTEST;
          END;

          IF LINE_LENGTH (FILE) /= ZERO THEN
               FAILED ("LINE_LENGTH FOR RESET TO OUT_FILE IS NOT " &
                       "ZERO - 1");
          END IF;
          IF PAGE_LENGTH (FILE) /= ZERO THEN
               FAILED ("PAGE_LENGTH FOR RESET TO OUT_FILE IS NOT " &
                       "ZERO - 1");
          END IF;
     EXCEPTION
          WHEN SUBTEST =>
               NULL;
     END;

     SET_LINE_LENGTH (FILE, FIVE);
     SET_PAGE_LENGTH (FILE, FIVE);

     PUT_LINE (FILE, "HELLO");

     IF LINE_LENGTH (FILE) /= 5 THEN
          FAILED ("LINE_LENGTH FOR RESET IN OUT_FILE, PLUS HELLO " &
                  "IS NOT FIVE");
     END IF;
     IF PAGE_LENGTH (FILE) /= 5 THEN
          FAILED ("PAGE_LENGTH FOR RESET IN OUT_FILE, PLUS HELLO " &
                  "IS NOT FIVE");
     END IF;

     BEGIN
          BEGIN
               RESET (FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    RAISE SUBTEST;
          END;

          IF LINE_LENGTH (FILE) /= ZERO THEN
               FAILED ("LINE_LENGTH FOR RESET IS NOT ZERO");
          END IF;
          IF PAGE_LENGTH (FILE) /= ZERO THEN
               FAILED ("PAGE_LENGTH FOR RESET IS NOT ZERO");
          END IF;
     EXCEPTION
          WHEN SUBTEST =>
               NULL;
     END;

     SET_LINE_LENGTH (FILE, FIVE);
     SET_PAGE_LENGTH (FILE, FIVE);

     PUT_LINE (FILE, "HELLO");

     IF LINE_LENGTH (FILE) /= 5 THEN
          FAILED ("LINE_LENGTH FOR RESET PLUS HELLO");
     END IF;
     IF PAGE_LENGTH (FILE) /= 5 THEN
          FAILED ("PAGE_LENGTH FOR RESET PLUS HELLO");
     END IF;

     CLOSE (FILE);

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               RAISE INCOMPLETE;
     END;

     BEGIN
          RESET (FILE, OUT_FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               RAISE INCOMPLETE;
     END;

     IF LINE_LENGTH (FILE) /= ZERO THEN
          FAILED ("LINE_LENGTH FOR RESET TO OUT_FILE IS NOT ZERO - 2");
     END IF;
     IF PAGE_LENGTH (FILE) /= ZERO THEN
          FAILED ("PAGE_LENGTH FOR RESET TO OUT_FILE IS NOT ZERO - 2");
     END IF;

     BEGIN
          DELETE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

EXCEPTION
     WHEN INCOMPLETE =>
          NULL;
END;

RESULT;

END CE3103A;
