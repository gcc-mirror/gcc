-- CE3115A.ADA

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
--     CHECK THAT RESETTING ONE OF A MULTIPLE OF INTERNAL FILES
--     ASSOCIATED WITH THE SAME EXTERNAL FILE HAS NO EFFECT ON ANY
--     OF THE OTHER INTERNAL FILES.


-- APPLICABILITY CRITERIA:
--     THIS TEST APPLIES ONLY TO IMPLEMENTATIONS WHICH SUPPORT MULTIPLE
--     INTERNAL FILES ASSOCIATED WITH THE SAME EXTERNAL FILE AND
--     RESETTING OF THESE MULTIPLE INTERNAL FILES FOR TEXT FILES.

-- HISTORY:
--     DLD 08/16/82
--     SPS 11/09/82
--     JBG 06/04/84
--     EG  11/19/85  MADE TEST INAPPLICABLE IF CREATE USE_ERROR.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE RESULT WHEN
--                   FILES NOT SUPPORTED.
--     GMT 08/25/87  COMPLETELY REVISED.
--     EDS 12/01/97  ADD NAME_ERROR HANDLER TO OUTPUT NOT_APPLICABLE RESULT.
--     RLB 09/29/98  MADE MODIFICATION TO AVOID BUFFERING PROBLEMS.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3115A IS

BEGIN

     TEST ("CE3115A", "CHECK THAT RESETTING ONE OF A MULTIPLE OF " &
                      "INTERNAL FILES ASSOCIATED WITH THE SAME " &
                      "EXTERNAL FILE HAS NO EFFECT ON ANY OF THE " &
                      "OTHER INTERNAL FILES");

     DECLARE
          TXT_FILE_ONE : TEXT_IO.FILE_TYPE;
          TXT_FILE_TWO : TEXT_IO.FILE_TYPE;

          CH           : CHARACTER := 'A';

          INCOMPLETE   : EXCEPTION;

          PROCEDURE TXT_CLEANUP IS
               FILE1_OPEN : BOOLEAN := IS_OPEN (TXT_FILE_ONE);
               FILE2_OPEN : BOOLEAN := IS_OPEN (TXT_FILE_TWO);
          BEGIN
               IF FILE1_OPEN AND FILE2_OPEN THEN
                    CLOSE (TXT_FILE_TWO);
                    DELETE (TXT_FILE_ONE);
               ELSIF FILE1_OPEN THEN
                    DELETE (TXT_FILE_ONE);
               ELSIF FILE2_OPEN THEN
                    DELETE (TXT_FILE_TWO);
               END IF;
          EXCEPTION
               WHEN TEXT_IO.USE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED " &
                            "IN CLEANUP - 1");
          END TXT_CLEANUP;

     BEGIN

          BEGIN -- CREATE FIRST FILE

               CREATE (TXT_FILE_ONE, OUT_FILE, LEGAL_FILE_NAME);
               PUT (TXT_FILE_ONE, CH);

          EXCEPTION
               WHEN TEXT_IO.USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; CREATE OF " &
                                    "EXTERNAL FILENAME IS NOT " &
                                    "SUPPORTED - 2");
                    RAISE INCOMPLETE;
               WHEN TEXT_IO.NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; CREATE OF " &
                                    "EXTERNAL FILENAME IS NOT " &
                                    "SUPPORTED - 3");
                    RAISE INCOMPLETE;

          END; -- CREATE FIRST FILE

          BEGIN -- OPEN SECOND FILE

               OPEN (TXT_FILE_TWO, IN_FILE, LEGAL_FILE_NAME);

          EXCEPTION

               WHEN TEXT_IO.USE_ERROR =>
                    NOT_APPLICABLE ("MULTIPLE INTERNAL FILES ARE NOT " &
                                    "SUPPORTED WHEN ONE IS MODE " &
                                    "OUT_FILE AND THE OTHER IS MODE " &
                                    "IN_FILE - 4" &
                                    " - USE_ERROR RAISED ");
                    TXT_CLEANUP;
                    RAISE INCOMPLETE;

               WHEN TEXT_IO.NAME_ERROR =>
                    NOT_APPLICABLE ("MULTIPLE INTERNAL FILES ARE NOT " &
                                    "SUPPORTED WHEN ONE IS MODE " &
                                    "OUT_FILE AND THE OTHER IS MODE " &
                                    "IN_FILE - 4" &
                                    " - NAME_ERROR RAISED ");
                    TXT_CLEANUP;
                    RAISE INCOMPLETE;

          END; -- OPEN SECOND FILE
          FLUSH (TXT_FILE_ONE); -- AVOID BUFFERING PROBLEMS.

          CH := 'B';
          GET (TXT_FILE_TWO, CH);
          IF CH /= 'A' THEN
               FAILED ("INCORRECT VALUE FOR GET - 5");
          END IF;

          BEGIN -- INITIALIZE FIRST FILE TO CHECK POINTER RESETTING

               RESET (TXT_FILE_ONE);
               IF MODE (TXT_FILE_ONE) /= OUT_FILE THEN
                    FAILED ("FILE WAS NOT RESET - 6");
               END IF;
               IF MODE (TXT_FILE_TWO) /= IN_FILE THEN
                    FAILED ("RESETTING OF ONE INTERNAL FILE " &
                            "AFFECTED THE OTHER INTERNAL FILE - 7");
               END IF;

          EXCEPTION

               WHEN TEXT_IO.USE_ERROR =>
                    NOT_APPLICABLE ("RESETTING OF EXTERNAL FILE FOR " &
                                    "OUT_FILE MODE IS " &
                                    " NOT SUPPORTED - 8");
                    TXT_CLEANUP;
                    RAISE INCOMPLETE;

          END; -- INITIALIZE FIRST FILE TO CHECK POINTER RESETTING

          -- PERFORM SOME I/O ON THE FIRST FILE

          PUT (TXT_FILE_ONE, 'C');
          PUT (TXT_FILE_ONE, 'D');
          PUT (TXT_FILE_ONE, 'E');
          CLOSE (TXT_FILE_ONE);

          BEGIN
               OPEN (TXT_FILE_ONE, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("MULTIPLE INTERNAL FILES NOT " &
                                    "SUPPORTED WHEN BOTH FILES HAVE " &
                                    "IN_FILE MODE - 9");
                    RAISE INCOMPLETE;
          END;

          GET (TXT_FILE_ONE, CH);
          GET (TXT_FILE_ONE, CH);

          BEGIN -- INITIALIZE SECOND FILE AND PERFORM SOME I/O

               CLOSE (TXT_FILE_TWO);
               OPEN (TXT_FILE_TWO, IN_FILE, LEGAL_FILE_NAME);

          EXCEPTION

               WHEN TEXT_IO.USE_ERROR =>
                    FAILED ("MULTIPLE INTERNAL FILES SHOULD STILL " &
                            "BE ALLOWED - 10");
                    TXT_CLEANUP;
                    RAISE INCOMPLETE;

          END; -- INITIALIZE SECOND FILE AND PERFORM SOME I/O

          BEGIN -- RESET FIRST FILE AND CHECK EFFECTS ON SECOND FILE

               GET (TXT_FILE_TWO, CH);
               IF CH /= 'C' THEN
                    FAILED ("INCORRECT VALUE FOR GET OPERATION - 11");
               END IF;

               RESET (TXT_FILE_ONE);
               GET (TXT_FILE_TWO, CH);
               IF CH /= 'D' THEN
                    FAILED ("RESETTING INDEX OF ONE TEXT FILE " &
                            "RESETS THE OTHER ASSOCIATED FILE - 12");
               END IF;

          EXCEPTION

               WHEN TEXT_IO.USE_ERROR =>
                    FAILED ("RESETTING SHOULD STILL BE SUPPORTED - 13");
                    TXT_CLEANUP;
                    RAISE INCOMPLETE;

          END; -- RESET FIRST FILE AND CHECK EFFECTS ON SECOND FILE

          TXT_CLEANUP;

     EXCEPTION

          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3115A;
