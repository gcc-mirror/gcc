-- CE2106A.ADA

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
--     CHECK THAT AFTER A SUCCESSFUL DELETE OF AN EXTERNAL FILE, THE
--     NAME OF THE FILE CAN BE USED IN A CREATE OPERATION.

--          A) SEQUENTIAL FILES

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION WITH OUT_FILE MODE FOR SEQUENTIAL FILES AND
--     DELETION OF EXTERNAL FILES.

-- HISTORY:
--     SPS 08/25/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 02/12/86  SPLIT TEST.  PUT DIRECT_IO INTO CE2106B.ADA.
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     SPW 08/07/87  INSERTED ALLOWABLE EXCEPTION USE_ERROR ON
--                   DELETE.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2106A IS

BEGIN

     TEST ("CE2106A", "CHECK THAT AN EXTERNAL FILE CAN BE CREATED " &
                      "AFTER AN EXTERNAL FILE WITH SAME NAME HAS " &
                      "BEEN DELETED FOR SEQUENTIAL_IO");

     DECLARE
          PACKAGE SEQ IS NEW SEQUENTIAL_IO (INTEGER);
          USE SEQ;
          FL1 : FILE_TYPE;
          FL2 : FILE_TYPE;
          T_FAILED : BOOLEAN := FALSE;
          D_FILE : BOOLEAN := FALSE;
     BEGIN
          BEGIN
               CREATE (FL1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; SEQUENTIAL " &
                                    "CREATE WITH OUT_FILE MODE");
                    T_FAILED := TRUE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; SEQUENTIAL " &
                                    "CREATE WITH OUT_FILE MODE");
                    T_FAILED := TRUE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED; SEQUENTIAL " &
                            "CREATE");
                    T_FAILED := TRUE;
          END;

          IF NOT T_FAILED THEN
               BEGIN
                    DELETE (FL1);
               EXCEPTION
                    WHEN USE_ERROR =>
                         NOT_APPLICABLE ("DELETION OF EXTERNAL FILE " &
                                         "IS NOT SUPPORTED");
                         T_FAILED := TRUE;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED ON " &
                                 "DELETE");
                         T_FAILED := TRUE;
               END;
          END IF;

          IF NOT T_FAILED THEN
               BEGIN
                    CREATE (FL2, OUT_FILE, LEGAL_FILE_NAME);
                    D_FILE := TRUE;
               EXCEPTION
                    WHEN USE_ERROR =>
                         FAILED ("USE_ERROR FOR RECREATE - SEQ");
                    WHEN OTHERS =>
                         FAILED ("UNABLE TO RECREATE FILE AFTER " &
                                 "DELETION - SEQ");
               END;

               IF D_FILE THEN
                    BEGIN
                         DELETE (FL2);
                    EXCEPTION
                         WHEN USE_ERROR =>
                              FAILED ("USE_ERROR FOR DELETE - SEQ");
                    END;
               END IF;
          END IF;
     END;

     RESULT;

END CE2106A;
