-- CE2204A.ADA

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
--     CHECK THAT WRITE IS FORBIDDEN FOR SEQUENTIAL FILES OF
--     MODE IN_FILE.

--          A)  CHECK NON-TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     DLD 08/17/82
--     SPS 08/24/82
--     SPS 11/09/82
--     JBG 02/22/84  CHANGE TO .ADA TEST.
--     JBG 03/30/84
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 07/27/87  SPLIT THIS TEST BY MOVING THE CODE FOR CHECKING
--                   TEMPORARY FILES INTO CE2204C.ADA.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2204A IS
     INCOMPLETE : EXCEPTION;
BEGIN
     TEST ("CE2204A", "CHECK THAT MODE_ERROR IS RAISED BY WRITE " &
                      "WHEN THE MODE IS IN_FILE AND THE FILE " &
                      "IS A NON-TEMPORARY FILE");
     DECLARE
          PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO (INTEGER);
          USE SEQ_IO;
          SEQ_FILE : FILE_TYPE;
          VAR1     : INTEGER := 5;
     BEGIN
          BEGIN
               CREATE (SEQ_FILE, OUT_FILE,
                       LEGAL_FILE_NAME (1, "CE2204A"));
               WRITE (SEQ_FILE, VAR1);
               CLOSE (SEQ_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; " &
                                    "SEQUENTIAL CREATE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; " &
                                    "SEQUENTIAL CREATE");
                    RAISE INCOMPLETE;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED; " &
                            "SEQUENTIAL CREATE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               OPEN (SEQ_FILE, IN_FILE,
                     LEGAL_FILE_NAME (1, "CE2204A"));
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR  RAISED ON THE " &
                                    "OPENING OF A SEQUENTIAL " &
                                    "NON-TEMPORARY FILE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               WRITE (SEQ_FILE, 3);
               FAILED ("MODE_ERROR NOT RAISED - NAMED FILE");
          EXCEPTION
               WHEN MODE_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - NAMED FILE");
          END;

          BEGIN
               DELETE (SEQ_FILE);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2204A;
