-- CE2102G.ADA

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
--     CHECK THAT USE_ERROR IS RAISED IF AN IMPLEMENTATION DOES NOT
--     SUPPORT RESET FOR SEQUENTIAL_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     SPS 08/27/82
--     JBG 06/04/84
--     TBN 02/12/86  SPLIT TEST.  PUT DIRECT_IO INTO CE2102K.ADA.
--     TBN 09/15/87  COMPLETELY REVISED TEST.

WITH SEQUENTIAL_IO;
WITH REPORT; USE REPORT;
PROCEDURE CE2102G IS
     INCOMPLETE : EXCEPTION;
BEGIN
     TEST ("CE2102G", "CHECK THAT USE_ERROR IS RAISED IF AN " &
                      "IMPLEMENTATION DOES NOT SUPPORT RESET FOR " &
                      "SEQUENTIAL_IO");
     DECLARE
          PACKAGE SEQ IS NEW SEQUENTIAL_IO (INTEGER);
          USE SEQ;
          FILE1 : FILE_TYPE;
          INT1 : INTEGER := IDENT_INT(1);
          INT2 : INTEGER := 2;
     BEGIN
          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE OF " &
                                    "SEQUENTIAL FILE WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE OF " &
                                    "SEQUENTIAL FILE WITH OUT_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE1, INT2);
          BEGIN
               RESET (FILE1, IN_FILE);
               COMMENT ("RESET FROM OUT_FILE TO IN_FILE IS ALLOWED");
               BEGIN
                    READ (FILE1, INT1);
                    IF INT1 /= IDENT_INT(2) THEN
                         FAILED ("RESETTING FROM OUT_FILE TO IN_FILE " &
                                 "AFFECTED DATA");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED WHILE " &
                                 "READING FROM FILE");
               END;
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM OUT_FILE TO IN_FILE IS NOT " &
                             "ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM OUT_FILE TO IN_FILE");
          END;

          CLOSE (FILE1);

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPENING OF " &
                                    "SEQUENTIAL FILE WITH IN_FILE " &
                                    "MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               RESET (FILE1, OUT_FILE);
               COMMENT ("RESET FROM IN_FILE TO OUT_FILE IS ALLOWED");
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM IN_FILE TO OUT_FILE IS NOT " &
                             "ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM IN_FILE TO OUT_FILE");
          END;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;
END CE2102G;
