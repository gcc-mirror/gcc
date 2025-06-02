-- CE2102K.ADA

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
--     SUPPORT RESET FOR DIRECT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     DIRECT FILES.

-- HISTORY:
--     TBN 02/12/86  CREATED ORIGINAL TEST.
--     TBN 09/15/87  COMPLETELY REVISED TEST.

WITH DIRECT_IO;
WITH REPORT; USE REPORT;
PROCEDURE CE2102K IS
     INCOMPLETE : EXCEPTION;
BEGIN
     TEST ("CE2102K", "CHECK THAT USE_ERROR IS RAISED IF AN " &
                      "IMPLEMENTATION DOES NOT SUPPORT RESET FOR " &
                      "DIRECT_IO");
     DECLARE
          PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
          USE DIR;
          FILE1 : FILE_TYPE;
          INT1 : INTEGER := IDENT_INT(1);
          INT2 : INTEGER := 2;
     BEGIN
          BEGIN
               CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE OF " &
                                    "DIRECT FILE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE OF " &
                                    "DIRECT FILE WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE1, INT2);

     -- RESETTING FROM OUT_FILE TO IN_FILE.

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
                                 "READING FROM FILE - 1");
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

     -- RESETTING FROM OUT_FILE TO INOUT_FILE.

          CREATE (FILE1, OUT_FILE, LEGAL_FILE_NAME(2));

          WRITE (FILE1, INT2);
          BEGIN
               RESET (FILE1, INOUT_FILE);
               COMMENT ("RESET FROM OUT_FILE TO INOUT_FILE IS ALLOWED");
               BEGIN
                    READ (FILE1, INT1);
                    IF INT1 /= IDENT_INT(2) THEN
                         FAILED ("RESETTING FROM OUT_FILE TO " &
                                 "INOUT_FILE AFFECTED DATA");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED WHILE " &
                                 "READING FROM FILE - 2");
               END;
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM OUT_FILE TO INOUT_FILE IS " &
                             "NOT ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM OUT_FILE TO INOUT_FILE");
          END;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     -- RESETTING FROM IN_FILE TO OUT_FILE.

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPENING OF " &
                                    "DIRECT FILE WITH IN_FILE MODE");
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

          CLOSE (FILE1);

     -- RESETTING FROM IN_FILE TO INOUT_FILE.

          OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);

          BEGIN
               RESET (FILE1, INOUT_FILE);
               COMMENT ("RESET FROM IN_FILE TO INOUT_FILE IS ALLOWED");
               BEGIN
                    READ (FILE1, INT1);
                    IF INT1 /= IDENT_INT(2) THEN
                         FAILED ("RESETTING FROM IN_FILE TO " &
                                 "INOUT_FILE AFFECTED DATA");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED WHILE " &
                                 "READING FROM FILE - 3");
               END;
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM IN_FILE TO INOUT_FILE IS " &
                             "NOT ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM IN_FILE TO INOUT_FILE");
          END;

          CLOSE (FILE1);

     -- RESETTING FROM INOUT_FILE TO IN_FILE.

          BEGIN
               OPEN (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON OPENING OF " &
                                    "DIRECT FILE WITH INOUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               RESET (FILE1, IN_FILE);
               COMMENT ("RESET FROM INOUT_FILE TO IN_FILE IS ALLOWED");
               BEGIN
                    READ (FILE1, INT1);
                    IF INT1 /= IDENT_INT(2) THEN
                         FAILED ("RESETTING FROM INOUT_FILE TO " &
                                 "IN_FILE AFFECTED DATA");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED WHILE " &
                                 "READING FROM FILE - 2");
               END;
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM INOUT_FILE TO IN_FILE IS " &
                             "NOT ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM INOUT_FILE TO IN_FILE");
          END;

          CLOSE (FILE1);

     -- RESETTING FROM INOUT_FILE TO OUT_FILE.

          OPEN (FILE1, INOUT_FILE, LEGAL_FILE_NAME);

          BEGIN
               RESET (FILE1, OUT_FILE);
               COMMENT ("RESET FROM INOUT_FILE TO OUT_FILE IS ALLOWED");
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT ("RESET FROM INOUT_FILE TO OUT_FILE IS " &
                             "NOT ALLOWED");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED WHEN " &
                            "RESETTING FROM INOUT_FILE TO OUT_FILE");
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
END CE2102K;
