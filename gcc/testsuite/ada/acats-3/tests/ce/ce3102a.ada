-- CE3102A.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED BY CREATE AND OPEN
--     IF THE GIVEN TEXT FILES ARE ALREADY OPEN.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH MODE OUT_FILE FOR TEXT FILES.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     JBG 07/25/83
--     JLH 08/07/87  COMPLETE REVISION OF TEST.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3102A IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;

BEGIN

     TEST ("CE3102A" , "CHECK THAT STATUS_ERROR IS RAISED " &
                       "APPROPRIATELY FOR TEXT FILES");

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON CREATE WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          CREATE (FILE, OUT_FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR CREATE - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 1");
     END;

     BEGIN
          CREATE (FILE, IN_FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR CREATE - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 2");
     END;

     BEGIN
          CREATE (FILE, OUT_FILE, LEGAL_FILE_NAME);
          FAILED ("STATUS_ERROR NOT RAISED FOR CREATE - 3");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR CREATE - 3");
     END;

     BEGIN
          OPEN (FILE, OUT_FILE, LEGAL_FILE_NAME);
          FAILED ("STATUS_ERROR NOT RAISED FOR OPEN - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 1");
     END;

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME);
          FAILED ("STATUS_ERROR NOT RAISED FOR OPEN - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 2");
     END;

     BEGIN
          OPEN (FILE, IN_FILE, LEGAL_FILE_NAME (2, "CE3102A"));
          FAILED ("STATUS_ERROR NOT RAISED FOR OPEN - 3");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 3");
     END;

     BEGIN
          CREATE (FILE, IN_FILE, LEGAL_FILE_NAME (2, "CE3102A"));
          FAILED ("STATUS_ERROR NOT RAISED FOR OPEN - 4");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR OPEN - 4");
     END;

     BEGIN
          DELETE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3102A;
