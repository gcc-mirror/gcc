-- CE3407B.ADA

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
--     CHECK THAT END_OF_PAGE CAN ONLY BE APPLIED TO FILES OF MODE
--     IN_FILE, THAT MODE_ERROR IS RAISED FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/22/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/28/87  CORRECTED EXCEPTION HANDLING AND ADDED CASE
--                   FOR CURRENT_OUTPUT.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3407B IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     BOOL : BOOLEAN;

BEGIN

     TEST ("CE3407B", "CHECK THAT END_OF_PAGE RAISES MODE_ERROR " &
                      "FOR FILES OF MODE OUT_FILE");

     BEGIN
          CREATE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE FOR " &
                               "TEMPORARY FILE WITH OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          BOOL := END_OF_PAGE (FILE);
          FAILED ("MODE_ERROR NOT RAISED FOR OUT_FILE");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR OUT_FILE");
     END;

     BEGIN
          BOOL := END_OF_PAGE (STANDARD_OUTPUT);
          FAILED ("MODE_ERROR NOT RAISED FOR STANDARD_OUTPUT");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR STANDARD_OUTPUT");
     END;

     BEGIN
          BOOL := END_OF_PAGE (CURRENT_OUTPUT);
          FAILED ("MODE_ERROR NOT RAISED FOR CURRENT_OUTPUT");
     EXCEPTION
          WHEN MODE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR CURRENT_OUTPUT");
     END;

     CLOSE (FILE);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3407B;
