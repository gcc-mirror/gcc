-- CE2102J.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN CREATING A FILE OF MODE
--     OUT_FILE, WHEN OUT_FILE MODE IS NOT SUPPORTED FOR CREATE BY
--     THE IMPLEMENTATION FOR DIRECT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT OUT_FILE FOR CREATE FOR DIRECT FILES.

-- HISTORY:
--     SPS 08/26/82
--     JBG 06/04/84
--     EG  05/08/85
--     TBN 07/23/87  COMPLETELY REVISED TEST.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2102J IS
BEGIN

     TEST ("CE2102J", "CHECK THAT USE_ERROR IS RAISED WHEN MODE " &
                      "OUT_FILE IS NOT SUPPORTED FOR THE OPERATION " &
                      "OF CREATE FOR DIRECT_IO");
     DECLARE
          PACKAGE DIR IS NEW DIRECT_IO (BOOLEAN);
          USE DIR;
          FILE1 : FILE_TYPE;
     BEGIN
          CREATE (FILE1, OUT_FILE);
          CLOSE (FILE1);
          NOT_APPLICABLE ("CREATE WITH MODE OUT_FILE ALLOWED");
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON CREATE");
     END;

     RESULT;

END CE2102J;
