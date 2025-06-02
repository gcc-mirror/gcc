-- CE3410A.ADA

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
--     CHECK THAT SET_LINE RAISES LAYOUT_ERROR IF THE PAGE LENGTH IS
--     BOUNDED AND THE GIVEN LINE POSITION EXCEEDS THE PAGE LENGTH
--     FOR FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     ABW 08/26/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  CORRECTED EXCEPTION HANDLING.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3410A IS

     INCOMPLETE : EXCEPTION;
     FILE  : FILE_TYPE;
     THREE : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(3));
     FOUR  : POSITIVE_COUNT := POSITIVE_COUNT (IDENT_INT(4));

BEGIN

     TEST ("CE3410A", "CHECK THAT SET_LINE RAISES " &
                      "LAYOUT_ERROR APPROPRIATELY");

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

     SET_PAGE_LENGTH (FILE, THREE);

     BEGIN
          SET_LINE (FILE, FOUR);
          FAILED ("LAYOUT ERROR NOT RAISED FOR SET_LINE");
     EXCEPTION
          WHEN LAYOUT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED FOR SET_LINE");
     END;

     CLOSE (FILE);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3410A;
