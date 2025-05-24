-- CE2109B.ADA

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
--     CHECK THAT THE DEFAULT MODES IN CREATE ARE SET CORRECTLY FOR
--     DIRECT_IO.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     TBN 02/13/86
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/12/87  CHANGED NOT_APPLICABLE MESSAGE, REMOVED
--                   NAME_ERROR, AND CLOSED THE FILE.
--     LDC 05/26/88  CHANGED APPLICABILITY COMMENT FROM OUT_FILE TO
--                   INOUT_FILE.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2109B IS

     INCOMPLETE : EXCEPTION;
     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE3 : DIR.FILE_TYPE;

BEGIN

     TEST( "CE2109B", "CHECK DEFAULT MODE IN CREATE FOR DIRECT_IO");

     BEGIN
          CREATE (FILE3);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("CREATE OF DIRECT FILE WITH " &
                               "INOUT_FILE MODE NOT SUPPORTED");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED; DIRECT CREATE");
               RAISE INCOMPLETE;
     END;

     IF MODE (FILE3) /= INOUT_FILE THEN
          FAILED( "MODE INCORRECTLY SET FOR DIRECT_IO" );
     END IF;

     CLOSE (FILE3);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2109B;
