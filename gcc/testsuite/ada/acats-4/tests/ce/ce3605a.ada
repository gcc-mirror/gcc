-- CE3605A.ADA

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
--     CHECK THAT PUT FOR CHARACTER AND STRING PARAMETERS DOES NOT
--     UPDATE THE LINE NUMBER WHEN THE LINE LENGTH IS UNBOUNDED,
--     ONLY THE COLUMN NUMBER.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- HISTORY:
--     SPS 09/02/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  CORRECTED EXCEPTION HANDLING AND ADDED CHECKS
--                   FOR COLUMN NUMBER.
--     RJW 03/28/90  REVISED NUMERIC LITERALS USED IN LOOPS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3605A IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3605A", "CHECK THAT PUT FOR CHARACTER AND STRING " &
                      "PARAMETERS DOES NOT UPDATE THE LINE NUMBER " &
                      "WHEN THE LINE LENGTH IS UNBOUNDED, ONLY THE " &
                      "COLUMN NUMBER");

     DECLARE
          FILE1 : FILE_TYPE;
          LN : POSITIVE_COUNT := 1;
     BEGIN

          BEGIN
               CREATE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                                    "FOR TEMPORARY FILES WITH " &
                                    "OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          LN := LINE (FILE1);

          IF LN /= 1 THEN
               FAILED ("CURRENT LINE NUMBER NOT INITIALLY ONE");
          END IF;

          IF COL (FILE1) /= 1 THEN
               FAILED ("CURRENT COLUMN NUMBER NOT INITIALLY ONE");
          END IF;

          FOR I IN 1 .. IDENT_INT(240) LOOP
               PUT(FILE1, 'A');
          END LOOP;
          IF LINE (FILE1) /= LN THEN
               FAILED ("PUT ALTERED LINE NUMBER - CHARACTER");
          END IF;

          IF COL(FILE1) /= 241 THEN
               FAILED ("COLUMN NUMBER NOT UPDATED CORRECTLY - 1");
          END IF;

          NEW_LINE(FILE1);
          LN := LINE (FILE1);

          FOR I IN 1 .. IDENT_INT(40) LOOP
               PUT (FILE1, "STRING");
          END LOOP;
          IF LN /= LINE (FILE1) THEN
               FAILED ("PUT ALTERED LINE NUMBER - STRING");
          END IF;

          IF COL(FILE1) /= 241 THEN
               FAILED ("COLUMN NUMBER NOT UPDATED CORRECTLY - 2");
          END IF;

          CLOSE (FILE1);

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE3605A;
