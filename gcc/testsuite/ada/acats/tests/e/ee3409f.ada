-- EE3409F.ADA

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
--     CHECK THAT THE FILE PARAMETER FOR SET_COL IS OPTIONAL, AND
--     THAT THE FUNCTION IS THEN APPLIED TO THE CURRENT DEFAULT
--     OUTPUT FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATION OF TEMPORARY TEXT FILES WITH OUT_FILE MODE.

-- PASS/FAIL CRITERIA:
--     THIS TEST IS PASSED IF IT EXECUTES, PRINTS TENTATIVELY PASSED,
--     AND THE CONTENTS OF THE STANDARD OUTPUT FILE ARE CORRECT.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/20/82
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/31/87  CORRECTED EXCEPTION HANDLING, CHECKED FOR
--                   USE_ERROR ON DELETE, AND RENAMED FROM
--                   CE3409F.ADA.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE EE3409F IS

     INCOMPLETE : EXCEPTION;
     FILE_OUT : FILE_TYPE;
     TWO   : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(2));
     THREE : POSITIVE_COUNT := POSITIVE_COUNT(IDENT_INT(3));

BEGIN

     TEST ("EE3409F", "CHECK DEFAULT FILE FOR SET_COL");

     BEGIN
          CREATE (FILE_OUT);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE " &
                               "FOR TEMPORARY FILES WITH " &
                               "OUT_FILE MODE");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     SPECIAL_ACTION ("THE NEXT LINE SHOULD BEGIN IN COLUMN TWO");

     SET_COL (TWO);
     PUT ("SHOULD BEGIN IN COLUMN TWO");

     IF COL (STANDARD_OUTPUT) /= 28 THEN
          FAILED ("SET_COL DOES NOT OPERATE ON THE DEFAULT " &
                  "STANDARD_OUTPUT");
     END IF;

     NEW_LINE;

     SET_OUTPUT (FILE_OUT);
     SET_COL (THREE);
     IF COL (CURRENT_OUTPUT) /= THREE THEN
          FAILED ("SET_COL DOES NOT OPERATE ON THE DEFAULT " &
                  "CURRENT_OUTPUT");
     END IF;

     CLOSE (FILE_OUT);

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END EE3409F;
