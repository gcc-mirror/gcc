-- AE3101A.ADA

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
--     CHECK THAT CREATE, OPEN, CLOSE, DELETE, RESET, MODE, NAME,
--     FORM, IS_OPEN, AND END_OF_FILE ARE AVAILABLE FOR TEXT FILES.
--     ALSO CHECK THAT FORMAL PARAMETER NAMES ARE CORRECT.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     DWC 09/24/87  REMOVED DEPENDENCE ON FILE SUPPORT.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE AE3101A IS

     FILE1 : FILE_TYPE;

BEGIN

     TEST ("AE3101A" , "CHECK THAT CREATE, OPEN, DELETE, " &
                       "RESET, MODE, NAME, FORM, IS_OPEN, " &
                       "AND END_OF_FILE ARE AVAILABLE " &
                       "FOR TEXT FILE");

     BEGIN
          CREATE (FILE => FILE1,
                  MODE => OUT_FILE,
                  NAME => LEGAL_FILE_NAME,
                  FORM => "");
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RESET (FILE => FILE1, MODE => IN_FILE);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          CLOSE (FILE => FILE1);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          OPEN (FILE => FILE1,
                MODE => IN_FILE,
                NAME => LEGAL_FILE_NAME,
                FORM => "");
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     IF IS_OPEN (FILE => FILE1) THEN
          NULL;
     END IF;

     BEGIN
          IF MODE (FILE => FILE1) /= IN_FILE THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF NAME (FILE => FILE1) /= LEGAL_FILE_NAME THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF FORM (FILE => FILE1) /= "" THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF END_OF_FILE (FILE => FILE1) THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          DELETE (FILE => FILE1);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     RESULT;

END AE3101A;
