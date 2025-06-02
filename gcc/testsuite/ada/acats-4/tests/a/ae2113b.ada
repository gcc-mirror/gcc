-- AE2113B.ADA

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
-- CHECK THAT THE SUBPROGRAMS CREATE, OPEN, CLOSE, DELETE, RESET, MODE,
-- NAME, FORM, AND IS_OPEN ARE AVAILABLE FOR SEQUENTIAL_IO AND THAT
-- SUBPROGRAMS HAVE THE CORRECT FORMAL PARAMETER NAMES.

-- TBN  9/30/86

WITH SEQUENTIAL_IO;
WITH REPORT; USE REPORT;
PROCEDURE AE2113B IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO (INTEGER);
     USE SEQ_IO;

     TEMP : FILE_TYPE;

BEGIN
     TEST ("AE2113B", "CHECK THAT THE SUBPROGRAMS CREATE, OPEN, " &
                      "CLOSE, DELETE, RESET, MODE, NAME, FORM, AND " &
                      "IS_OPEN ARE AVAILABLE FOR SEQUENTIAL_IO AND " &
                      "THAT SUBPROGRAMS HAVE THE CORRECT FORMAL " &
                      "PARAMETER NAMES");
     BEGIN
          CREATE (FILE=> TEMP, MODE=> OUT_FILE,
                  NAME=> "AE2113B.DAT", FORM=> "");
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RESET (FILE=> TEMP, MODE=> OUT_FILE);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          CLOSE (FILE=> TEMP);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          OPEN (FILE=> TEMP, MODE=> OUT_FILE,
                  NAME=> "AE2113B.DAT", FORM=> "");
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF IS_OPEN (FILE=> TEMP) THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF MODE (FILE=> TEMP) /= OUT_FILE THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF NAME (FILE=> TEMP) /= "AE2113B.DAT" THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          IF FORM (FILE=> TEMP) /= "" THEN
               NULL;
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          DELETE (FILE=> TEMP);
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     RESULT;
END AE2113B;
