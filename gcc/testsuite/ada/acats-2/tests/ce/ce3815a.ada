-- CE3815A.ADA

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
--     CHECK THAT THE OPERATIONS IN GENERIC PACKAGE FLOAT_IO ALL HAVE
--     THE CORRECT PARAMETER NAMES.

-- HISTORY:
--     JET 10/28/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;
PROCEDURE CE3815A IS

     STR : STRING(1..20) := (OTHERS => ' ');
     FIN, FOUT : FILE_TYPE;
     F : FLOAT;
     L : POSITIVE;
     FILE_OK : BOOLEAN := FALSE;

     PACKAGE FIO IS NEW FLOAT_IO(FLOAT);
     USE FIO;

BEGIN
     TEST ("CE3815A", "CHECK THAT THE OPERATIONS IN GENERIC PACKAGE " &
                      "FLOAT_IO ALL HAVE THE CORRECT PARAMETER NAMES");

     PUT (TO => STR, ITEM => 1.0, AFT => 3, EXP => 3);
     GET (FROM => STR, ITEM => F, LAST => L);

     BEGIN
          CREATE(FOUT, OUT_FILE, LEGAL_FILE_NAME);
          FILE_OK := TRUE;
     EXCEPTION
          WHEN OTHERS =>
               COMMENT("OUTPUT FILE COULD NOT BE CREATED");
     END;

     IF FILE_OK THEN
          BEGIN
               PUT (FILE => FOUT, ITEM => 1.0, FORE => 3, AFT => 3,
                    EXP => 3);
               NEW_LINE(FOUT);

               CLOSE(FOUT);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("OUTPUT FILE COULD NOT BE WRITTEN");
                    FILE_OK := FALSE;
          END;
     END IF;

     IF FILE_OK THEN
          BEGIN
               OPEN(FIN, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("INPUT FILE COULD NOT BE OPENED");
                    FILE_OK := FALSE;
          END;
     END IF;

     IF FILE_OK THEN
          BEGIN
               GET (FILE => FIN, ITEM => F, WIDTH => 10);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("DATA COULD NOT BE READ FROM FILE");
          END;

          BEGIN
               DELETE(FIN);
          EXCEPTION
               WHEN USE_ERROR =>
                    COMMENT("FILE COULD NOT BE DELETED");
               WHEN OTHERS =>
                    FAILED("UNEXPECTED ERROR AT DELETION");
          END;
     END IF;

     RESULT;
END CE3815A;
