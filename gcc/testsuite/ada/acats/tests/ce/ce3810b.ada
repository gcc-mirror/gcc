-- CE3810B.ADA

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
--     CHECK THAT FIXED_IO PUT CAN OPERATE ON STRINGS.  ALSO CHECK THAT
--     LAYOUT_ERROR IS RAISED WHEN THE STRING IS INSUFFICIENTLY LONG.

-- HISTORY:
--     DWC 09/15/87  CREATE ORIGINAL TEST.
--     JRL 02/28/96  Changed upper bound of type FX from 1000.0 to 250.0.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3810B IS
BEGIN

     TEST ("CE3810B", "CHECK THAT FIXED_IO PUT CAN OPERATE ON " &
                      "STRINGS.  ALSO CHECK THAT LAYOUT_ERROR IS " &
                      "RAISED WHEN THE STRING IS INSUFFICIENTLY LONG");

     DECLARE
          TYPE FX IS DELTA 0.0001 RANGE 0.0 .. 250.0;
          PACKAGE FXIO IS NEW FIXED_IO (FX);
          USE FXIO;
          ST1 : CONSTANT STRING  := "  234.5000";
          ST : STRING (ST1'RANGE);
          ST2 : STRING (1 .. 2);

     BEGIN
          BEGIN
               PUT (ST, 234.5);
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    FAILED ("LAYOUT_ERROR RAISED ON PUT" &
                            "TO STRING - FIXED");
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED ON PUT" &
                            "TO STRING -FIXED");
          END;

          IF ST /= ST1 THEN
               FAILED ("PUT FIXED TO STRING INCORRECT; OUTPUT " &
                       "WAS """ & ST & """");
          END IF;

          BEGIN
               PUT (ST (1..7), 234.5000);
               FAILED ("LAYOUT_ERROR NOT RAISED - FIXED - 1");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FIXED - 1");
          END;

          BEGIN
               PUT (ST, 2.3, 9, 0);
               FAILED ("LAYOUT_ERROR NOT RAISED - FIXED - 2");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FIXED - 2");
          END;

          BEGIN
               PUT (ST2, 2.0, 0, 0);
               FAILED ("LAYOUT_ERROR NOT RAISED - FIXED - 3");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FIXED - 3");
          END;

          BEGIN
               PUT (ST, 2.345, 6, 2);
               FAILED ("LAYOUT_ERROR NOT RAISED - FIXED - 4");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FIXED - 4");
          END;

          BEGIN
               PUT (ST, 2.0, 0, 7);
               FAILED ("LAYOUT_ERROR NOT RAISED - FIXED - 5");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FIXED - 5");
          END;
     END;

     RESULT;
END CE3810B;
