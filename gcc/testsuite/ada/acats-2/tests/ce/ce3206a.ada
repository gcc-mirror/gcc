-- CE3206A.ADA

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
--     CHECK THAT SET_INPUT AND SET_OUTPUT RAISE STATUS_ERROR WHEN
--     CALLED WITH A FILE PARAMETER DENOTING A CLOSED FILE.

-- HISTORY:
--     ABW 08/31/82
--     SPS 10/01/82
--     SPS 11/09/82
--     JLH 08/18/87  ADDED NEW CASES FOR SET_INPUT AND SET_OUTPUT.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3206A IS

     FILE_IN, FILE1 : FILE_TYPE;
     ITEM : CHARACTER := 'A';

BEGIN

     TEST ("CE3206A", "CHECK THAT SET_INPUT AND SET_OUTPUT " &
                      "RAISE STATUS_ERROR WHEN CALLED WITH A " &
                      "FILE PARAMETER DENOTING A CLOSED FILE");

     BEGIN
          SET_INPUT (FILE_IN);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_INPUT - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR SET_INPUT - 1");
     END;

     BEGIN
          SET_OUTPUT (FILE_IN);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_OUTPUT - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR SET_OUTPUT - 1");
     END;

     BEGIN
          CREATE (FILE1, OUT_FILE);
          PUT (FILE1, ITEM);
          CLOSE (FILE1);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     BEGIN
          SET_INPUT (FILE1);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_INPUT - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR SET_INPUT - 2");
     END;

     BEGIN
          SET_OUTPUT (FILE1);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_OUTPUT - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR SET_OUTPUT - 2");
     END;


     RESULT;

END CE3206A;
