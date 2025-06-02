-- CE2102L.ADA

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
--     CHECK TO SEE THAT STATUS_ERROR IS RAISED WHEN PERFORMING ILLEGAL
--     OPERATIONS ON OPENED OR UNOPENED FILES OF TYPE SEQUENTIAL_IO.

--          B) UNOPENED FILES

-- HISTORY:
--     SPW 07/29/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2102L IS

     PACKAGE SEQ_IO IS NEW SEQUENTIAL_IO(INTEGER);
     USE SEQ_IO;

     TEST_FILE_ONE : SEQ_IO.FILE_TYPE;
     STR : STRING (1 .. 10);
     FL_MODE : SEQ_IO.FILE_MODE ;

BEGIN

     TEST ("CE2102L", "CHECK THAT STATUS_ERROR IS RAISED WHEN " &
                      "PERFORMING ILLEGAL OPERATIONS ON UNOPENED " &
                      "FILES OF TYPE SEQUENTIAL_IO");

-- CHECK TO SEE THAT PROPER EXCEPTIONS ARE RAISED WHEN
-- PERFORMING OPERATIONS ON AN UNOPENED FILE

-- CLOSE AN UNOPENED FILE

     BEGIN
          CLOSE (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN AN UNOPENED " &
                  "FILE IS USED IN A CLOSE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON CLOSE");
     END;

-- DELETE AN UNOPENED FILE

     BEGIN
          DELETE (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN AN UNOPENED " &
                  "FILE IS USED IN A DELETE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON DELETE");
     END;

-- RESET UNOPENED FILE

     BEGIN
          RESET (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN AN UNOPENED " &
                  "FILE IS USED IN A RESET");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON RESET");
     END;

     BEGIN
          RESET (TEST_FILE_ONE, IN_FILE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN A UNOPENED FILE " &
                  "IS USED IN A RESET WITH MODE PARAMETER");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON RESET " &
                       "WITH MODE");
     END;

-- ATTEMPT TO DETERMINE MODE OF UNOPENED FILE

     BEGIN
          FL_MODE := MODE (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN A UNOPENED " &
                  "FILE IS USED IN A MODE OPERATION");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON MODE");
     END;

-- ATTEMPT TO DETERMINE NAME OF UNOPENED FILE

     BEGIN
          STR := NAME (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN A UNOPENED " &
                  "FILE IS USED IN A NAME OPERATION");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON NAME");
     END;

--ATTEMPT TO DETERMINE FORM OF UNOPENED FILE

     BEGIN
          STR := FORM (TEST_FILE_ONE);
          FAILED ("STATUS_ERROR NOT RAISED WHEN AN UNOPENED " &
                  "FILE IS USED IN A FORM OPERATION");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED ON FORM");
     END;

     RESULT;

END CE2102L;
