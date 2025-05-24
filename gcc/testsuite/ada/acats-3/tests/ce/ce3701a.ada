-- CE3701A.ADA

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
--     CHECK THAT GET AND PUT OF INTEGER_IO RAISE STATUS_ERROR IF
--     THE FILE IS NOT OPEN.

-- HISTORY:
--     ABW 08/27/82
--     JBG 08/30/83
--     DWC 09/09/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND ATTEMPTED TO CREATE A FILE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3701A IS

     PACKAGE INT_IO IS NEW INTEGER_IO (INTEGER);
     USE INT_IO;
     FILE : FILE_TYPE;
     INT_ITEM : INTEGER := 7;

BEGIN

     TEST ("CE3701A", "CHECK THAT GET AND PUT RAISE " &
                       "STATUS_ERROR IF THE FILE " &
                       "IS NOT OPEN");

     BEGIN
          PUT (FILE, IDENT_INT(8));
          FAILED ("STATUS_ERROR NOT RAISED WHEN PUT APPLIED " &
                  "TO A NON-EXISTENT FILE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED WHEN PUT " &
                       "APPLIED TO A NON-EXISTENT FILE");
     END;

     BEGIN
          GET (FILE, INT_ITEM);
          FAILED ("STATUS_ERROR NOT RAISED WHEN GET APPLIED " &
                  "TO A NON-EXISTENT FILE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED WHEN GET " &
                       "APPLIED TO A NON-EXISTENT FILE");
     END;

     BEGIN
          CREATE (FILE);     -- THIS IS JUST AN ATTEMPT TO CREATE
          CLOSE (FILE);      -- A FILE.  WHETHER THIS IS SUCCESSFUL
     EXCEPTION               -- OR NOT HAS NO EFFECT ON TEST
          WHEN USE_ERROR =>  -- OBJECTIVE.
               NULL;
     END;

     BEGIN
          PUT (FILE, IDENT_INT(8));
          FAILED ("STATUS_ERROR NOT RAISED WHEN PUT APPLIED " &
                  "TO AN UNOPENED FILE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED WHEN PUT " &
                       "APPLIED TO AN UNOPENED FILE");
     END;

     BEGIN
          GET (FILE, INT_ITEM);
          FAILED ("STATUS_ERROR NOT RAISED WHEN GET APPLIED " &
                  "TO AN UNOPENED FILE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED WHEN GET " &
                       "APPLIED TO AN UNOPENED FILE");
     END;

     RESULT;

END CE3701A;
