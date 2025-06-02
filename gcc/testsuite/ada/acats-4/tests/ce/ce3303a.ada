-- CE3303A.ADA

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
--     CHECK THAT SET_LINE_LENGTH, SET_PAGE_LENGTH, LINE_LENGTH, AND
--     PAGE_LENGTH RAISE STATUS_ERROR WHEN APPLIED TO A CLOSED FILE.

-- HISTORY:
--     ABW 08/26/82
--     SPS 09/16/82
--     JLH 08/19/87  ADDED AN ATTEMPT TO CREATE AN EXTERNAL FILE;
--                   ADDED CHECKS TO THE SAME FOUR CASES WHICH EXIST
--                   IN TEST AGAINST ATTEMPTED CREATE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3303A IS

     FILE : FILE_TYPE;
     FIVE : COUNT := COUNT(IDENT_INT(5));
     C : COUNT;
     ITEM : CHARACTER := 'A';

BEGIN

     TEST ("CE3303A" , "CHECK THAT SET_LINE_LENGTH, " &
                       "SET_PAGE_LENGTH, LINE_LENGTH, AND " &
                       "PAGE_LENGTH RAISE STATUS_ERROR " &
                       "WHEN APPLIED TO A CLOSED FILE");

-- FILE NONEXISTANT

     BEGIN
          SET_LINE_LENGTH (FILE, FIVE);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_LINE_LENGTH - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR SET_LINE_LENGTH " &
                       "- 1");
     END;

     BEGIN
          SET_PAGE_LENGTH (FILE, FIVE);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_PAGE_LENGTH - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR SET_PAGE_LENGTH " &
                       "- 1");
     END;

     BEGIN
          C := LINE_LENGTH (FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR LINE_LENGTH - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR LINE_LENGTH - 1");
     END;

     BEGIN
          C := PAGE_LENGTH (FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR PAGE_LENGTH - 1");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR PAGE_LENGTH - 1");
     END;

     BEGIN
          CREATE (FILE, OUT_FILE);
          PUT (FILE, ITEM);
          CLOSE (FILE);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     BEGIN
          SET_LINE_LENGTH (FILE, FIVE);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_LINE_LENGTH - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR SET_LINE_LENGTH " &
                       "- 2");
     END;

     BEGIN
          SET_PAGE_LENGTH (FILE, FIVE);
          FAILED ("STATUS_ERROR NOT RAISED FOR SET_PAGE_LENGTH - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR SET_PAGE_LENGTH " &
                       "- 2");
     END;

     BEGIN
          C := LINE_LENGTH (FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR LINE_LENGTH - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR LINE_LENGTH - 2");
     END;

     BEGIN
          C := PAGE_LENGTH (FILE);
          FAILED ("STATUS_ERROR NOT RAISED FOR PAGE_LENGTH - 2");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR PAGE_LENGTH - 2");
     END;

     RESULT;

END CE3303A;
