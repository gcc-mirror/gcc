-- CE3102D.ADA

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
--     CHECK THAT STATUS_ERROR IS RAISED BY CLOSE, DELETE, RESET, MODE,
--     NAME, AND FORM IF THE GIVEN TEXT FILES ARE NOT OPEN.

-- HISTORY:
--     JLH 08/10/87  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3102D IS

     INCOMPLETE : EXCEPTION;
     FILE : FILE_TYPE;
     FT : FILE_TYPE;

BEGIN

     TEST ("CE3102D" , "CHECK THAT STATUS_ERROR IS RAISED " &
                       "APPROPRIATELY FOR TEXT FILES");

     BEGIN
          CREATE (FT);
          CLOSE (FT);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR CREATE");
     END;

     BEGIN
          RESET (FT);
          FAILED ("STATUS_ERROR NOT RAISED FOR RESET");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR RESET OF CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR RESET");
     END;

     BEGIN
          DECLARE
               MD : FILE_MODE := MODE (FT);
          BEGIN
               FAILED ("STATUS_ERROR NOT RAISED FOR MODE");
          END;
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR MODE OF CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR MODE");
     END;

     BEGIN
          DECLARE
               NM : CONSTANT STRING := NAME (FT);
          BEGIN
               FAILED ("STATUS_ERROR NOT RAISED FOR NAME");
          END;
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR NAME OF CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR NAME");
     END;

     BEGIN
          DECLARE
               FM : CONSTANT STRING := FORM (FT);
          BEGIN
               FAILED ("STATUS_ERROR NOT RAISED FOR FORM");
          END;
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR FORM OF CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR FORM");
     END;

     BEGIN
          CLOSE (FT);
          FAILED ("STATUS_ERROR NOT RAISED FOR CLOSE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED WHEN CLOSING CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR CLOSE");
     END;

     BEGIN
          DELETE (FT);
          FAILED ("STATUS_ERROR NOT RAISED FOR DELETE");
     EXCEPTION
          WHEN STATUS_ERROR =>
               NULL;
          WHEN USE_ERROR =>
               FAILED ("USE_ERROR RAISED FOR DELETE OF CLOSED FILE");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR DELETE");
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE3102D;
