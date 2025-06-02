-- CE3601A.ADA

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
--     CHECK THAT GET (FOR STRINGS AND CHARACTERS), PUT (FOR STRINGS AND
--     CHARACTERS), GET_LINE, AND PUT_LINE RAISE STATUS_ERROR WHEN
--     CALLED WITH AN UNOPEN FILE PARAMETER.  ALSO CHECK NAMES OF FORMAL
--     PARAMETERS.

-- HISTORY:
--     SPS 08/27/82
--     VKG 02/15/83
--     JBG 03/30/83
--     JLH 09/04/87  ADDED CASE WHICH ATTEMPTS TO CREATE FILE AND THEN
--                   RETESTED OBJECTIVE.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3601A IS

BEGIN

     TEST ("CE3601A", "STATUS_ERROR RAISED BY GET, PUT, GET_LINE, " &
                      "PUT_LINE WHEN FILE IS NOT OPEN");

     DECLARE
          FILE1, FILE2 : FILE_TYPE;
          CH: CHARACTER := '%';
          LST: NATURAL;
          ST: STRING (1 .. 10);
          LN : STRING (1 .. 80);
     BEGIN
          BEGIN
               GET (FILE => FILE1, ITEM => CH);
               FAILED ("STATUS_ERROR NOT RAISED - GET CHARACTER");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHARACTER");
          END;

          BEGIN
               GET (FILE => FILE1, ITEM => ST);
               FAILED ("STATUS_ERROR NOT RAISED - GET STRING");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING");
          END;

          BEGIN
               GET_LINE (FILE => FILE1, ITEM => LN, LAST => LST);
               FAILED ("STATUS_ERROR NOT RAISED - GET_LINE");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET_LINE");
          END;

          BEGIN
               PUT (FILE => FILE1, ITEM => CH);
               FAILED ("STATUS_ERROR NOT RAISED - PUT CHARACTER");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT CHARACTER");
          END;

          BEGIN
               PUT (FILE => FILE1, ITEM => ST);
               FAILED ("STATUS_ERROR NOT RAISED - PUT STRING");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT STRING");
          END;

          BEGIN
               PUT_LINE (FILE => FILE1, ITEM => LN);
               FAILED ("STATUS_ERROR NOT RAISED - PUT_LINE");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT_LINE");
          END;

          BEGIN
               CREATE (FILE2, OUT_FILE);   -- THIS IS ONLY AN ATTEMPT TO
               CLOSE (FILE2);              -- CREATE A FILE. OK, WHETHER
          EXCEPTION                        -- SUCCESSFUL OR NOT.
               WHEN USE_ERROR =>
                    NULL;
          END;

          BEGIN
               GET (FILE => FILE2, ITEM => CH);
               FAILED ("STATUS_ERROR NOT RAISED - GET CHARACTER");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET CHARACTER");
          END;

          BEGIN
               GET (FILE => FILE2, ITEM => ST);
               FAILED ("STATUS_ERROR NOT RAISED - GET STRING");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET STRING");
          END;

          BEGIN
               GET_LINE (FILE => FILE2, ITEM => LN, LAST => LST);
               FAILED ("STATUS_ERROR NOT RAISED - GET_LINE");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - GET_LINE");
          END;

          BEGIN
               PUT (FILE => FILE2, ITEM => CH);
               FAILED ("STATUS_ERROR NOT RAISED - PUT CHARACTER");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT CHARACTER");
          END;

          BEGIN
               PUT (FILE => FILE2, ITEM => ST);
               FAILED ("STATUS_ERROR NOT RAISED - PUT STRING");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT STRING");
          END;

          BEGIN
               PUT_LINE (FILE => FILE2, ITEM => LN);
               FAILED ("STATUS_ERROR NOT RAISED - PUT_LINE");
          EXCEPTION
               WHEN STATUS_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - PUT_LINE");
          END;

     END;

     RESULT;

END CE3601A;
