-- CE3704C.ADA

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
--     CHECK THAT INTEGER_IO GET RAISES CONSTRAINT_ERROR IF THE
--     WIDTH PARAMETER IS NEGATIVE, IF THE WIDTH PARAMETER IS
--     GREATER THAN FIELD'LAST WHEN FIELD'LAST IS LESS THAN
--     INTEGER'LAST, OR THE VALUE READ IS OUT OF THE RANGE OF
--     THE ITEM PARAMETER BUT WITHIN THE RANGE OF INSTANTIATED
--     TYPE.

-- HISTORY:
--     SPS 10/04/82
--     DWC 09/09/87  ADDED CASES FOR WIDTH BEING GREATER THAN
--                   FIELD'LAST AND THE VALUE BEING READ IS OUT
--                   OF ITEM'S RANGE BUT WITHIN INSTANTIATED
--                   RANGE.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3704C IS
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE3704C", "CHECK THAT INTEGER_IO GET RAISES " &
                      "CONSTRAINT_ERROR IF THE WIDTH PARAMETER " &
                      "IS NEGATIVE, IF THE WIDTH PARAMETER IS " &
                      "GREATER THAN FIELD'LAST WHEN FIELD'LAST IS " &
                      "LESS THAN INTEGER'LAST, OR THE VALUE READ " &
                      "IS OUT OF THE RANGE OF THE ITEM PARAMETER " &
                      "BUT WITHIN THE RANGE OF INSTANTIATED TYPE");

     DECLARE
          FT : FILE_TYPE;
          TYPE INT IS NEW INTEGER RANGE 1 .. 10;
          PACKAGE IIO IS NEW INTEGER_IO (INT);
          X : INT RANGE 1 .. 5;
          USE IIO;
     BEGIN

          BEGIN
               GET (FT, X, IDENT_INT(-1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - FILE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("RAISED STATUS_ERROR");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FILE");
          END;

          BEGIN
               GET (X, IDENT_INT(-6));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - DEFAULT");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - DEFAULT");
          END;

          BEGIN
               CREATE (FT, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
               WHEN NAME_ERROR =>
                    NOT_APPLICABLE ("NAME_ERROR RAISED; TEXT CREATE " &
                                    "WITH OUT_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          PUT (FT, 1);
          NEW_LINE (FT);
          PUT (FT, 8);
          NEW_LINE (FT);
          PUT (FT, 2);

          CLOSE (FT);

          BEGIN
               OPEN (FT, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("USE_ERROR FOR OPEN " &
                                    "WITH IN_FILE MODE");
                    RAISE INCOMPLETE;
          END;

          BEGIN
               GET (FT, X, IDENT_INT(-1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - " &
                       "NEGATIVE WIDTH WITH EXTERNAL FILE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - " &
                            "NEGATIVE WIDTH WITH EXTERNAL FILE");
          END;

          SKIP_LINE (FT);

          BEGIN
               GET (FT, X);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - " &
                       "OUT OF RANGE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - " &
                            "OUT OF RANGE");
          END;

          SKIP_LINE (FT);

          IF FIELD'LAST < INTEGER'LAST THEN
               BEGIN
                    GET (FT, X, FIELD'LAST + Ident_Int(1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - " &
                            "FIELD'LAST + 1 WIDTH WITH " &
                            "EXTERNAL FILE");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - " &
                                 "FIELD'LAST + 1 WIDTH WITH " &
                                 "EXTERNAL FILE");
               END;
          END IF;

          BEGIN
               DELETE (FT);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;
     END;

     RESULT;
END CE3704C;
