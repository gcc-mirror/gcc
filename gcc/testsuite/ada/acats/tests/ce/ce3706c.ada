-- CE3706C.ADA

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
--     CHECK THAT INTEGER_IO PUT RAISES CONSTRAINT_ERROR IF:
--        A) THE BASE IS OUTSIDE THE RANGE 2..16.
--        B) THE VALUE OF WIDTH IS NEGATIVE OR GREATER THAN FIELD'LAST,
--           WHEN FIELD'LAST < INTEGER'LAST.
--        C) THE VALUE OF ITEM IS OUTSIDE THE RANGE OF THE INSTANTIATED
--           TYPE.

-- HISTORY:
--     SPS 10/05/82
--     JBG 08/30/83
--     JLH 09/10/87  ADDED CASES FOR THE VALUE OF THE WIDTH BEING LESS
--                   THAN ZERO AND GREATER THAN FIELD'LAST AND CASES FOR
--                   THE VALUE OF ITEM OUTSIDE THE RANGE OF THE
--                   INSTANTIATED TYPE.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3706C IS
BEGIN

     TEST ("CE3706C", "CHECK THAT INTEGER_IO PUT RAISES CONSTRAINT " &
                      "ERROR APPROPRIATELY");

     DECLARE
          FT : FILE_TYPE;
          TYPE INT IS NEW INTEGER RANGE 1 .. 10;
          PACKAGE IIO IS NEW INTEGER_IO (INT);
          USE IIO;
          ST : STRING (1 .. 10);
     BEGIN

          BEGIN
               PUT (FT, 2, 6, 1);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - FILE - 1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FILE - 1");
          END;

          BEGIN
               PUT (3, 4, 17);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - DEFAULT - 1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - DEFAULT - 1");
          END;

          BEGIN
               PUT (TO => ST, ITEM => 4, BASE => -3);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - STRING - 1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - STRING - 1");
          END;

          BEGIN
               PUT (ST, 5, 17);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - STRING - 2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - STRING - 2");
          END;

          BEGIN
               PUT (FT, 5, -1);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - FILE - 2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FILE - 2");
          END;

          BEGIN
               PUT (7, -3);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - DEFAULT - " &
                       "2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - DEFAULT - 2");
          END;

          IF FIELD'LAST < INTEGER'LAST THEN
               BEGIN
                    PUT (7, FIELD'LAST+Ident_Int(1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED FOR WIDTH " &
                            "GREATER THAN FIELD'LAST");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED FOR WIDTH " &
                                 "GREATER THAN FIELD'LAST");
               END;

          END IF;

          BEGIN
               PUT (FT, 11);
               FAILED ("CONSTRAINT_ERROR NOT RAISED FOR ITEM OUTSIDE " &
                       "RANGE - FILE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR ITEM OUTSIDE " &
                            "RANGE - FILE");
          END;

          BEGIN
               PUT (11);
               FAILED ("CONSTRAINT_ERROR NOT RAISED FOR ITEM OUTSIDE " &
                       "RANGE - DEFAULT");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR ITEM OUTSIDE " &
                            "RANGE - DEFAULT");
          END;

     END;

     RESULT;
END CE3706C;
