-- CE3906D.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED BY PUT FOR ENUMERATION
--     TYPES WHEN THE VALUE OF WIDTH IS NEGATIVE, WHEN WIDTH IS
--     GREATER THAN FIELD'LAST, OR WHEN THE VALUE OF ITEM IS OUTSIDE
--     THE RANGE OF THE SUBTYPE USED TO INSTANTIATE ENUMERATION_IO.

-- HISTORY:
--     SPS 10/08/82
--     DWC 09/17/87  ADDED CASES FOR CONSTRAINT_ERROR.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3906D IS
BEGIN

     TEST ("CE3906D", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY PUT " &
                      "FOR ENUMERATION TYPES WHEN THE VALUE OF " &
                      "WIDTH IS NEGATIVE, WHEN WIDTH IS GREATER " &
                      "THAN FIELD'LAST, OR WHEN THE VALUE OF ITEM " &
                      "IS OUTSIDE THE RANGE OF THE SUBTYPE USED TO " &
                      "INSTANTIATE ENUMERATION_IO");

     DECLARE
          FT : FILE_TYPE;
          TYPE DAY IS (SUNDAY, MONDAY, TUESDAY, WEDNESDAY,
                       THURSDAY, FRIDAY, SATURDAY);
          TODAY : DAY := FRIDAY;
          SUBTYPE WEEKDAY IS DAY RANGE MONDAY .. FRIDAY;
          PACKAGE DAY_IO IS NEW ENUMERATION_IO (WEEKDAY);
          USE DAY_IO;
     BEGIN

          BEGIN
               PUT (FT, TODAY, -1);
               FAILED ("CONSTRAINT_ERROR NOT RAISED; NEGATIVE " &
                       "WIDTH - FILE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("RAISED STATUS_ERROR");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED; NEGATIVE " &
                            "WIDTH - FILE");
          END;

          IF FIELD'LAST < INTEGER'LAST THEN
               BEGIN
                    PUT (FT, TODAY, FIELD'LAST + Ident_Int(1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED; WIDTH " &
                            "GREATER THAN FIELD'LAST + 1- FILE");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED; WIDTH " &
                                 "GREATER THAN FIELD'LAST + 1 - FILE");
               END;

               BEGIN
                    PUT (TODAY, FIELD'LAST + Ident_Int(1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED; WIDTH " &
                            "GREATER THAN FIELD'LAST + 1 - DEFAULT");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED; WIDTH " &
                                 "GREATER THAN FIELD'LAST + 1 " &
                                 "- DEFAULT");
          END;

          END IF;

          TODAY := SATURDAY;

          BEGIN
               PUT (FT, TODAY);
               FAILED ("CONSTRAINT_ERROR NOT RAISED; ITEM VALUE " &
                       "OUT OF RANGE - FILE");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED; ITEM VALUE " &
                            "OUT OF RANGE - FILE");
          END;

          TODAY := FRIDAY;

          BEGIN
               PUT (TODAY, -3);
               FAILED ("CONSTRAINT_ERROR NOT RAISED; NEGATIVE " &
                       "WIDTH - DEFAULT");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("RAISED STATUS_ERROR");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED; NEGATIVE " &
                            "WIDTH - DEFAULT");
          END;

          TODAY := SATURDAY;

          BEGIN
               PUT (TODAY);
               FAILED ("CONSTRAINT_ERROR NOT RAISED; ITEM VALUE " &
                       "OUT OF RANGE - DEFAULT");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED; ITEM VALUE " &
                            "OUT OF RANGE - DEFAULT");
          END;
     END;

     RESULT;

END CE3906D;
