-- CE3908A.ADA

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
--     CHECK THAT GET FOR ENUMERATION TYPES CAN OPERATE ON STRINGS.
--     CHECK THAT IT RAISES END_ERROR WHEN THE STRING IS NULL OR
--     EMPTY.  CHECK THAT LAST CONTAINS THE INDEX VALUE OF THE LAST
--     CHARACTER READ FROM THE STRING.

-- HISTORY:
--     SPS 10/11/82
--     VKG 01/06/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     DWC 09/18/87  ADDED CASES WHICH CONTAIN TABS WITH AND WITHOUT
--                   ENUMERATION LITERALS.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3908A IS
BEGIN

     TEST ("CE3908A", "CHECK THAT GET FOR ENUMERATION TYPES CAN " &
                      "OPERATE ON STRINGS.  CHECK THAT IT RAISES " &
                      "END_ERROR WHEN THE STRING IS NULL OR EMPTY.  " &
                      "CHECK THAT LAST CONTAINS THE INDEX VALUE OF " &
                      "THE LAST CHARACTER READ FROM THE STRING");

     DECLARE
          TYPE FRUIT IS (APPLE, PEAR, ORANGE, STRAWBERRY);
          DESSERT : FRUIT;
          PACKAGE FRUIT_IO IS NEW ENUMERATION_IO (FRUIT);
          USE FRUIT_IO;
          L : POSITIVE;
     BEGIN
          GET ("APPLE  ", DESSERT, L);
          IF DESSERT /= APPLE THEN
               FAILED ("ENUMERATION VALUE FROM STRING INCORRECT - 1");
          END IF;

          IF L /= IDENT_INT (5) THEN
               FAILED ("LAST CONTAINS INCORRECT VALUE AFTER GET - 1");
          END IF;

          GET ("APPLE", DESSERT, L);
          IF DESSERT /= APPLE THEN
               FAILED ("ENUMERATION VALUE FROM STRING INCORRECT - 2");
          END IF;

          IF L /= IDENT_INT (5) THEN
               FAILED ("LAST CONTAINS INCORRECT VALUE AFTER GET - 2");
          END IF;

          BEGIN
               GET (ASCII.HT & "APPLE", DESSERT, L);
               IF DESSERT /= APPLE THEN
                    FAILED ("ENUMERATION VALUE FROM STRING " &
                            "INCORRECT - 3");
               END IF;
               IF L /= IDENT_INT(6) THEN
                    FAILED ("LAST CONTAINS INCORRECT VALUE AFTER " &
                            "GET - 3");
               END IF;
          EXCEPTION
               WHEN END_ERROR =>
                    FAILED ("GET DID NOT SKIP LEADING TABS");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 3");
          END;

-- NULL STRING LITERAL.

          BEGIN
               GET ("", DESSERT, L);
               FAILED ("END_ERROR NOT RAISED - 4");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= IDENT_INT(6) THEN
                         FAILED ("LAST CONTAINS INCORRECT VALUE " &
                                 "AFTER GET - 4");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 4");
          END;

          BEGIN
               GET (ASCII.HT & "", DESSERT, L);
               FAILED ("END_ERROR NOT RAISED - 5");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= IDENT_INT(6) THEN
                         FAILED ("LAST CONTAINS INCORRECT VALUE " &
                                 "AFTER GET - 5");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 5");
          END;

-- STRING LITERAL WITH BLANKS.

          BEGIN
               GET("     ", DESSERT, L);
               FAILED ("END ERROR NOT RAISED - 6");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= IDENT_INT(6) THEN
                         FAILED ("LAST CONTAINS INCORRECT VALUE " &
                                 "AFTER GET - 6");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 6");
          END;

     END;

     RESULT;
END CE3908A;
