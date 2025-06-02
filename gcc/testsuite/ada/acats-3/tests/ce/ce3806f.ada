-- CE3806F.ADA

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
--     CHECK THAT PUT FOR FIXED_IO RAISES CONSTRAINT_ERROR WHEN THE
--     VALUES SUPPLIED BY FORE, AFT, OR EXP ARE NEGATIVE OR GREATER
--     THAN FIELD'LAST WHEN FIELD'LAST < FIELD'BASE'LAST.  ALSO CHECK
--     THAT PUT FOR FIXED_IO RAISES CONSTRAINT_ERROR WHEN THE VALUE
--     OF ITEM IS OUTSIDE THE RANGE OF THE TYPE USED TO INSTANTIATE
--     FIXED_IO.

-- HISTORY:
--     JLH 09/15/87  CREATED ORIGINAL TEST.
--     JRL 06/07/96  Added call to Ident_Int in expressions involving
--                   Field'Last, to make the expressions non-static and
--                   prevent compile-time rejection.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3806F IS

BEGIN

     TEST ("CE3806F", "CHECK THAT PUT FOR FIXED_IO RAISES " &
                      "CONSTRAINT_ERROR APPROPRIATELY");

     DECLARE
          TYPE FIXED IS DELTA 0.01 RANGE 1.0 .. 2.0;
          SUBTYPE MY_FIXED IS FIXED DELTA 0.01 RANGE 1.0 .. 1.5;
          PACKAGE NFX_IO IS NEW FIXED_IO (MY_FIXED);
          USE NFX_IO;
          FT : FILE_TYPE;
          Y : FIXED := 1.8;
          X : MY_FIXED := 1.3;

     BEGIN

          BEGIN
               PUT (FT, X, FORE => IDENT_INT(-6));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE FORE " &
                       "FIXED");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 1");
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 1");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - NEGATIVE FORE " &
                            "FIXED");
          END;

          BEGIN
               PUT (FT, X, AFT => IDENT_INT(-2));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE AFT " &
                       "FIXED");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 2");
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 2");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - NEGATIVE AFT " &
                            "FIXED");
          END;

          BEGIN
               PUT (FT, X, EXP => IDENT_INT(-1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE EXP " &
                       "FIXED");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN STATUS_ERROR =>
                    FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 3");
               WHEN USE_ERROR =>
                    FAILED ("USE_ERROR RAISED INSTEAD OF " &
                            "CONSTRAINT_ERROR - 3");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - NEGATIVE EXP " &
                            "FIXED");
          END;

          IF FIELD'LAST < FIELD'BASE'LAST THEN

               BEGIN
                    PUT (FT, X, FORE => IDENT_INT(FIELD'LAST+Ident_Int(1)));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - FORE FIXED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN STATUS_ERROR =>
                         FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 4");
                    WHEN USE_ERROR =>
                         FAILED ("USE_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 4");
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - FORE FIXED");
               END;

               BEGIN
                    PUT (FT, X, AFT => IDENT_INT(FIELD'LAST+Ident_Int(1)));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - AFT FIXED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN STATUS_ERROR =>
                         FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 5");
                    WHEN USE_ERROR =>
                         FAILED ("USE_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 5");
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - AFT FIXED");
               END;

               BEGIN
                    PUT (FT, X, EXP => IDENT_INT(FIELD'LAST+Ident_Int(1)));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - EXP FIXED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN STATUS_ERROR =>
                         FAILED ("STATUS_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 6");
                    WHEN USE_ERROR =>
                         FAILED ("USE_ERROR RAISED INSTEAD OF " &
                                 "CONSTRAINT_ERROR - 6");
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED - EXP FIXED");
               END;

          END IF;

          BEGIN
               PUT (FT, Y);
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
               PUT (Y);
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

END CE3806F;
