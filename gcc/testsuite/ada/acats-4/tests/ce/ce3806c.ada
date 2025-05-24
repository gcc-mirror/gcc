-- CE3806C.ADA

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
--     CHECK THAT PUT FOR FLOAT_IO RAISES CONSTRAINT_ERROR WHEN THE
--     VALUES SUPPLIED BY FORE, AFT, OR EXP ARE NEGATIVE OR GREATER
--     THAN FIELD'LAST WHEN FIELD'LAST < FIELD'BASE'LAST.  ALSO CHECK
--     THAT PUT FOR FLOAT_IO RAISES CONSTRAINT_ERROR WHEN THE VALUE OF
--     ITEM IS OUTSIDE THE RANGE OF THE TYPE USED TO INSTANTIATE
--     FLOAT_IO.

-- HISTORY:
--     SPS 09/10/82
--     JBG 08/30/83
--     JLH 09/14/87  ADDED CASES FOR COMPLETE OBJECTIVE.
--     KAS 11/24/95  DELETED DIGITS CONSTRAINT FROM SUBTYPE
--                   CHANGED STATIC EXPRESSIONS INVOLVING 'LAST

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3806C IS

     FIELD_LAST : TEXT_IO.FIELD := TEXT_IO.FIELD'LAST;

BEGIN

     TEST ("CE3806C", "CHECK THAT PUT FOR FLOAT_IO RAISES " &
                      "CONSTRAINT_ERROR APPROPRIATELY");

     DECLARE
          TYPE FLOAT IS DIGITS 5 RANGE 0.0 .. 2.0;
          SUBTYPE MY_FLOAT IS FLOAT RANGE 0.0 .. 1.0;
          PACKAGE NFL_IO IS NEW FLOAT_IO (MY_FLOAT);
          USE NFL_IO;
          FT : FILE_TYPE;
          Y : FLOAT := 1.8;
          X : MY_FLOAT := 26.3 / 26.792;

     BEGIN
          BEGIN
               PUT (FT, X, FORE => IDENT_INT(-6));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE FORE " &
                       "FLOAT");
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
                            "FLOAT");
          END;

          BEGIN
               PUT (FT, X, AFT => IDENT_INT(-2));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE AFT " &
                       "FLOAT");
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
                            "FLOAT");
          END;

          BEGIN
               PUT (FT, X, EXP => IDENT_INT(-1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - NEGATIVE EXP " &
                       "FLOAT");
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
                            "FLOAT");
          END;

          IF FIELD_LAST < FIELD'BASE'LAST THEN

               BEGIN
                    PUT (FT, X, FORE => IDENT_INT(FIELD_LAST+1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - FORE FLOAT");
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
                         FAILED ("WRONG EXCEPTION RAISED - FORE FLOAT");
               END;

               BEGIN
                    PUT (FT, X, AFT => IDENT_INT(FIELD_LAST+1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - AFT FLOAT");
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
                         FAILED ("WRONG EXCEPTION RAISED - AFT FLOAT");
               END;

               BEGIN
                    PUT (FT, X, EXP => IDENT_INT(FIELD_LAST+1));
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - EXP FLOAT");
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
                         FAILED ("WRONG EXCEPTION RAISED - EXP FLOAT");
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

END CE3806C;
