-- CE3707A.ADA

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
--     CHECK THAT INTEGER_IO GET CAN READ A VALUE FROM A STRING.  CHECK
--     THAT IT TREATS THE END OF THE STRING AS A FILE TERMINATOR.  CHECK
--     THAT LAST CONTAINS THE INDEX VALUE OF THE LAST CHARACTER READ
--     FROM THE STRING.

-- HISTORY:
--     SPS 10/05/82
--     VKG 01/13/83
--     JLH 09/11/87  CORRECTED EXCEPTION HANDLING.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3707A IS

     PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
     USE IIO;
     X : INTEGER;
     L : POSITIVE;
     STR : STRING(1..6) := "123456" ;

BEGIN

     TEST ("CE3707A", "CHECK THAT INTEGER_IO GET OPERATES CORRECTLY " &
                      "ON STRINGS");

-- LEFT JUSTIFIED STRING NON NULL

     GET ("2362  ", X, L);
     IF X /= 2362 THEN
          FAILED ("VALUE FROM STRING INCORRECT - 1");
     END IF;

     IF L /= 4 THEN
          FAILED ("VALUE OF LAST INCORRECT - 1");
     END IF;

-- STRING LITERAL WITH BLANKS

     BEGIN
          GET ("  ", X, L);
          FAILED ("END_ERROR NOT RAISED - 2");
     EXCEPTION
          WHEN END_ERROR =>
               IF L /= 4 THEN
                    FAILED ("AFTER END ERROR VALUE OF LAST " &
                            "INCORRECT - 2");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 2");
     END;

-- NULL STRING

     BEGIN
          GET ("", X, L);
          FAILED (" END_ERROR NOT RAISED - 3");
     EXCEPTION
          WHEN END_ERROR =>
               IF L /= 4 THEN
                    FAILED ("AFTER END_ERROR VALUE OF LAST " &
                            "INCORRECT - 3");
               END IF;
          WHEN OTHERS =>
               FAILED ("SOME EXCEPTION RAISED - 3");
     END;

-- NULL SLICE

     BEGIN
          GET(STR(5..IDENT_INT(2)), X, L);
          FAILED ("END_ERROR NOT RAISED - 4");
     EXCEPTION
          WHEN END_ERROR =>
               IF L /= 4 THEN
                    FAILED ("AFTER END_ERROR VALUE OF LAST " &
                            "INCORRECT - 4");
               END IF;
          WHEN OTHERS =>
               FAILED ("SOME EXCEPTION RAISED - 4");
     END;

-- NON-NULL SLICE

     GET (STR(2..3), X, L);
     IF X /= 23 THEN
           FAILED ("INTEGER VALUE INCORRECT - 5");
     END IF;
     IF L /= 3 THEN
          FAILED ("LAST INCORRECT FOR SLICE - 5");
     END IF;

-- RIGHT JUSTIFIED NEGATIVE NUMBER

     GET("   -2345",X,L);
     IF X /= -2345 THEN
          FAILED ("INTEGER VALUE INCORRECT - 6");
     END IF;
     IF L /= 8 THEN
          FAILED ("LAST INCORRECT FOR NEGATIVE NUMBER - 6");
     END IF;

     RESULT;

END CE3707A;
