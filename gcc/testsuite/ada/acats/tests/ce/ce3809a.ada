-- CE3809A.ADA

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
--     CHECK THAT FLOAT I/O GET CAN READ A VALUE FROM A STRING.
--     CHECK THAT END_ERROR IS RAISED WHEN CALLED WITH A NULL STRING
--     OR A STRING CONTAINING SPACES AND/OR HORIZONTAL TABULATION
--     CHARACTERS.  CHECK THAT LAST CONTAINS THE INDEX OF THE LAST
--     CHARACTER READ FROM THE STRING.

-- HISTORY:
--     SPS 10/07/82
--     SPS 12/14/82
--     JBG 12/21/82
--     DWC 09/15/87  ADDED CASE TO INCLUDE ONLY TABS IN STRING AND
--                   CHECKED THAT END_ERROR IS RAISED.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3809A IS
BEGIN

     TEST ("CE3809A", "CHECK THAT FLOAT_IO GET " &
                      "OPERATES CORRECTLY ON STRINGS");

     DECLARE
          TYPE FL IS DIGITS 4;
          PACKAGE FLIO IS NEW FLOAT_IO (FL);
          USE FLIO;
          X : FL;
          STR : STRING (1..10) := "   10.25  ";
          L : POSITIVE;
     BEGIN

-- LEFT-JUSTIFIED IN STRING, POSITIVE, NO EXPONENT
          BEGIN
               GET ("896.5  ", X, L);
               IF X /= 896.5 THEN
                    FAILED ("FLOAT VALUE FROM STRING INCORRECT");
               END IF;
          EXCEPTION
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 1");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - FLOAT - 1");
          END;

          IF L /= IDENT_INT (5) THEN
               FAILED ("VALUE OF LAST INCORRECT - FLOAT - 1.  LAST IS" &
                       INTEGER'IMAGE(L));
          END IF;

-- STRING LITERAL WITH BLANKS
          BEGIN
               GET ("   ", X, L);
               FAILED ("END_ERROR NOT RAISED - FLOAT - 2");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= 5 THEN
                         FAILED ("AFTER END_ERROR, VALUE OF LAST " &
                                 "INCORRECT - 2.  LAST IS" &
                                 INTEGER'IMAGE(L));
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 2");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FLOAT - 2");
          END;

-- NULL STRING LITERAL
          BEGIN
               GET ("", X, L);
               FAILED ("END_ERROR NOT RAISED - FLOAT - 3");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= 5 THEN
                         FAILED ("AFTER END_ERROR, VALUE OF LAST " &
                                 "INCORRECT - 3.  LAST IS" &
                                 INTEGER'IMAGE(L));
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 3");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FLOAT - 3");
          END;

-- NULL SLICE
          BEGIN
               GET (STR(5..IDENT_INT(2)), X, L);
               FAILED ("END_ERROR NOT RAISED - FLOAT - 4");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= 5 THEN
                         FAILED ("AFTER END_ERROR, VALUE OF LAST " &
                                 "INCORRECT - 4.  LAST IS" &
                                 INTEGER'IMAGE(L));
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 4");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FLOAT - 4");
          END;

-- SLICE WITH BLANKS
          BEGIN
               GET (STR(IDENT_INT(9)..10), X, L);
               FAILED ("END_ERROR NOT RAISED - FLOAT - 5");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= IDENT_INT(5) THEN
                         FAILED ("AFTER END_ERROR, VALUE OF LAST " &
                                 "INCORRECT - 5.  LAST IS" &
                                 INTEGER'IMAGE(L));
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 5");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FLOAT - 5");
          END;

-- NON-NULL SLICE
          BEGIN
               GET (STR(2..IDENT_INT(8)), X, L);
               IF X /= 10.25 THEN
                    FAILED ("FLOAT VALUE INCORRECT - 6");
               END IF;
               IF L /= 8 THEN
                    FAILED ("LAST INCORRECT FOR SLICE - 6.  LAST IS" &
                            INTEGER'IMAGE(L));
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - 6");
          END;

-- LEFT-JUSTIFIED, POSITIVE EXPONENT
          BEGIN
               GET ("1.34E+02", X, L);
               IF X /= 134.0 THEN
                    FAILED ("FLOAT WITH EXP FROM STRING INCORRECT - 7");
               END IF;

               IF L /= 8 THEN
                    FAILED ("VALUE OF LAST INCORRECT - FLOAT - 7.  " &
                            "LAST IS" & INTEGER'IMAGE(L));
               END IF;
          EXCEPTION
               WHEN DATA_ERROR =>
                    FAILED ("DATA_EROR RAISED - FLOAT - 7");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - FLOAT - 7");
          END;

-- RIGHT-JUSTIFIED, NEGATIVE EXPONENT
          BEGIN
               GET (" 25.0E-2", X, L);
               IF X /= 0.25 THEN
                    FAILED ("NEG EXPONENT INCORRECT - 8");
               END IF;
               IF L /= 8 THEN
                    FAILED ("LAST INCORRECT - 8.  LAST IS" &
                            INTEGER'IMAGE(L));
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - 8");
          END;

-- RIGHT-JUSTIFIED, NEGATIVE
          GET ("  -1.50", X, L);
          IF X /= -1.5 THEN
               FAILED ("FLOAT IN RIGHT JUSTIFIED STRING INCORRECT - 9");
          END IF;
          IF L /= 7 THEN
               FAILED ("LAST INCORRECT - 9.  LAST IS" &
                       INTEGER'IMAGE(L));
          END IF;

-- HORIZONTAL TAB WITH BLANKS
          BEGIN
               GET (" " & ASCII.HT & "2.3E+2", X, L);
               IF X /= 230.0 THEN
                    FAILED ("FLOAT WITH TAB IN STRING INCORRECT - 10");
               END IF;
               IF L /= 8 THEN
                    FAILED ("LAST INCORRECT FOR TAB - 10.  LAST IS" &
                            INTEGER'IMAGE(L));
               END IF;
          EXCEPTION
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR FOR STRING WITH TAB - 10");
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED FOR STRING WITH " &
                            "TAB - 10");
          END;

-- HORIZONTAL TABS ONLY
          BEGIN
               GET (ASCII.HT & ASCII.HT, X, L);
               FAILED ("END_ERROR NOT RAISED - FLOAT - 11");
          EXCEPTION
               WHEN END_ERROR =>
                    IF L /= IDENT_INT(8) THEN
                         FAILED ("AFTER END_ERROR, VALUE OF LAST " &
                                 "INCORRECT - 11.  LAST IS" &
                                 INTEGER'IMAGE(L));
                    END IF;
               WHEN DATA_ERROR =>
                    FAILED ("DATA_ERROR RAISED - FLOAT - 11");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - FLOAT - 11");
          END;
     END;

     RESULT;

END CE3809A;
