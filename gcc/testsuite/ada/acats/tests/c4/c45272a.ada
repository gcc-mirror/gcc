-- C45272A.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR 
-- RECORDS WHOSE COMPONENTS HAVE CHANGEABLE DISCRIMINANTS, INCLUDING
-- RECORDS DESIGNATED BY ACCESS VALUES.

-- TBN  8/7/86

WITH REPORT; USE REPORT;
PROCEDURE C45272A IS

     SUBTYPE INT IS INTEGER RANGE 0 .. 20;
     TYPE VARSTR (LEN : INT := 0) IS
          RECORD
               VAL : STRING (1..LEN);
          END RECORD;
     TYPE VARREC IS
          RECORD
               A, B : VARSTR;
          END RECORD;

     TYPE CELL2;
     TYPE LINK IS ACCESS CELL2;
     TYPE CELL1 (NAM_LEN : INT := 0) IS
          RECORD
               NAME : STRING (1..NAM_LEN);
          END RECORD;
     TYPE CELL2 IS
          RECORD
               ONE : CELL1;
               TWO : CELL1;
               NEW_LINK : LINK;
          END RECORD;

     X, Y : VARREC;
     FRONT : LINK := NEW CELL2'((5, "XXYZZ"), (5, "YYYZZ"), NULL);
     BACK : LINK := NEW CELL2'((5, "XXYZZ"), (5, "YYYZZ"), NULL);

BEGIN
     TEST ("C45272A", "CHECK THAT EQUALITY AND INEQUALITY ARE " &
                      "EVALUATED CORRECTLY FOR RECORDS WHOSE " &
                      "COMPONENTS HAVE CHANGEABLE DISCRIMINANTS");

     X := ((5, "AAAXX"), (5, "BBBYY"));
     Y := ((5, "AAAZZ"), (5, "BBBYY"));
     IF X = Y THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 1");
     END IF;

     X.A := (3, "HHH");
     Y.A := (IDENT_INT(3), IDENT_STR("HHH"));
     IF X /= Y THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 2");
     END IF;

     IF FRONT.ALL /= BACK.ALL THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 3");
     END IF;

     BACK.NEW_LINK := FRONT;
     IF FRONT.ALL = BACK.ALL THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 4");
     END IF;

     FRONT.NEW_LINK := FRONT;
     IF FRONT.ALL /= BACK.ALL THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 5");
     END IF;

     FRONT.ONE := (5, "XXXXX");
     BACK.ONE := (5, "ZZZZZ");
     IF FRONT.ALL = BACK.ALL THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 6");
     END IF;

     FRONT.ONE := (3, "XXX");
     BACK.ONE := (3, "XXX");
     IF FRONT.ALL /= BACK.ALL THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 7");
     END IF;

     RESULT;
END C45272A;
