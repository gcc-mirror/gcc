-- C45281A.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE EVALUATED CORRECTLY FOR ACCESS
-- TYPES.

-- TBN  8/8/86

WITH REPORT; USE REPORT;
PROCEDURE C45281A IS

     TYPE STR_NAME IS ACCESS STRING;

     TYPE GENDER IS (F, M);
     TYPE PERSON (SEX : GENDER) IS
          RECORD
               NAME : STRING (1..6) := "NONAME";
          END RECORD;

     TYPE PERSON_NAME IS ACCESS PERSON;
     SUBTYPE MALE IS PERSON_NAME (M);
     SUBTYPE FEMALE IS PERSON_NAME (F);

     S : STR_NAME (1..10) := NEW STRING'("0123456789");
     T : STR_NAME (1..10) := S;
     A : MALE;
     B : FEMALE;
     C : PERSON_NAME;

BEGIN
     TEST ("C45281A", "CHECK THAT EQUALITY AND INEQUALITY ARE " &
                      "EVALUATED CORRECTLY FOR ACCESS TYPES");

     IF "/=" (LEFT => S, RIGHT => T) THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS VALUES - 1");
     END IF;
     T := NEW STRING'("0123456789");
     IF "=" (S, T) THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS VALUES - 2");
     END IF;

     IF A /= B THEN
          FAILED ("INCORRECT RESULTS FOR NULL ACCESS VALUES - 3");
     END IF;
     IF A /= C THEN
          FAILED ("INCORRECT RESULTS FOR NULL ACCESS VALUES - 4");
     END IF;

     A := NEW PERSON'(M, "THOMAS");
     IF "=" (LEFT => A, RIGHT => B) THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS VALUES - 5");
     END IF;
     C := A;
     IF C /= A THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS VALUES - 6");
     END IF;
     C := NEW PERSON'(M, "THOMAS");
     IF A = C THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS VALUES - 7");
     END IF;

     RESULT;
END C45281A;
