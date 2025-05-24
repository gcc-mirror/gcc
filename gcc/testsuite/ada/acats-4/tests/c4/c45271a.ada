-- C45271A.ADA

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
-- RECORDS WHOSE COMPONENTS DO NOT HAVE CHANGEABLE DISCRIMINANTS.

-- TBN  8/6/86

WITH REPORT; USE REPORT;
PROCEDURE C45271A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 20;
     TYPE ARRAY_BOOL IS ARRAY (1 .. 5) OF BOOLEAN;

     TYPE REC_TYPE1 IS
          RECORD
               BOOL : ARRAY_BOOL;
               A : INTEGER;
          END RECORD;

     TYPE REC_TYPE2 (LEN : INT := 3) IS
          RECORD
               A : STRING (1 .. LEN);
          END RECORD;

     TYPE REC_TYPE3 (NUM : INT := 1) IS
          RECORD
               A : REC_TYPE1;
          END RECORD;

     REC1, REC2 : REC_TYPE1 := (A => 2, OTHERS => (OTHERS => TRUE));
     REC3, REC4 : REC_TYPE2 (5) := (5, "WHERE");
     REC5, REC6 : REC_TYPE2;
     REC7, REC8 : REC_TYPE3;
     REC9, REC10 : REC_TYPE3 (3) := (NUM => 3, A =>
                                   (A => 5, BOOL => (OTHERS => FALSE)));

BEGIN
     TEST ("C45271A", "CHECK THAT EQUALITY AND INEQUALITY ARE " &
                      "EVALUATED CORRECTLY FOR RECORDS WHOSE " &
                      "COMPONENTS DO NOT HAVE CHANGEABLE " &
                      "DISCRIMINANTS");

     IF "/=" (LEFT => REC1, RIGHT => REC2) THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 1");
     END IF;
     REC1.A := IDENT_INT(1);
     IF "=" (LEFT => REC2, RIGHT => REC1) THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 2");
     END IF;

     IF REC3 /= REC4 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 3");
     END IF;
     REC4.A := IDENT_STR("12345");
     IF REC3 = REC4 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 4");
     END IF;

     REC5.A := IDENT_STR("WHO");
     REC6.A := IDENT_STR("WHY");
     IF REC5 = REC6 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 5");
     END IF;
     REC5.A := "WHY";
     IF REC6 /= REC5 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 6");
     END IF;

     REC7.A.A := IDENT_INT(1);
     REC7.A.BOOL := (OTHERS => IDENT_BOOL(TRUE));
     REC8.A.A := 1;
     REC8.A.BOOL := (OTHERS => TRUE);
     IF REC7 /= REC8 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 7");
     END IF;
     REC8.A.BOOL := (OTHERS => IDENT_BOOL(FALSE));
     IF REC8 = REC7 THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 8");
     END IF;

     IF "/=" (LEFT => REC9, RIGHT => REC10) THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 9");
     END IF;
     REC9.A.A := IDENT_INT(1);
     IF "=" (LEFT => REC9, RIGHT => REC10) THEN
          FAILED ("INCORRECT RESULTS FOR RECORDS - 10");
     END IF;

     RESULT;
END C45271A;
