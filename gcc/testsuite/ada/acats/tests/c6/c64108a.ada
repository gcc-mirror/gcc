-- C64108A.ADA

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
-- CHECK THAT ALL PERMITTED FORMS OF VARIABLE NAMES ARE PERMITTED
--   AS ACTUAL PARAMETERS.

-- DAS 2/10/81
-- SPS 10/26/82
-- SPS 11/5/82

WITH REPORT;
PROCEDURE C64108A IS

     USE REPORT;
     SUBTYPE INT IS INTEGER RANGE 1..3;
     TYPE REC (N : INT) IS
          RECORD
               S : STRING (1..N);
          END RECORD;
     TYPE PTRSTR IS ACCESS STRING;

     R1,R2,R3 : REC(3);
     S1,S2,S3 : STRING (1..3);
     PTRTBL   : ARRAY (1..3) OF PTRSTR;

     PROCEDURE P1 (S1 : IN STRING; S2: IN OUT STRING; 
                   S3 : OUT STRING) IS
     BEGIN
          S3 := S2;
          S2 := S1;
     END P1;

     PROCEDURE P2 (C1 : IN CHARACTER; C2 : IN OUT CHARACTER;
                   C3 : OUT CHARACTER) IS
     BEGIN
          C3 := C2;
          C2 := C1;
     END P2;

     FUNCTION F1 (X : INT) RETURN PTRSTR IS
     BEGIN
          RETURN PTRTBL(X);
     END F1;

     FUNCTION "+" (S1,S2 : STRING) RETURN PTRSTR IS
     BEGIN
          RETURN PTRTBL(CHARACTER'POS(S1(1))-CHARACTER'POS('A')+1);
     END "+";

BEGIN

     TEST ("C64108A", "CHECK THAT ALL PERMITTED FORMS OF VARIABLE" &
                      " NAMES ARE PERMITTED AS ACTUAL PARAMETERS");

     S1 := "AAA";
     S2 := "BBB";
     P1 (S1, S2, S3);
     IF (S2 /= "AAA") OR (S3 /= "BBB") THEN
          FAILED ("SIMPLE VARIABLE AS AN ACTUAL PARAMETER NOT WORKING");
     END IF;

     S1 := "AAA";
     S2 := "BBB";
     S3 := IDENT_STR("CCC");
     P2 (S1(1), S2(IDENT_INT(1)), S3(1));
     IF (S2 /= "ABB") OR (S3 /= "BCC") THEN
          FAILED ("INDEXED COMPONENT AS AN ACTUAL PARAMETER NOT " &
                  "WORKING");
     END IF;

     R1.S := "AAA";
     R2.S := "BBB";
     P1 (R1.S, R2.S, R3.S);
     IF (R2.S /= "AAA") OR (R3.S /= "BBB") THEN
          FAILED ("SELECTED COMPONENT AS AN ACTUAL PARAMETER" &
                  " NOT WORKING");
     END IF;

     S1 := "AAA";
     S2 := "BBB";
     P1 (S1(1..IDENT_INT(2)), S2(1..2), S3(IDENT_INT(1)..IDENT_INT(2)));
     IF (S2 /= "AAB") OR (S3 /= "BBC") THEN
          FAILED ("SLICE AS AN ACTUAL PARAMETER NOT WORKING");
     END IF;

     PTRTBL(1) := NEW STRING'("AAA");
     PTRTBL(2) := NEW STRING'("BBB");
     PTRTBL(3) := NEW STRING'("CCC");
     P1 (F1(1).ALL, F1(2).ALL, F1(IDENT_INT(3)).ALL);
     IF (PTRTBL(2).ALL /= "AAA") OR (PTRTBL(3).ALL /= "BBB") THEN
          FAILED ("SELECTED COMPONENT OF FUNCTION VALUE AS AN ACTUAL" &
                  " PARAMETER NOT WORKING");
     END IF;

     PTRTBL(1) := NEW STRING'("AAA");
     PTRTBL(2) := NEW STRING'("BBB");
     PTRTBL(3) := NEW STRING'("CCC");
     S1 := IDENT_STR("AAA");
     S2 := IDENT_STR("BBB");
     S3 := IDENT_STR("CCC");
     P1 ("+"(S1,S1).ALL, "+"(S2,S2).ALL, "+"(S3,S3).ALL);
     IF (PTRTBL(2).ALL /= "AAA") OR (PTRTBL(3).ALL /= "BBB") THEN
          FAILED ("SELECTED COMPONENT OF OVERLOADED OPERATOR FUNCTION" &
                  " VALUE AS AN ACTUAL PARAMETER NOT WORKING");
     END IF;

     PTRTBL(1) := NEW STRING'("AAA");
     PTRTBL(2) := NEW STRING'("BBB");
     PTRTBL(3) := NEW STRING'("CCC");
     P2 (F1(1)(1), F1(IDENT_INT(2))(1), F1(3)(IDENT_INT(1)));
     IF (PTRTBL(2).ALL /= "ABB") OR (PTRTBL(3).ALL /= "BCC") THEN
          FAILED ("INDEXED COMPONENT OF FUNCTION VALUE AS AN ACTUAL" &
                  " PARAMETER NOT WORKING");
     END IF;

     PTRTBL(1) := NEW STRING'("AAA");
     PTRTBL(2) := NEW STRING'("BBB");
     PTRTBL(3) := NEW STRING'("CCC");
     P1 (F1(1)(2..3), F1(2)(IDENT_INT(2)..3), F1(3)(2..IDENT_INT(3)));
     IF (PTRTBL(2).ALL /= "BAA") OR (PTRTBL(3).ALL /= "CBB") THEN
          FAILED ("SLICE OF FUNCTION VALUE AS AN ACTUAL PARAMETER" &
                  " NOT WORKING");
     END IF;

     RESULT;

END C64108A;
