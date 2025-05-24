-- C36205I.ADA

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
-- CHECK THAT ATTRIBUTES GIVE THE CORRECT VALUES FOR
-- UNCONSTRAINED FORMAL PARAMETERS.

-- ATTRIBUTES OF DYNAMIC NULL AGGREGATES

-- DAT 2/17/81
-- JBG 9/11/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE C36205I IS

     USE REPORT;

     TYPE I_A IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
     TYPE I_A_2 IS ARRAY (INTEGER RANGE <> ,
          INTEGER RANGE <> ) OF INTEGER;
     A10 : I_A (1 .. 10);
     A20 : I_A (18 .. 20);
     I10 : INTEGER := IDENT_INT (10);
     A2_10 : I_A_2 (1 .. I10, 3+I10 .. I10*I10);       -- 1..10, 13..20
     A2_20 : I_A_2 (11 .. 3*I10, I10+11 .. I10+I10);   -- 11..30, 21..20
     SUBTYPE STR IS STRING;
     ALF : CONSTANT STR(IDENT_INT(1)..IDENT_INT(5)) := "ABCDE";
     ARF : STR(5 .. 9) := ALF;

     PROCEDURE P1 (A : I_A; FIR, LAS: INTEGER; S : STRING) IS
     BEGIN
          IF A'FIRST /= FIR
             OR A'FIRST(1) /= FIR
          THEN
               FAILED ("'FIRST IS WRONG " & S);
          END IF;

          IF A'LAST /= LAS
             OR A'LAST(1) /= LAS
          THEN
               FAILED ("'LAST IS WRONG " & S);
          END IF;

          IF A'LENGTH /= LAS - FIR + 1
             OR A'LENGTH /= A'LENGTH(1)
          THEN
               FAILED ("'LENGTH IS WRONG " & S);
          END IF;

          IF (LAS NOT IN A'RANGE AND LAS >= FIR)
             OR (FIR NOT IN A'RANGE AND LAS >= FIR)
             OR FIR - 1 IN A'RANGE
             OR LAS + 1 IN A'RANGE(1)
          THEN
               FAILED ("'RANGE IS WRONG " & S);
          END IF;

     END P1;

     PROCEDURE P2 (A : I_A_2; F1,L1,F2,L2 : INTEGER; S : STRING) IS
     BEGIN
          IF A'FIRST /= A'FIRST(1)
             OR A'FIRST /= F1
          THEN
               FAILED ("'FIRST(1) IS WRONG " & S);
          END IF;

          IF A'LAST(1) /= L1 THEN
               FAILED ("'LAST(1) IS WRONG " & S);
          END IF;

          IF A'LENGTH(1) /= A'LENGTH
             OR A'LENGTH /= L1 - F1 + 1
          THEN
               FAILED ("'LENGTH(1) IS WRONG " & S);
          END IF;

          IF F1 - 1 IN A'RANGE
             OR (F1 NOT IN A'RANGE AND F1 <= L1)
             OR (L1 NOT IN A'RANGE(1) AND F1 <= L1)
             OR L1 + 1 IN A'RANGE(1)
          THEN
               FAILED ("'RANGE(1) IS WRONG " & S);
          END IF;

          IF A'FIRST(2) /= F2 THEN
               FAILED ("'FIRST(2) IS WRONG " & S);
          END IF;

          IF A'LAST(2) /= L2 THEN
               FAILED ("'LAST(2) IS WRONG " & S);
          END IF;

          IF L2 - F2 /= A'LENGTH(2) - 1 THEN
               FAILED ("'LENGTH(2) IS WRONG " & S);
          END IF;

          IF F2 - 1 IN A'RANGE(2)
             OR (F2 NOT IN A'RANGE(2) AND A'LENGTH(2) > 0)
             OR (L2 NOT IN A'RANGE(2) AND A'LENGTH(2) /= 0)
             OR L2 + 1 IN A'RANGE(2)
          THEN
               FAILED ("'RANGE(2) IS WRONG " & S);
          END IF;
     END P2;

     PROCEDURE S1 (S:STR; F,L:INTEGER; MESS:STRING) IS
     BEGIN
          IF S'FIRST /= F THEN
               FAILED ("STRING 'FIRST IS WRONG " & MESS);
          END IF;

          IF S'LAST(1) /= L THEN
               FAILED ("STRING 'LAST IS WRONG " & MESS);
          END IF;

          IF S'LENGTH /= L - F + 1
             OR S'LENGTH(1) /= S'LENGTH
          THEN
               FAILED ("STRING 'LENGTH IS WRONG " & MESS);
          END IF;

          IF (F <= L AND
                (F NOT IN S'RANGE
                OR L NOT IN S'RANGE
                OR F NOT IN S'RANGE(1)
                OR L NOT IN S'RANGE(1)))
             OR F - 1 IN S'RANGE
             OR L + 1 IN S'RANGE(1)
          THEN
               FAILED ("STRING 'RANGE IS WRONG " & MESS);
          END IF;
     END S1;

BEGIN
     TEST ( "C36205I", "CHECKING ATTRIBUTE VALUES POSSESSED BY FORMAL "&
                       "PARAMETERS WHOSE ACTUALS ARE UNCONSTRAINED " &
                       "ARRAYS - DYNAMIC NULL AGGREGATES");


     P1 ((IDENT_INT(5) .. IDENT_INT(4) => 4), 5, 4, "P1 18");
     P1 ((IDENT_INT(1) .. IDENT_INT(0) => 0), 1, 0, "P1 19");
     P1 ((IDENT_INT(-12) .. -13 => 3), -12, -13, "P1 21");

     RESULT;
END C36205I;
