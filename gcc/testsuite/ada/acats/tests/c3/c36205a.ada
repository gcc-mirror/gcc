-- C36205A.ADA

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

-- BASIC CHECKS OF ARRAY OBJECTS AND WHOLE ARRAYS PASSED AS
--   PARAMETERS

-- DAT 2/17/81
-- JBG 9/11/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT;
PROCEDURE C36205A IS

     USE REPORT;

     TYPE I_A IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
     TYPE I_A_2 IS ARRAY (INTEGER RANGE <> ,
          INTEGER RANGE <> ) OF INTEGER;
     A10 : I_A (1 .. 10);
     A20 : I_A (18 .. 20);
     I10 : INTEGER := IDENT_INT (10);
     A2_10 : I_A_2 (1 .. I10, 3+I10 .. I10+I10);       -- 1..10, 13..20
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
     TEST ( "C36205A", "CHECKING ATTRIBUTE VALUES POSSESSED BY FORMAL "&
                       "PARAMETERS WHOSE ACTUALS ARE UNCONSTRAINED " &
                       "ARRAYS - BASIC CHECKS");

     IF A10'FIRST /= 1
        OR A2_10'FIRST(1) /= 1
        OR A2_10'FIRST(2) /= IDENT_INT(13)
        OR A2_20'FIRST /= 11
        OR A2_20'FIRST(2) /= 21
     THEN
          FAILED ("'FIRST FOR OBJECTS IS WRONG");
     END IF;


     IF A10'LAST(1) /= 10
        OR A2_10'LAST /= 10
        OR A2_10'LAST(2) /= 20
        OR A2_20'LAST(1) /= 30
        OR A2_20'LAST(2) /= IDENT_INT(20)
     THEN
          FAILED ("'LAST FOR OBJECTS IS WRONG");
     END IF;
       IF A10'LENGTH /= IDENT_INT(10)
        OR A2_10'LENGTH(1) /= 10
        OR A2_10'LENGTH(2) /= IDENT_INT(8)
        OR A2_20'LENGTH /= 20
        OR A2_20'LENGTH(2) /= IDENT_INT(0)
     THEN
          FAILED ("'LENGTH FOR OBJECTS IS WRONG");
     END IF;

     IF 0 IN A10'RANGE
        OR IDENT_INT(11) IN A10'RANGE(1)
        OR IDENT_INT(0) IN A2_10'RANGE(1)
        OR 11 IN A2_10'RANGE
        OR 12 IN A2_10'RANGE(2)
        OR IDENT_INT(21) IN A2_10'RANGE(2)
        OR 10 IN A2_20'RANGE
        OR IDENT_INT(31) IN A2_20'RANGE(1)
        OR IDENT_INT(20) IN A2_20'RANGE(2)
        OR 0 IN A2_20'RANGE(2)
     THEN
          FAILED ("'RANGE FOR OBJECTS IS WRONG");
     END IF;

     P1 (A10, 1, 10, "P1 1");
     P1 (A20, 18, 20, "P1 A20");
     P2(A2_10, 1, 10, 13, 20, "P2 1");
     P2 (A2_20, 11, 30, 21, 20, "P2 2");
     S1 (ALF, 1, 5, "X0");
     S1 (ARF, 5, 9, "ARF1");

     RESULT;

END C36205A;
