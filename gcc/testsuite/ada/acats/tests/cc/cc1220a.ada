-- CC1220A.ADA

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
--     CHECK THAT A GENERIC UNIT CAN REFER TO AN IMPLICITLY
--     DECLARED PREDEFINED OPERATOR.

-- HISTORY:
--     DAT 08/20/81  CREATED ORIGINAL TEST.
--     SPS 05/03/82
--     BCB 08/04/88  MODIFIED HEADER FORMAT AND ADDED CHECKS FOR OTHER
--                   OPERATIONS OF A DISCRETE TYPE.
--     RJW 03/27/90  REVISED TEST TO CHECK FOR A GENERIC FORMAL
--                   DISCRETE TYPE.
--     CJJ 10/14/90  ADDED CHECKS FOR RELATIONAL OPERATOR (<, <=, >, >=);
--                   MADE FAILED MESSAGES IN PROCEDURE BODY MORE SPECIFIC.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE CC1220A IS

BEGIN
     TEST ("CC1220A", "GENERIC UNIT CAN REFER TO IMPLICITLY " &
           "DECLARED OPERATORS");


     DECLARE

          GENERIC
               TYPE T IS (<>);
               STR : STRING;
               P1 : T := T'FIRST;
               P2 : T := T(T'SUCC (P1));
               P3 : T := T'(T'PRED (P2));
               P4 : INTEGER := IDENT_INT(T'WIDTH);
               P5 : BOOLEAN := (P1 < P2) AND (P2 > P3);
               P6: BOOLEAN := (P1 <= P3) AND (P2 >= P1);
               P7 : BOOLEAN := (P3 = P1);
               P8 : T := T'BASE'FIRST;
               P10 : T := T'LAST;
               P11 : INTEGER := T'SIZE;
               P12 : ADDRESS := P10'ADDRESS;
               P13 : INTEGER := T'WIDTH;
               P14 : INTEGER := T'POS(T'LAST);
               P15 : T := T'VAL(1);
               P16 : INTEGER := T'POS(P15);
               P17 : STRING := T'IMAGE(T'BASE'LAST);
               P18 : T := T'VALUE(P17);
               P19 : BOOLEAN := (P15 IN T);
               WITH FUNCTION IDENT (X : T) RETURN T;
          PACKAGE PKG IS
               ARR : ARRAY (1 .. 3) OF T := (P1,P2,P3);
               B1 : BOOLEAN := P7 AND P19;
               B2 : BOOLEAN := P5 AND P6;
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF P1 /= T(T'FIRST) THEN
                    FAILED ("IMPROPER VALUE FOR 'FIRST - " & STR);
               END IF;

               IF T'SUCC (P1) /= IDENT (P2) OR
                  T'PRED (P2) /= IDENT (P1) THEN
                    FAILED ("IMPROPER VALUE FOR 'SUCC, PRED - " & STR);
               END IF;

               IF P10 /= T(T'LAST) THEN
                    FAILED ("IMPROPER VALUE FOR 'LAST - " & STR);
               END IF;

               IF NOT EQUAL(P11,T'SIZE) THEN
                    FAILED ("IMPROPER VALUE FOR 'SIZE - " & STR);
               END IF;

               IF NOT EQUAL(P13,T'WIDTH) THEN
                    FAILED ("IMPROPER VALUE FOR 'WIDTH - " & STR);
               END IF;

               IF NOT EQUAL (P16, T'POS (P15)) OR
                  T'VAL (P16) /= T(IDENT (P15)) THEN
                    FAILED ("IMPROPER VALUE FOR 'POS, 'VAL - " & STR);
               END IF;

               IF T'VALUE (P17) /= T'BASE'LAST OR
                  T'IMAGE (P18) /= T'IMAGE (T'BASE'LAST) THEN
                    FAILED ("IMPROPER VALUE FOR 'VALUE, 'IMAGE - " &
                             STR);
               END IF;
          END PKG;

     BEGIN
          DECLARE
               TYPE CHAR IS ('A', 'B', 'C', 'D', 'E');

               FUNCTION IDENT (C : CHAR) RETURN CHAR IS
               BEGIN
                    RETURN CHAR'VAL (IDENT_INT (CHAR'POS (C)));
               END IDENT;

               PACKAGE N_CHAR IS NEW PKG (T => CHAR, STR => "CHAR",
                                          IDENT => IDENT);
          BEGIN
               IF N_CHAR.ARR (1) /= IDENT ('A') OR
                  N_CHAR.ARR (2) /= IDENT ('B') OR
                  N_CHAR.ARR (3) /= 'A' OR
                  N_CHAR.B1 /= TRUE OR
                 N_CHAR.B2 /= TRUE THEN
                    FAILED ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
                            " IN INSTANTIATION OF N_CHAR.");
               END IF;
          END;

          DECLARE
               TYPE ENUM IS (JOVIAL, ADA, FORTRAN, BASIC);

               FUNCTION IDENT (C : ENUM) RETURN ENUM IS
               BEGIN
                    RETURN ENUM'VAL (IDENT_INT (ENUM'POS (C)));
               END IDENT;

               PACKAGE N_ENUM IS NEW PKG (T => ENUM, STR => "ENUM",
                                          IDENT => IDENT);

          BEGIN
               IF N_ENUM.ARR (1) /= IDENT (JOVIAL) OR
                  N_ENUM.ARR (2) /= IDENT (ADA) OR
                  N_ENUM.ARR (3) /= JOVIAL OR
                  N_ENUM.B1 /= TRUE OR
                  N_ENUM.B2 /= TRUE THEN
                    FAILED ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
                            " IN INSTANTIATION OF N_ENUM.");
               END IF;
          END;

          DECLARE

               PACKAGE N_INT IS NEW PKG (T => INTEGER, STR => "INTEGER",
                                          IDENT => IDENT_INT);
          BEGIN
               IF N_INT.ARR (1) /= IDENT_INT (INTEGER'FIRST) OR
                  N_INT.ARR (2) /= IDENT_INT (INTEGER'FIRST + 1) OR
                  N_INT.ARR (3) /= INTEGER'FIRST OR
                  N_INT.B1 /= TRUE OR
                  N_INT.B2 /= TRUE THEN
                    FAILED ("IMPROPER VALUES FOR ARRAY COMPONENTS" &
                            " IN INSTANTIATION OF N_INT.");
               END IF;
          END;
     END;
     RESULT;
END CC1220A;
