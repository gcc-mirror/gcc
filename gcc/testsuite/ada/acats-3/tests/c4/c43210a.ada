-- C43210A.ADA

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
-- CHECK THAT A NON-AGGREGATE EXPRESSION IN A NAMED COMPONENT
-- ASSOCIATION IS EVALUATED ONCE FOR EACH COMPONENT SPECIFIED
-- BY THE ASSOCIATION.

-- EG  02/02/84

WITH REPORT;

PROCEDURE C43210A IS

     USE REPORT;

BEGIN

     TEST("C43210A", "CHECK THAT A NON-AGGREGATE IN A NAMED "   &
                     "COMPONENT ASSOCIATION IS EVALUATED ONCE " &
                     "FOR EACH COMPONENT SPECIFIED BY THE "     &
                     "ASSOCIATION");

     DECLARE

          TYPE T1 IS ARRAY(1 .. 10) OF INTEGER;
          TYPE T2 IS ARRAY(1 .. 8, 1 .. 2) OF INTEGER;
          TYPE T3 IS ARRAY(1 .. 2, 1 .. 8) OF INTEGER;
          TYPE T4 IS ARRAY(1 .. 8, 1 .. 8) OF INTEGER;

          A1 : T1;
          A2 : T2;
          A3 : T3;
          A4 : T4;
          CC : INTEGER;

          FUNCTION CALC (A : INTEGER) RETURN INTEGER IS
          BEGIN
               CC := CC + 1;
               RETURN IDENT_INT(A);
          END CALC;

          PROCEDURE CHECK (A : STRING; B : INTEGER) IS
          BEGIN
               IF CC /= B THEN
                    FAILED ("CASE " & A & " : INCORRECT NUMBER OF " &
                            "EVALUATIONS. NUMBER OF EVALUATIONS "   &
                            "SHOULD BE " & INTEGER'IMAGE(B) & 
                            ", BUT IS " & INTEGER'IMAGE(CC));
               END IF;
          END CHECK;

     BEGIN

CASE_A :  BEGIN

               CC := 0;
               A1 := T1'(4 .. 5 => CALC(2), 6 .. 8 => CALC(4),
                         OTHERS => 5);
               CHECK ("A", 5);

          END CASE_A;

CASE_B :  BEGIN

               CC := 0;
               A1 := T1'(1 | 4 .. 6 | 3 | 2 => CALC(-1), OTHERS => -2);
               CHECK ("B", 6);

          END CASE_B;

CASE_C :  BEGIN

               CC := 0;
               A1 := T1'(1 | 3 | 5 | 7 .. 9 => -1, OTHERS => CALC(-2));
               CHECK ("C", 4);

          END CASE_C;

CASE_D :  BEGIN

               CC := 0;
               A2 := T2'(4 .. 6 | 8 | 2 .. 3 => (1 .. 2 => CALC(1)),
                         OTHERS => (1 .. 2 => -1));
               CHECK ("D", 12);

          END CASE_D;

CASE_E : BEGIN

               CC := 0;
               A3 := T3'(1 .. 2 => (2 | 4 | 6 .. 8 => CALC(-1),
                                    OTHERS => -2));
               CHECK ("E", 10);

          END CASE_E;

CASE_F :  BEGIN

               CC := 0;
               A4 := T4'(7 .. 8 | 3 .. 5 => 
                          (1 | 2 | 4 | 6 .. 8 => CALC(1), OTHERS => -2),
                         OTHERS => (OTHERS => -2));
               CHECK ("F", 30);

          END CASE_F;

CASE_G :  BEGIN

               CC := 0;
               A4 := T4'(5 .. 8 | 3 | 1 => (7 | 1 .. 5 | 8 => -1,
                                            OTHERS => CALC(-2)),
                         OTHERS => (OTHERS => CALC(-2)));
               CHECK ("G", 22);

          END CASE_G;

     END;

     RESULT;

END C43210A;
