-- C43208B.ADA

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
-- FOR AN AGGREGATE OF THE FORM:
--        (B..C => (D..E => (F..G => (H..I => J))))
-- WHOSE TYPE IS A TWO-DIMENSIONAL ARRAY TYPE THAT HAS A TWO-
-- DIMENSIONAL ARRAY COMPONENT TYPE, CHECK THAT:

--     A) IF B..C OR D..E IS A NULL RANGE, THEN F, G, H, I, AND J
--        ARE NOT EVALUATED.

--     B) IF B..C AND D..E ARE NON-NULL RANGES, THEN F, G, H AND I
--        ARE EVALUATED (C-B+1)*(E-D+1) TIMES, AND J IS EVALUATED
--        (C-B+1)*(E-D+1)*(G-F+1)*(I-H+1) TIMES IF F..G AND H..I
--        ARE NON-NULL.

-- EG  01/19/84

WITH REPORT;

PROCEDURE C43208B IS

     USE REPORT;

BEGIN

     TEST("C43208B", "CHECK THAT THE EVALUATION OF A MULTI" &
                     "DIMENSIONAL ARRAY TYPE THAT HAS AN "  &
                     "ARRAY COMPONENT TYPE IS PERFORMED "   &
                     "CORRECTLY");

     DECLARE

          TYPE CHOICE_INDEX IS (B, C, D, E, F, G, H, I, J);
          TYPE CHOICE_CNTR  IS ARRAY(CHOICE_INDEX) OF INTEGER;

          CNTR : CHOICE_CNTR := (CHOICE_INDEX => 0);

          TYPE T1 IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>)
                          OF INTEGER;
   
          FUNCTION CALC (A : CHOICE_INDEX; B : INTEGER)
                         RETURN INTEGER IS
          BEGIN
               CNTR(A) := CNTR(A) + 1;
               RETURN IDENT_INT(B);
          END CALC;

     BEGIN

CASE_A :  BEGIN

     CASE_A1 : DECLARE
                    A1 : ARRAY(4 .. 3, 3 .. 4) OF T1(2 .. 3, 1 .. 2);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    A1 := (4 .. 3 => (3 .. 4 => 
                            (CALC(F,2) .. CALC(G,3) =>
                              (CALC(H,1) .. CALC(I,2) => CALC(J,2)))));
                    IF CNTR(F) /= 0 THEN
                         FAILED("CASE A1 : F WAS EVALUATED");
                    END IF;
                    IF CNTR(G) /= 0 THEN
                         FAILED("CASE A1 : G WAS EVALUATED");
                    END IF;
                    IF CNTR(H) /= 0 THEN
                         FAILED("CASE A1 : H WAS EVALUATED");
                    END IF;
                    IF CNTR(I) /= 0 THEN
                         FAILED("CASE A1 : I WAS EVALUATED");
                    END IF;
                    IF CNTR(J) /= 0 THEN
                         FAILED("CASE A1 : J WAS EVALUATED");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE A1 : EXCEPTION RAISED");
               END CASE_A1;

     CASE_A2 : DECLARE
                    A2 : ARRAY(3 .. 4, 4 .. 3) OF T1(2 .. 3, 1 .. 2);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    A2 := (CALC(B,3) .. CALC(C,4) =>
                           (CALC(D,4) .. CALC(E,3) =>
                            (CALC(F,2) .. CALC(G,3) =>
                             (CALC(H,1) .. CALC(I,2) => CALC(J,2)))));
                    IF CNTR(F) /= 0 THEN
                         FAILED("CASE A2 : F WAS EVALUATED");
                    END IF;
                    IF CNTR(G) /= 0 THEN
                         FAILED("CASE A2 : G WAS EVALUATED");
                    END IF;
                    IF CNTR(H) /= 0 THEN
                         FAILED("CASE A2 : H WAS EVALUATED");
                    END IF;
                    IF CNTR(I) /= 0 THEN
                         FAILED("CASE A2 : I WAS EVALUATED");
                    END IF;
                    IF CNTR(J) /= 0 THEN
                         FAILED("CASE A2 : J WAS EVALUATED");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE A2 : EXCEPTION RAISED");
               END CASE_A2;

          END CASE_A;

CASE_B :  BEGIN

     CASE_B1 : DECLARE
                    B1 : ARRAY(2 .. 3, 1 .. 2) OF T1(1 .. 2, 9 .. 10);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    B1 := (2 .. 3 => (1 .. 2 =>
                            (CALC(F,1) .. CALC(G,2) =>
                              (CALC(H,9) .. CALC(I,10) => CALC(J,2)))));
                    IF CNTR(F) /= 4 THEN
                         FAILED("CASE B1 : F NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(G) /= 4 THEN
                         FAILED("CASE B1 : G NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(H) /= 4 THEN
                         FAILED("CASE B1 : H NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(I) /= 4 THEN
                         FAILED("CASE B1 : I NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(J) /= 16 THEN
                         FAILED("CASE B1 : J NOT EVALUATED (C-B+1)*" &
                                "(E-D+1)*(G-F+1)*(I-H+1) TIMES");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE B1 : EXECEPTION RAISED");
               END CASE_B1;

     CASE_B2 : DECLARE
                    B2 : ARRAY(2 .. 3, 1 .. 2) OF T1(1 .. 2, 9 .. 10);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    B2 := (CALC(B,2) .. CALC(C,3) =>
                           (CALC(D,1) .. CALC(E,2) =>
                            (CALC(F,1) .. CALC(G,2) =>
                             (CALC(H,9) .. CALC(I,10) => CALC(J,2)))));
                    IF CNTR(F) /= 4 THEN
                         FAILED("CASE B2 : F NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(G) /= 4 THEN
                         FAILED("CASE B2 : G NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(H) /= 4 THEN
                         FAILED("CASE B2 : H NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(I) /= 4 THEN
                         FAILED("CASE B2 : I NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(J) /= 16 THEN
                         FAILED("CASE B2 : J NOT EVALUATED (C-B+1)*" &
                                "(E-D+1)*(G-F+1)*(I-H+1) TIMES");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE B2 : EXECEPTION RAISED");
               END CASE_B2;

     CASE_B3 : DECLARE
                    B3 : ARRAY(2 .. 3, 1 .. 2) OF T1(1 .. 2, 2 .. 1);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    B3 := (2 .. 3 => (1 .. 2 =>
                            (CALC(F,1) .. CALC(G,2) =>
                              (CALC(H,2) .. CALC(I,1) => CALC(J,2)))));
                    IF CNTR(F) /= 4 THEN
                         FAILED("CASE B3 : F NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(G) /= 4 THEN
                         FAILED("CASE B3 : G NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(H) /= 4 THEN
                         FAILED("CASE B3 : H NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(I) /= 4 THEN
                         FAILED("CASE B3 : I NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(J) /= 0 THEN
                         FAILED("CASE B3 : J NOT EVALUATED ZERO TIMES");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE B3 : EXECEPTION RAISED");
               END CASE_B3;

     CASE_B4 : DECLARE
                    B4 : ARRAY(2 .. 3, 1 .. 2) OF T1(2 .. 1, 1 .. 2);
               BEGIN
                    CNTR := (CHOICE_INDEX => 0);
                    B4 := (CALC(B,2) .. CALC(C,3) =>
                           (CALC(D,1) .. CALC(E,2) =>
                            (CALC(F,2) .. CALC(G,1) =>
                             (CALC(H,1) .. CALC(I,2) => CALC(J,2)))));
                    IF CNTR(F) /= 4 THEN
                         FAILED("CASE B4 : F NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(G) /= 4 THEN
                         FAILED("CASE B4 : G NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(H) /= 4 THEN
                         FAILED("CASE B4 : H NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(I) /= 4 THEN
                         FAILED("CASE B4 : I NOT EVALUATED (C-B+1)*" &
                                "(E-D+1) TIMES");
                    END IF;
                    IF CNTR(J) /= 0 THEN
                         FAILED("CASE B4 : J NOT EVALUATED ZERO TIMES");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED("CASE B4 : EXECEPTION RAISED");
               END CASE_B4;

          END CASE_B;
      END;

     RESULT;

END C43208B;
