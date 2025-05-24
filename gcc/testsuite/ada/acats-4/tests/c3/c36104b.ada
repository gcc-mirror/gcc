-- C36104B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED OR NOT, AS APPROPRIATE,
-- DURING DISCRETE_RANGE ELABORATIONS/EVALUATIONS IN LOOPS,
-- ARRAY_TYPE_DEFINITIONS, ARRAY AGGREGATES, SLICES,
-- AND INDEX CONSTRAINTS IN OBJECT AND TYPE DECLARATIONS, WHERE
-- AN EXPLICIT (SUB)TYPE IS INCLUDED IN EACH DISCRETE_RANGE.
-- MEMBERSHIP OPERATORS ARE CHECKED HERE, ALSO, TO ENSURE THAT
-- EXCEPTIONS ARE NOT RAISED FOR NULL RANGES.
-- ONLY DYNAMIC CASES ARE CHECKED HERE.

-- DAT 2/3/81
-- JRK 2/25/81
-- L.BROWN  7/15/86  1) ADDED ACCESS TYPES.
--                   2) DELETED "NULL INDEX RANGE, CONSTRAINT_ERROR
--                      RAISED" SECTION.
--                   3) MADE USE OF DYNAMIC-RESULT FUNCTIONS.
--                   4) DELETED ALL REFERENCES TO CASE STATEMENT CHOICES
--                      AND VARIANT PART CHOICES IN THE ABOVE COMMENT.
-- EDS      7/16/98  AVOID OPTIMIZATION

WITH REPORT;
PROCEDURE C36104B IS

     USE REPORT;

     TYPE WEEK IS (SSUN, SMON, STUE, SWED, STHU, SFRI, SSAT);
     SUN : WEEK := WEEK'VAL(IDENT_INT(0));
     MON : WEEK := WEEK'VAL(IDENT_INT(1));
     TUE : WEEK := WEEK'VAL(IDENT_INT(2));
     WED : WEEK := WEEK'VAL(IDENT_INT(3));
     THU : WEEK := WEEK'VAL(IDENT_INT(4));
     FRI : WEEK := WEEK'VAL(IDENT_INT(5));
     SAT : WEEK := WEEK'VAL(IDENT_INT(6));
     TYPE WEEK_ARRAY IS ARRAY (WEEK RANGE <>) OF WEEK;
     SUBTYPE WORK_WEEK IS WEEK RANGE MON .. FRI;
     SUBTYPE MID_WEEK IS WORK_WEEK RANGE TUE .. THU;

     TYPE INT_10 IS NEW INTEGER RANGE -10 .. 10;
     TYPE I_10 IS NEW INT_10;
     SUBTYPE I_5 IS I_10 RANGE I_10(IDENT_INT(-5)) ..
                               I_10(IDENT_INT(5));
     TYPE I_5_ARRAY IS ARRAY (I_5 RANGE <>) OF I_5;

     FUNCTION F(DAY : WEEK) RETURN WEEK IS
        BEGIN
          RETURN DAY;
        END;

BEGIN
     TEST ("C36104B", "CONSTRAINT_ERROR IS RAISED OR NOT IN DYNAMIC "
          & "DISCRETE_RANGES WITH EXPLICIT TYPE_MARKS");

     -- NON-NULL RANGES, CONSTRAINT_ERROR RAISED.

     BEGIN
          DECLARE
               TYPE A IS ARRAY (I_5 RANGE 0 .. 6) OF I_5;
               -- ABOVE DECLARATION RAISES CONSTRAINT_ERROR.
          BEGIN
               DECLARE
                  -- DEFINE AN OBJECT OF TYPE A AND USE IT TO AVOID 
                  -- OPTIMIZATION OF SUBTYPE
                  A1 : A := (A'RANGE => I_5(IDENT_INT(1)));
               BEGIN
                  FAILED ("CONSTRAINT_ERROR NOT RAISED 1 " & 
                          I_5'IMAGE(A1(1)) );  --USE A1
               END;
          EXCEPTION
             --MAKE SURE THAT CONSTRAINT_ERROR FROM ABOVE STATEMENTS
             --REPORT FAILED.
             WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 1");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 1");
     END;

     BEGIN
          FOR I IN MID_WEEK RANGE MON .. MON LOOP

               IF EQUAL(2,2)  THEN
                    SAT := SSAT;
               END IF;

          END LOOP;
          FAILED ("CONSTRAINT_ERROR NOT RAISED 3");
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 3");
     END;

     BEGIN
          DECLARE
               TYPE P IS ACCESS I_5_ARRAY (0 .. 6);
               -- ABOVE DECLARATION RAISES CONSTRAINT_ERROR.
          BEGIN
               DECLARE
                  TYPE PA IS NEW P;
                  -- DEFINE AN OBJECT OF TYPE PA AND USE IT TO AVOID 
                  -- OPTIMIZATION OF TYPE
                  PA1 : PA :=NEW I_5_ARRAY'(0.. I_5(IDENT_INT(6)) => 
                                            I_5(IDENT_INT(1)));
               BEGIN
                  FAILED ("CONSTRAINT_ERROR NOT RAISED 4 " & 
                          I_5'IMAGE(PA1(1))); --USE PA1
               END;
          EXCEPTION
             WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 4");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 4");
     END;

     DECLARE
          W : WEEK_ARRAY (MID_WEEK);
     BEGIN
          W := (MID_WEEK RANGE MON .. WED => WED);
          -- CONSTRAINT_ERROR RAISED.
          BEGIN
               FAILED ("CONSTRAINT_ERROR NOT RAISED 7 " & 
                       MID_WEEK'IMAGE(W(WED))); --USE W
          EXCEPTION
               WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 7");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 7");
     END;

     DECLARE
          W : WEEK_ARRAY (WORK_WEEK);
     BEGIN
          W := (W'RANGE => WED); -- OK.
          W (MON .. WED) := W (MID_WEEK RANGE MON .. WED); -- EXCEPTION.
          BEGIN
               FAILED ("CONSTRAINT_ERROR NOT RAISED 8 " & 
                       MID_WEEK'IMAGE(W(WED))); --USE W
          EXCEPTION
               WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 8");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 8");
     END;

     BEGIN
          DECLARE
               W : WEEK_ARRAY (MID_WEEK RANGE MON .. FRI);
               -- ELABORATION OF ABOVE RAISES CONSTRAINT_ERROR.
          BEGIN
               W(WED) := THU;        -- OK.
               FAILED ("CONSTRAINT_ERROR NOT RAISED 9 " &
                       WEEK'IMAGE(W(WED)));   -- USE W
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 9");
     END;

     BEGIN
          DECLARE
               TYPE W IS NEW WEEK_ARRAY (MID_WEEK RANGE SUN .. WED);
               -- RAISES CONSTRAINT_ERROR.
          BEGIN
               DECLARE
                    X : W;              -- OK.
               BEGIN
                    X(TUE) := THU;   -- OK.
                    FAILED ("CONSTRAINT_ERROR NOT RAISED 10 " &
                            WEEK'IMAGE(X(TUE)));   -- USE X
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 10");
     END;

     BEGIN
          DECLARE
               SUBTYPE W IS WEEK_ARRAY (MID_WEEK RANGE MON .. THU);
               -- RAISES CONSTRAINT_ERROR.
          BEGIN
               DECLARE
                    T : W;               -- OK.
               BEGIN
                    T(TUE) := THU;    -- OK.
                    FAILED ("CONSTRAINT_ERROR NOT RAISED 11 " &
                            WEEK'IMAGE(T(TUE)));
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("DID NOT RAISE CONSTRAINT_ERROR AT PROPER PLACE");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 11");
     END;

     -- NULL DISCRETE/INDEX RANGES, EXCEPTION NOT RAISED.

     BEGIN
          DECLARE
               TYPE A IS ARRAY (I_5 RANGE I_5(IDENT_INT(-5)) .. -6) OF I_5;
               A1 : A;
          BEGIN
               IF A1'FIRST /= I_5(IDENT_INT(-5)) THEN
                    FAILED ("'FIRST OF NULL ARRAY INCORRECT");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 1");
     END;

     BEGIN
          FOR I IN MID_WEEK RANGE SAT .. SUN LOOP
               
               IF EQUAL(2,2)  THEN
                    TUE := STUE;
               END IF;

          END LOOP;
          FOR I IN MID_WEEK RANGE FRI .. WED LOOP
               
               IF EQUAL(2,2)  THEN
                    MON := SMON;
               END IF;

          END LOOP;
          FOR I IN MID_WEEK RANGE MON .. SUN LOOP
               
               IF EQUAL(3,3)  THEN
                    WED := SWED;
               END IF;

          END LOOP;
          FOR I IN I_5 RANGE 10 .. -10 LOOP

               IF EQUAL(2,2)  THEN
                    TUE := STUE;
               END IF;

          END LOOP;
          FOR I IN I_5 RANGE 10 .. 9 LOOP

               IF EQUAL(2,2)  THEN
                    THU := STHU;
               END IF;

          END LOOP;
          FOR I IN I_5 RANGE -10 .. -11 LOOP

               IF EQUAL(2,2)  THEN
                    SAT := SSAT;
               END IF;

          END LOOP;
          FOR I IN I_5 RANGE -10 .. -20 LOOP

               IF EQUAL(2,2)  THEN
                    SUN := SSUN;
               END IF;

          END LOOP;
          FOR I IN I_5 RANGE 6 .. 5 LOOP

               IF EQUAL(2,2)  THEN
                    MON := SMON;
               END IF;

          END LOOP;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 3");
     END;

     BEGIN
          DECLARE
               TYPE P IS ACCESS I_5_ARRAY (I_5(IDENT_INT(-5)) .. -6);
               PA1 : P := NEW I_5_ARRAY (I_5(IDENT_INT(-5)) .. -6);
          BEGIN
               IF PA1'LENGTH /= IDENT_INT(0) THEN
                    FAILED ("'LENGTH OF NULL ARRAY INCORRECT");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED 5");
     END;

     DECLARE
          TYPE NARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
          SUBTYPE SNARR IS INTEGER RANGE 1 .. 2;
          W : NARR(SNARR) := (1,2);
     BEGIN
          IF W = (SNARR RANGE IDENT_INT(4) .. 2 => 5) THEN
               FAILED("EVALUATION OF EXPRESSION IS INCORRECT");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 7");
     END;

     DECLARE
          W : WEEK_ARRAY (MID_WEEK);
     BEGIN
          W := (W'RANGE => WED); -- OK.
          W (TUE .. MON) := W (MID_WEEK RANGE MON .. SUN);
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 8");
     END;

     BEGIN
          DECLARE
               W : WEEK_ARRAY (MID_WEEK RANGE MON .. SUN);
          BEGIN

               IF EQUAL(W'LENGTH,0)  THEN
                    TUE := STUE;
               END IF;

          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 9");
     END;

     BEGIN
          DECLARE
               TYPE W IS NEW WEEK_ARRAY (MID_WEEK RANGE TUE .. MON);
          BEGIN

               IF EQUAL(W'LENGTH,0)  THEN
                    MON := SMON;
               END IF;

          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 10");
     END;

     BEGIN
          DECLARE
               SUBTYPE W IS WEEK_ARRAY (MID_WEEK RANGE TUE .. MON);
          BEGIN

               IF EQUAL(W'LENGTH,0)  THEN
                    WED := SWED;
               END IF;

          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 12");
     END;

     -- NULL MEMBERSHIP RANGES, EXCEPTION NOT RAISED.

     BEGIN
          IF F(SUN) IN  SAT .. SUN
          OR SAT IN  FRI .. WED
          OR F(WED) IN  THU .. TUE
          OR THU IN  MON .. SUN
          OR F(FRI) IN  SAT .. FRI
          OR WED IN  FRI .. MON
          THEN
               FAILED ("INCORRECT 'IN' EVALUATION 1");
          END IF;

          IF IDENT_INT(0) IN  10 .. IDENT_INT(-10)
          OR 0 IN  IDENT_INT(10) .. 9
          OR IDENT_INT(0) IN  IDENT_INT(-10) .. -11
          OR 0 IN  -10 .. IDENT_INT(-20) 
          OR IDENT_INT(0) IN  6 .. IDENT_INT(5)
          OR 0 IN  5 .. IDENT_INT(3)
          OR IDENT_INT(0) IN  7 .. IDENT_INT(3)
          THEN
               FAILED ("INCORRECT 'IN' EVALUATION 2");
          END IF;

          IF F(WED) NOT IN  THU .. TUE
          AND IDENT_INT(0) NOT IN  IDENT_INT(4) .. -4
          THEN NULL;
          ELSE FAILED ("INCORRECT 'NOT IN' EVALUATION");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 52");
     END;

     RESULT;
END C36104B;
