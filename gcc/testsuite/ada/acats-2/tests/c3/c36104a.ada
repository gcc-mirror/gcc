-- C36104A.ADA

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
-- AND INDEX CONSTRAINTS IN OBJECT AND TYPE DECLARATIONS,
-- WHERE AN EXPLICIT (SUB)TYPE IS INCLUDED IN EACH DISCRETE_RANGE.
-- MEMBERSHIP OPERATORS ARE CHECKED HERE, ALSO, TO ENSURE THAT
-- EXCEPTIONS ARE NOT RAISED FOR NULL RANGES.
-- ONLY STATIC CASES ARE CHECKED HERE.

-- DAT 2/3/81
-- JRK 2/25/81
-- VKG 1/21/83
-- L.BROWN  7/15/86  1) ADDED ACCESS TYPES.
--                   2) DELETED "NULL INDEX RANGES, CONSTRAINT_ERROR 
--                      RAISED" SECTION.
--                   3) DELETED ANY MENTION OF CASE STATEMENT CHOICES
--                      AND VARIANT CHOICES IN THE ABOVE COMMENT.
-- EDS      7/16/98  AVOID OPTIMIZATION

WITH REPORT;
PROCEDURE C36104A IS

     USE REPORT;

     TYPE WEEK IS (SUN, MON, TUE, WED, THU, FRI, SAT);
     TYPE WEEK_ARRAY IS ARRAY (WEEK RANGE <>) OF WEEK;
     SUBTYPE WORK_WEEK IS WEEK RANGE MON .. FRI;
     SUBTYPE MID_WEEK IS WORK_WEEK RANGE TUE .. THU;

     TYPE INT_10 IS NEW INTEGER RANGE -10 .. 10;
     TYPE I_10 IS NEW INT_10;
     SUBTYPE I_5 IS I_10 RANGE -5 .. 5;
     TYPE I_5_ARRAY IS ARRAY (I_5 RANGE <>) OF I_5;

BEGIN
     TEST ("C36104A", "CONSTRAINT_ERROR IS RAISED OR NOT IN STATIC "
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
                  A1 : A := (OTHERS => I_5(IDENT_INT(1)));
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
               FAILED ("CONSTRAINT_ERROR NOT RAISED 3");
          END LOOP;
          FAILED ("CONSTRAINT_ERROR NOT RAISED 3");
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 3");
     END;

     BEGIN
          DECLARE
               TYPE P IS ACCESS I_5_ARRAY (I_5 RANGE 0 .. 6);
               -- ABOVE DECLARATION RAISES CONSTRAINT_ERROR.
          BEGIN
             DECLARE
                TYPE PA IS NEW P;
                -- DEFINE AN OBJECT OF TYPE PA AND USE IT TO AVOID 
                -- OPTIMIZATION OF TYPE
                PA1 : PA := NEW I_5_ARRAY'(0 .. I_5(IDENT_INT(6)) =>
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
          FAILED ("CONSTRAINT_ERROR NOT RAISED 7 " & 
                  MID_WEEK'IMAGE(W(WED))); --USE W
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
          FAILED ("CONSTRAINT_ERROR NOT RAISED 8 " & 
                  MID_WEEK'IMAGE(W(WED))); --USE W
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
               W := (W'RANGE => WED); -- OK.
               FAILED ("CONSTRAINT_ERROR NOT RAISED 9 " & 
                       MID_WEEK'IMAGE(W(WED))); --USE W
          EXCEPTION
               WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 9");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 9");
     END;

     BEGIN
          DECLARE
               TYPE W IS NEW WEEK_ARRAY (MID_WEEK RANGE SUN .. TUE);
               -- RAISES CONSTRAINT_ERROR.
          BEGIN
             DECLARE
                W1 : W := (OTHERS => WED);
             BEGIN
                FAILED ("CONSTRAINT_ERROR NOT RAISED 10 " & 
                        MID_WEEK'IMAGE(W1(WED))); --USE W1
             END;
          EXCEPTION
             WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 10");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 10");
     END;

     BEGIN
          DECLARE
               SUBTYPE W IS WEEK_ARRAY (MID_WEEK RANGE MON .. WED);
               -- RAISES CONSTRAINT_ERROR.
          BEGIN
               DECLARE
                    W1 : W := (OTHERS => (WED));
               BEGIN
                    FAILED ("CONSTRAINT_ERROR NOT RAISED 8 " & 
                            MID_WEEK'IMAGE(W1(WED))); --USE W1
               END;
          EXCEPTION
               WHEN OTHERS => FAILED ("UNHANDLED EXCEPTION RAISED 8");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED 11");
     END;

     -- NULL DISCRETE/INDEX RANGES, EXCEPTION NOT RAISED.

     BEGIN
          DECLARE
               TYPE A IS ARRAY (I_5 RANGE -5 .. -6) OF I_5;
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
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN MID_WEEK RANGE FRI .. WED LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN MID_WEEK RANGE MON .. SUN LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN I_5 RANGE 10 .. -10 LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN I_5 RANGE 10 .. 9 LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN I_5 RANGE -10 .. -11 LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN I_5 RANGE -10 .. -20 LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
          FOR I IN I_5 RANGE 6 .. 5 LOOP
               FAILED("LOOP WAS EXECUTED WITH NULL DISCRETE/INDEX RANGES");
          END LOOP;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 3");
     END;

     BEGIN
          DECLARE
               TYPE P IS ACCESS I_5_ARRAY (-5 .. -6);
               PA1 : P := NEW I_5_ARRAY (-5 .. -6);
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
               IF (W'FIRST /= MON) THEN
                    FAILED ("'FIRST OF NULL ARRAY INCORRECT");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 9");
     END;

     BEGIN
          DECLARE
               TYPE W IS NEW WEEK_ARRAY (MID_WEEK RANGE TUE .. MON);
               W1 : W;
          BEGIN
               IF (W1'FIRST /= TUE) THEN
                    FAILED ("'FIRST OF NULL ARRAY INCORRECT");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 10");
     END;

     BEGIN
          DECLARE
               SUBTYPE W IS WEEK_ARRAY (MID_WEEK RANGE TUE .. MON);
               W1 : W;
          BEGIN
               IF (W1'FIRST /= TUE) THEN
                    FAILED ("'FIRST OF NULL ARRAY INCORRECT");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 12");
     END;

     -- NULL MEMBERSHIP RANGES, EXCEPTION NOT RAISED.

     BEGIN
          IF SUN IN  SAT .. SUN
          OR SAT IN  FRI .. WED
          OR WED IN  THU .. TUE
          OR THU IN  MON .. SUN
          OR FRI IN  SAT .. FRI
          OR WED IN  FRI .. MON
          THEN
               FAILED ("INCORRECT 'IN' EVALUATION 1");
          END IF;

          IF INTEGER'(0) IN  10 .. -10
          OR INTEGER'(0) IN  10 .. 9
          OR INTEGER'(0) IN  -10 .. -11
          OR INTEGER'(0) IN  -10 .. -20 
          OR INTEGER'(0) IN  6 .. 5
          OR INTEGER'(0) IN  5 .. 3
          OR INTEGER'(0) IN  7 .. 3
          THEN
               FAILED ("INCORRECT 'IN' EVALUATION 2");
          END IF;

          IF WED NOT IN  THU .. TUE
          AND INTEGER'(0) NOT IN  4 .. -4
          THEN NULL;
          ELSE FAILED ("INCORRECT 'NOT IN' EVALUATION");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED ("EXCEPTION RAISED 52");
     END;


     RESULT;
END C36104A;
