-- CC3007B.ADA

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
--  CHECK THAT THE NAMES IN A GENERIC INSTANTIATION ARE STATICALLY
--  IDENTIFIED (I.E., BOUND) AT THE TEXTUAL POINT OF THE INSTANTIA-
--  TION, AND ARE BOUND BEFORE BEING "SUBSTITUTED" FOR THE COR-
--  RESPONDING GENERIC FORMAL PARAMETERS IN THE SPECIFICATION AND
--  BODY TEMPLATES.
--
--  SEE AI-00365/05-BI-WJ.

-- HISTORY:
--      EDWARD V. BERARD, 15 AUGUST 1990
--      DAS   08 OCT 90   CHANGED INSTANTIATIONS TO USE VARIABLES
--                        M1 AND M2 IN THE FIRST_BLOCK INSTANTIA-
--                        TION AND TO ASSIGN THIRD_DATE AND
--                        FOURTH_DATE VALUES BEFORE AND AFTER THE
--                        SECOND_BLOCK INSTANTIATION.

WITH REPORT;

PROCEDURE CC3007B IS

     INCREMENTED_VALUE : NATURAL := 0;

     TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                         SEP, OCT, NOV, DEC);
     TYPE DAY_TYPE IS RANGE 1 .. 31;
     TYPE YEAR_TYPE IS RANGE 1904 .. 2050;
     TYPE DATE IS RECORD
          MONTH : MONTH_TYPE;
          DAY   : DAY_TYPE;
          YEAR  : YEAR_TYPE;
     END RECORD;

     TYPE DATE_ACCESS IS ACCESS DATE;

     TODAY           : DATE := (MONTH => AUG,
                                DAY   => 8,
                                YEAR  => 1990);

     CHRISTMAS       : DATE := (MONTH => DEC,
                                DAY   => 25,
                                YEAR  => 1948);

     WALL_DATE       : DATE := (MONTH => NOV,
                                DAY   => 9,
                                YEAR  => 1989);

     BIRTH_DATE     : DATE := (MONTH => OCT,
                               DAY   => 3,
                               YEAR  => 1949);

     FIRST_DUE_DATE : DATE := (MONTH => JAN,
                               DAY   => 23,
                               YEAR  => 1990);

     LAST_DUE_DATE  : DATE := (MONTH => DEC,
                               DAY   => 20,
                               YEAR  => 1990);

     THIS_MONTH    : MONTH_TYPE := AUG;

     STORED_RECORD : DATE := TODAY;

     STORED_INDEX  : MONTH_TYPE := AUG;

     FIRST_DATE   : DATE_ACCESS := NEW DATE'(WALL_DATE);
     SECOND_DATE  : DATE_ACCESS := FIRST_DATE;

     THIRD_DATE     : DATE_ACCESS := NEW DATE'(BIRTH_DATE);
     FOURTH_DATE  : DATE_ACCESS := NEW DATE'(CHRISTMAS);

     TYPE DUE_DATES IS ARRAY (MONTH_TYPE RANGE JAN .. DEC) OF DATE;
     REPORT_DATES : DUE_DATES := ((JAN, 23, 1990), (FEB, 23, 1990),
                                  (MAR, 23, 1990), (APR, 23, 1990),
                                  (MAY, 23, 1990), (JUN, 22, 1990),
                                  (JUL, 23, 1990), (AUG, 23, 1990),
                                  (SEP, 24, 1990), (OCT, 23, 1990),
                                  (NOV, 23, 1990), (DEC, 20, 1990));

     GENERIC

          NATURALLY     : IN NATURAL;
          FIRST_RECORD  : IN OUT DATE;
          SECOND_RECORD : IN OUT DATE;
          TYPE RECORD_POINTER IS ACCESS DATE;
          POINTER : IN OUT RECORD_POINTER;
          TYPE ARRAY_TYPE IS ARRAY (MONTH_TYPE) OF DATE;
          THIS_ARRAY           : IN OUT ARRAY_TYPE;
          FIRST_ARRAY_ELEMENT  : IN OUT DATE;
          SECOND_ARRAY_ELEMENT : IN OUT DATE;
          INDEX_ELEMENT        : IN OUT MONTH_TYPE;
          POINTER_TEST         : IN OUT DATE;
          ANOTHER_POINTER_TEST : IN OUT DATE;

     PACKAGE TEST_ACTUAL_PARAMETERS IS

          PROCEDURE EVALUATE_FUNCTION;
          PROCEDURE CHECK_RECORDS;
          PROCEDURE CHECK_ACCESS;
          PROCEDURE CHECK_ARRAY;
          PROCEDURE CHECK_ARRAY_ELEMENTS;
          PROCEDURE CHECK_SCALAR;
          PROCEDURE CHECK_POINTERS;

     END TEST_ACTUAL_PARAMETERS;

     PACKAGE BODY TEST_ACTUAL_PARAMETERS IS

          PROCEDURE EVALUATE_FUNCTION IS
          BEGIN  -- EVALUATE_FUNCTION

               IF (INCREMENTED_VALUE = 0) OR 
                  (NATURALLY /= INCREMENTED_VALUE) THEN
                    REPORT.FAILED ("PROBLEMS EVALUATING FUNCTION " &
                                   "PARAMETER.");
               END IF;

          END EVALUATE_FUNCTION;

          PROCEDURE CHECK_RECORDS IS

               STORE : DATE;

          BEGIN  -- CHECK_RECORDS

               IF STORED_RECORD /= FIRST_RECORD THEN
                    REPORT.FAILED ("PROBLEM WITH RECORD TYPES");
               ELSE
                    STORED_RECORD := SECOND_RECORD;
                    STORE := FIRST_RECORD;
                    FIRST_RECORD := SECOND_RECORD;
                    SECOND_RECORD := STORE;
               END IF;

          END CHECK_RECORDS;

          PROCEDURE CHECK_ACCESS IS
          BEGIN  -- CHECK_ACCESS

               IF ((INCREMENTED_VALUE / 2) * 2) /= INCREMENTED_VALUE
               THEN
                    IF POINTER.ALL /= DATE'(WALL_DATE) THEN
                         REPORT.FAILED ("PROBLEM WITH ACCESS TYPES " &
                                        "- 1");
                    ELSE
                         POINTER.ALL := DATE'(BIRTH_DATE);
                    END IF;
               ELSE
                    IF POINTER.ALL /= DATE'(BIRTH_DATE) THEN
                         REPORT.FAILED ("PROBLEM WITH ACCESS TYPES " &
                                        "- 2");
                    ELSE
                         POINTER.ALL := DATE'(WALL_DATE);
                    END IF;
               END IF;

          END CHECK_ACCESS;

          PROCEDURE CHECK_ARRAY IS

               STORE : DATE;

          BEGIN  -- CHECK_ARRAY

               IF ((INCREMENTED_VALUE / 2) * 2) /= INCREMENTED_VALUE
               THEN
                    IF THIS_ARRAY (THIS_ARRAY'FIRST) /= FIRST_DUE_DATE
                    THEN
                         REPORT.FAILED ("PROBLEM WITH ARRAY TYPES - 1");
                    ELSE
                         THIS_ARRAY (THIS_ARRAY'FIRST) := LAST_DUE_DATE;
                         THIS_ARRAY (THIS_ARRAY'LAST) := FIRST_DUE_DATE;
                    END IF;
               ELSE
                    IF THIS_ARRAY (THIS_ARRAY'FIRST) /= LAST_DUE_DATE
                    THEN
                         REPORT.FAILED ("PROBLEM WITH ARRAY TYPES - 2");
                    ELSE
                         THIS_ARRAY (THIS_ARRAY'FIRST) :=
                                                  FIRST_DUE_DATE;
                         THIS_ARRAY (THIS_ARRAY'LAST)  := LAST_DUE_DATE;
                    END IF;
               END IF;

          END CHECK_ARRAY;

          PROCEDURE CHECK_ARRAY_ELEMENTS IS

               STORE : DATE;

          BEGIN  -- CHECK_ARRAY_ELEMENTS

               IF ((INCREMENTED_VALUE / 2) * 2) /= INCREMENTED_VALUE
               THEN
                    IF (FIRST_ARRAY_ELEMENT.MONTH /= MAY) OR
                       (SECOND_ARRAY_ELEMENT.DAY /= 22) THEN
                         REPORT.FAILED ("PROBLEM WITH ARRAY ELEMENTS " &
                                        "- 1");
                    ELSE
                         STORE := FIRST_ARRAY_ELEMENT;
                         FIRST_ARRAY_ELEMENT := SECOND_ARRAY_ELEMENT;
                         SECOND_ARRAY_ELEMENT := STORE;
                    END IF;
               ELSE
                    IF (FIRST_ARRAY_ELEMENT.MONTH /= JUN) OR
                       (SECOND_ARRAY_ELEMENT.DAY /= 23) THEN
                         REPORT.FAILED ("PROBLEM WITH ARRAY ELEMENTS " &
                                        "- 2");
                    ELSE
                         STORE := FIRST_ARRAY_ELEMENT;
                         FIRST_ARRAY_ELEMENT := SECOND_ARRAY_ELEMENT;
                         SECOND_ARRAY_ELEMENT := STORE;
                    END IF;
               END IF;

          END CHECK_ARRAY_ELEMENTS;

          PROCEDURE CHECK_SCALAR IS
          BEGIN  -- CHECK_SCALAR

               IF ((INCREMENTED_VALUE / 2) * 2) /= INCREMENTED_VALUE
               THEN
                    IF INDEX_ELEMENT /= STORED_INDEX THEN
                         REPORT.FAILED ("PROBLEM WITH INDEX TYPES - 1");
                    ELSE
                         INDEX_ELEMENT :=
                                   MONTH_TYPE'SUCC(INDEX_ELEMENT);
                         STORED_INDEX := INDEX_ELEMENT;
                    END IF;
               ELSE
                    IF INDEX_ELEMENT /= STORED_INDEX THEN
                         REPORT.FAILED ("PROBLEM WITH INDEX TYPES - 2");
                    ELSE
                         INDEX_ELEMENT :=
                              MONTH_TYPE'PRED (INDEX_ELEMENT);
                         STORED_INDEX := INDEX_ELEMENT;
                    END IF;
               END IF;

          END CHECK_SCALAR;

          PROCEDURE CHECK_POINTERS IS

               STORE : DATE;

          BEGIN  -- CHECK_POINTERS

               IF ((INCREMENTED_VALUE / 2) * 2) /= INCREMENTED_VALUE
               THEN
                    IF (POINTER_TEST /= DATE'(OCT, 3, 1949)) OR
                       (ANOTHER_POINTER_TEST /= DATE'(DEC, 25, 1948))
                    THEN
                         REPORT.FAILED ("PROBLEM WITH POINTER TEST " &
                                        "- 1");
                    ELSE
                         STORE := POINTER_TEST;
                         POINTER_TEST := ANOTHER_POINTER_TEST;
                         ANOTHER_POINTER_TEST := STORE;
                    END IF;
               ELSE
                    IF (POINTER_TEST /= DATE'(DEC, 25, 1948)) OR
                       (ANOTHER_POINTER_TEST /= DATE'(OCT, 3, 1949))
                    THEN
                         REPORT.FAILED ("PROBLEM WITH POINTER TEST " &
                                        "- 2");
                    ELSE
                         STORE := POINTER_TEST;
                         POINTER_TEST := ANOTHER_POINTER_TEST;
                         ANOTHER_POINTER_TEST := STORE;
                    END IF;
               END IF;

          END CHECK_POINTERS;

     END TEST_ACTUAL_PARAMETERS;

     FUNCTION INC RETURN NATURAL IS
     BEGIN  -- INC
          INCREMENTED_VALUE := NATURAL'SUCC (INCREMENTED_VALUE);
          RETURN INCREMENTED_VALUE;
     END INC;

BEGIN  -- CC3007B

     REPORT.TEST ("CC3007B", "CHECK THAT THE NAMES IN A GENERIC " &
                  "INSTANTIATION ARE STAICALLY IDENTIFIED (I.E., " &
                  "BOUND) AT THE TEXTUAL POINT OF THE INSTANTIATION" &
                  ", AND ARE BOUND BEFORE BEING SUBSTITUTED FOR " &
                  "THE CORRESPONDING GENERIC FORMAL PARAMETERS IN " &
                  "THE SPECIFICATION AND BODY TEMPLATES.  " &
                  "SEE AI-00365/05-BI-WJ.");

     FIRST_BLOCK:

     DECLARE

          M1 : MONTH_TYPE := MAY;
          M2 : MONTH_TYPE := JUN;

          PACKAGE NEW_TEST_ACTUAL_PARAMETERS IS
               NEW TEST_ACTUAL_PARAMETERS (
                    NATURALLY              => INC,
                    FIRST_RECORD           => TODAY,
                    SECOND_RECORD          => CHRISTMAS,
                    RECORD_POINTER         => DATE_ACCESS,
                    POINTER                => SECOND_DATE,
                    ARRAY_TYPE             => DUE_DATES,
                    THIS_ARRAY             => REPORT_DATES,
                    FIRST_ARRAY_ELEMENT    => REPORT_DATES (M1),
                    SECOND_ARRAY_ELEMENT   => REPORT_DATES (M2),
                    INDEX_ELEMENT          => THIS_MONTH,
                    POINTER_TEST           => THIRD_DATE.ALL,
                    ANOTHER_POINTER_TEST   => FOURTH_DATE.ALL);

     BEGIN  -- FIRST_BLOCK

          REPORT.COMMENT ("ENTERING FIRST BLOCK");
          NEW_TEST_ACTUAL_PARAMETERS.EVALUATE_FUNCTION;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_SCALAR;
          M1 := SEP;
          M2 := OCT;
          -- NEW_TEST_ACTUAL_PARAMETERS SHOULD USE THE PREVIOUS
          -- VALUES OF MAY AND JUN.
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ARRAY;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ARRAY_ELEMENTS;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ACCESS;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_RECORDS;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_POINTERS;

     END FIRST_BLOCK;

     SECOND_BLOCK:

     DECLARE

          SAVE_THIRD_DATE  : DATE_ACCESS := THIRD_DATE;
          SAVE_FOURTH_DATE : DATE_ACCESS := FOURTH_DATE;

          PACKAGE NEW_TEST_ACTUAL_PARAMETERS IS
               NEW TEST_ACTUAL_PARAMETERS (
                    NATURALLY              => INC,
                    FIRST_RECORD           => TODAY,
                    SECOND_RECORD          => CHRISTMAS,
                    RECORD_POINTER         => DATE_ACCESS,
                    POINTER                => SECOND_DATE,
                    ARRAY_TYPE             => DUE_DATES,
                    THIS_ARRAY             => REPORT_DATES,
                    FIRST_ARRAY_ELEMENT    => REPORT_DATES (MAY),
                    SECOND_ARRAY_ELEMENT   => REPORT_DATES (JUN),
                    INDEX_ELEMENT          => THIS_MONTH,
                    POINTER_TEST           => THIRD_DATE.ALL,
                    ANOTHER_POINTER_TEST   => FOURTH_DATE.ALL);

     BEGIN  -- SECOND_BLOCK

          REPORT.COMMENT ("ENTERING SECOND BLOCK");
          NEW_TEST_ACTUAL_PARAMETERS.EVALUATE_FUNCTION;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_SCALAR;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ARRAY;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ARRAY_ELEMENTS;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_ACCESS;
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_RECORDS;

          THIRD_DATE := NEW DATE'(JUL, 13, 1951);
          FOURTH_DATE := NEW DATE'(JUL, 4, 1976);
          NEW_TEST_ACTUAL_PARAMETERS.CHECK_POINTERS;
          THIRD_DATE := SAVE_THIRD_DATE;
          FOURTH_DATE := SAVE_FOURTH_DATE;

     END SECOND_BLOCK;

     REPORT.RESULT;

END CC3007B;
