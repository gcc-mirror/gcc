-- C43206A.ADA

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
-- CHECK THAT THE BOUNDS OF A NULL ARRAY AGGREGATE ARE DETERMINED
-- BY THE BOUNDS SPECIFIED BY THE CHOICES. IN PARTICULAR, CHECK
-- THAT:

--   A) THE UPPER BOUND IS NOT REQUIRED TO BE THE PREDECESSOR OF
--      THE LOWER BOUND.

--   B) NEITHER THE UPPER NOR THE LOWER BOUND NEED BELONG TO THE
--      INDEX SUBTYPE FOR NULL RANGES.

--   C) IF ONE CHOICE OF A MULTIDIMENSIONAL AGGREGATE IS NON-NULL
--      BUT THE AGGREGATE IS A NULL ARRAY, CONSTRAINT_ERROR IS 
--      RAISED WHEN THE NON-NULL CHOICES DO NOT BELONG TO THE
--      INDEX SUBTYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- EG  02/02/84
-- JBG 12/6/84
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT;

PROCEDURE C43206A IS

     USE REPORT;

BEGIN

     TEST("C43206A", "CHECK THAT THE BOUNDS OF A NULL ARRAY ARE " &
                     "DETERMINED BY THE BOUNDS SPECIFIED BY THE " &
                     "CHOICES");

     DECLARE

          SUBTYPE ST1 IS INTEGER RANGE 10 .. 15;
          SUBTYPE ST2 IS INTEGER RANGE 1 .. 5;

          TYPE T1 IS ARRAY (ST1 RANGE <>) OF INTEGER;
          TYPE T2 IS ARRAY (ST2 RANGE <>, ST1 RANGE <>) OF INTEGER;

     BEGIN

CASE_A :  BEGIN

     CASE_A1 : DECLARE

                    PROCEDURE PROC1 (A : T1) IS
                    BEGIN
                         IF A'FIRST /= 12 OR A'LAST /= 10 THEN
                              FAILED ("CASE A1 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC1((12 .. 10 => -2));

               EXCEPTION

                    WHEN OTHERS =>
                         FAILED ("CASE A1 : EXCEPTION RAISED");

               END CASE_A1;

     CASE_A2 : DECLARE

                    PROCEDURE PROC1 (A : STRING) IS
                    BEGIN
                         IF A'FIRST /= 5 OR A'LAST /= 2 THEN
                              FAILED ("CASE A2 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC1 ((5 .. 2 => 'E'));

               EXCEPTION

                    WHEN OTHERS =>
                         FAILED ("CASE A2 : EXCEPTION RAISED");

               END CASE_A2;

          END CASE_A;

CASE_B :  BEGIN

     CASE_B1 : DECLARE

                    PROCEDURE PROC1 (A : T1; L, U : INTEGER) IS
                    BEGIN
                         IF A'FIRST /= L OR A'LAST /= U THEN
                              FAILED ("CASE B1 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    BEGIN

                         PROC1 ((5 .. INTEGER'FIRST => -2),
                                 5, INTEGER'FIRST);

                    EXCEPTION

                         WHEN CONSTRAINT_ERROR =>
                              FAILED ("CASE B1A : CONSTRAINT_ERROR " &
                                      "RAISED FOR NULL RANGE");
                         WHEN OTHERS =>
                              FAILED ("CASE B1A : EXCEPTION RAISED");

                    END;

                    BEGIN

                         PROC1 ((IDENT_INT(6) .. 3 => -2),6,3);

                    EXCEPTION

                         WHEN OTHERS =>
                              FAILED ("CASE B1B : EXCEPTION RAISED");

                    END;

               END CASE_B1;

     CASE_B2 : DECLARE

                    PROCEDURE PROC1 (A : STRING) IS
                    BEGIN
                         IF A'FIRST /= 1 OR 
                            A'LAST /= INTEGER'FIRST THEN
                              FAILED ("CASE B2 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC1 ((1 .. INTEGER'FIRST => ' '));

               EXCEPTION

                    WHEN OTHERS =>
                         FAILED ("CASE B2 : EXCEPTION RAISED");

               END CASE_B2;

          END CASE_B;

CASE_C :  BEGIN

     CASE_C1 : DECLARE

                    PROCEDURE PROC1 (A : T2) IS
                    BEGIN
                         IF A'FIRST(1) /=  5 OR A'LAST(1) /=  3 OR
                            A'FIRST(2) /= INTEGER'LAST-1 OR
                            A'LAST(2)  /= INTEGER'LAST THEN
                              FAILED ("CASE C1 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC1 ((5 .. 3 => 
                              (IDENT_INT(INTEGER'LAST-1) ..
                               IDENT_INT(INTEGER'LAST) => -2)));
                    FAILED ("CASE C1 : CONSTRAINT_ERROR NOT RAISED");

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>
                         NULL;

                    WHEN OTHERS =>
                         FAILED ("CASE C1 : EXCEPTION RAISED");

               END CASE_C1;

     CASE_C2 : DECLARE

                    PROCEDURE PROC1 (A : T2) IS
                    BEGIN
                         IF A'FIRST(1) /=  INTEGER'FIRST OR
                            A'LAST(1)  /=  INTEGER'FIRST+1 OR
                            A'FIRST(2) /= 14 OR A'LAST(2) /= 11 THEN
                              FAILED ("CASE C2 : INCORRECT BOUNDS");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC1 ((IDENT_INT(INTEGER'FIRST) ..
                            IDENT_INT(INTEGER'FIRST+1) =>
                                    (14 .. IDENT_INT(11) => -2)));
                    FAILED ("CASE C2 : CONSTRAINT_ERROR NOT RAISED");

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>
                         NULL;

                    WHEN OTHERS =>
                         FAILED ("CASE C2 : EXCEPTION RAISED");

               END CASE_C2;

          END CASE_C;

     END;

     RESULT;

END C43206A;
