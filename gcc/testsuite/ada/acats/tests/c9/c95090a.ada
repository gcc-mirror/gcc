-- C95090A.ADA

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
-- CHECK THAT ARRAYS THAT ARE COMPONENTS OF RECORDS ARE PASSED CORRECTLY
-- TO ENTRIES.  SPECIFICALLY,
--   (A) CHECK ALL PARAMETER MODES.

-- GLH 7/25/85

WITH REPORT; USE REPORT;
PROCEDURE C95090A IS

BEGIN
     TEST ("C95090A", "CHECK THAT ARRAYS WHICH ARE COMPONENTS OF " &
                      "RECORDS ARE PASSED CORRECTLY TO ENTRIES");

     --------------------------------------------

     DECLARE   -- (A)

          TYPE ARRAY_TYPE IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
          SUBTYPE ARRAY_SUBTYPE IS ARRAY_TYPE (1..IDENT_INT(5));

          TYPE RECORD_TYPE IS
               RECORD
                    I : INTEGER;
                    A : ARRAY_SUBTYPE;
               END RECORD;

          REC : RECORD_TYPE := (I => 23,
                                A => (1..3 => IDENT_INT(7), 4..5 => 9));
          BOOL : BOOLEAN;

          TASK T1 IS
               ENTRY E1 (ARR : ARRAY_TYPE);
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT E1 (ARR : ARRAY_TYPE) DO
                    IF ARR /= (7, 7, 7, 9, 9) THEN
                         FAILED ("IN PARAMETER NOT PASSED CORRECTLY");
                    END IF;
                    IF ARR'FIRST /= IDENT_INT (1) OR
                       ARR'LAST /= IDENT_INT (5) THEN
                         FAILED ("WRONG BOUNDS FOR IN PARAMETER");
                    END IF;
               END E1;
          END T1;

          TASK T2 IS
               ENTRY E2 (ARR : IN OUT ARRAY_TYPE);
          END T2;

          TASK BODY T2 IS
          BEGIN
               ACCEPT E2 (ARR : IN OUT ARRAY_TYPE) DO
                    IF ARR /= (7, 7, 7, 9, 9) THEN
                         FAILED ("IN OUT PARAMETER NOT PASSED " &
                                 "CORRECTLY");
                    END IF;
                    IF ARR'FIRST /= IDENT_INT (1) OR
                       ARR'LAST /= IDENT_INT (5) THEN
                         FAILED ("WRONG BOUNDS FOR IN OUT PARAMETER");
                    END IF;
                    ARR := (ARR'RANGE => 5);
               END E2;
          END T2;

          TASK T3 IS
               ENTRY E3 (ARR : OUT ARRAY_TYPE);
          END T3;

          TASK BODY T3 IS
          BEGIN
               ACCEPT E3 (ARR : OUT ARRAY_TYPE) DO
                    IF ARR'FIRST /= IDENT_INT (1) OR
                       ARR'LAST /= IDENT_INT (5) THEN
                         FAILED ("WRONG BOUNDS FOR OUT PARAMETER");
                    END IF;
                    ARR := (ARR'RANGE => 3);
               END E3;
          END T3;

     BEGIN     -- (A)

          T1.E1 (REC.A);
          IF REC.A /= (7, 7, 7, 9, 9) THEN
               FAILED ("IN PARAM CHANGED BY PROCEDURE");
          END IF;

          T2.E2 (REC.A);
          IF REC.A /= (5, 5, 5, 5, 5) THEN
               FAILED ("IN OUT PARAM RETURNED INCORRECTLY");
          END IF;

          T3.E3 (REC.A);
          IF REC.A /= (3, 3, 3, 3, 3) THEN
               FAILED ("OUT PARAM RETURNED INCORRECTLY");
          END IF;

     END; -- (A)

     --------------------------------------------

     RESULT;
END C95090A;
