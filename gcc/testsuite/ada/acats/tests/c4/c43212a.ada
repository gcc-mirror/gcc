-- C43212A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL SUBAGGREGATES FOR A
-- PARTICULAR DIMENSION DO NOT HAVE THE SAME BOUNDS.

-- EG  02/06/1984
-- JBG 3/30/84
-- JRK 4/18/86   CORRECTED ERROR TO ALLOW CONSTRAINT_ERROR TO BE
--               RAISED EARLIER.
-- EDS 7/15/98   AVOID OPTIMIZATION.

WITH REPORT;

PROCEDURE C43212A IS

     USE REPORT;

BEGIN

     TEST ("C43212A", "CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL " &
                      "SUBAGGREGATES FOR A PARTICULAR DIMENSION DO "  &
                      "NOT HAVE THE SAME BOUNDS");

     DECLARE

          TYPE CHOICE_INDEX IS (H, I);
          TYPE CHOICE_CNTR  IS ARRAY(CHOICE_INDEX) OF INTEGER;

          CNTR : CHOICE_CNTR := (CHOICE_INDEX => 0);

          FUNCTION CALC (A : CHOICE_INDEX; B : INTEGER)
                                             RETURN INTEGER IS
          BEGIN
               CNTR(A) := CNTR(A) + 1;
               RETURN IDENT_INT(B);
          END CALC;

     BEGIN

CASE_1 :  DECLARE

               TYPE T IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>)
                                                   OF INTEGER;

               A1 : T(1 .. 3, 2 .. 5) := (OTHERS => (OTHERS => 0));

          BEGIN

               CNTR := (CHOICE_INDEX => 0);
               A1 := (1 => (CALC(H,2) .. CALC(I,5) => -4),
                      2 => (CALC(H,3) .. CALC(I,6) => -5),
                      3 => (CALC(H,2) .. CALC(I,5) => -3));
               FAILED ("CASE 1 : CONSTRAINT_ERROR NOT RAISED" &
                       INTEGER'IMAGE(A1(1,5)) );

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    IF CNTR(H) < 2 AND CNTR(I) < 2 THEN
                         FAILED ("CASE 1 : BOUNDS OF SUBAGGREGATES " &
                                 "NOT DETERMINED INDEPENDENTLY");
                    END IF;

               WHEN OTHERS =>
                    FAILED ("CASE 1 : WRONG EXCEPTION RAISED");

          END CASE_1;

CASE_1A : DECLARE

               TYPE T IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>)
                                                  OF INTEGER;

               A1 : T(1 .. 3, 2 .. 3) := (1 .. 3 => (2 .. 3 => 1));

          BEGIN

               IF (1 .. 2 => (IDENT_INT(3) .. IDENT_INT(4) => 0),
                        3 => (1, 2)) = A1 THEN
                    BEGIN
                         COMMENT(" IF SHOULD GENERATE CONSTRAINT_ERROR " & 
                                 INTEGER'IMAGE(A1(1,2)) );
                    EXCEPTION
                         WHEN OTHERS =>
                            FAILED ("CASE 1A : CONSTRAINT_ERROR NOT RAISED");
                    END;
               END IF;
                    FAILED ("CASE 1A : CONSTRAINT_ERROR NOT RAISED");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("CASE 1A : WRONG EXCEPTION RAISED");

          END CASE_1A;

CASE_2 :  DECLARE

               TYPE T IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>)
                                                   OF INTEGER;

               A2 : T(1 .. 3, IDENT_INT(4) .. 2);

          BEGIN

               CNTR := (CHOICE_INDEX => 0);
               A2 := (1 => (CALC(H,5) .. CALC(I,3) => -4),
                      3 => (CALC(H,4) .. CALC(I,2) => -5),
                      2 => (CALC(H,4) .. CALC(I,2) => -3));
               FAILED ("CASE 2 : CONSTRAINT_ERROR NOT RAISED " &
                       INTEGER'IMAGE(IDENT_INT(A2'FIRST(1))));
          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    IF CNTR(H) < 2 AND CNTR(I) < 2 THEN
                         FAILED ("CASE 2 : BOUNDS OF SUBAGGREGATES " &
                                 "NOT DETERMINED INDEPENDENTLY");
                    END IF;

               WHEN OTHERS =>
                    FAILED ("CASE 2 : WRONG EXCEPTION RAISED");

          END CASE_2;

     END;

     RESULT;

END C43212A;
