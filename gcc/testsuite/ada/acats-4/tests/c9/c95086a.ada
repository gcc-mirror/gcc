-- C95086A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED AT THE TIME OF CALL WHEN
-- THE VALUE OF AN ACTUAL OUT SCALAR PARAMETER DOES NOT SATISFY THE
-- RANGE CONSTRAINTS OF THE FORMAL PARAMETER.

-- GLH 7/16/85
-- JRK 8/23/85

WITH REPORT; USE REPORT;
PROCEDURE C95086A IS

     SUBTYPE SUBINT1 IS INTEGER RANGE -10..10;
     SUBTYPE SUBINT2 IS INTEGER RANGE -20..20;

     I10  : SUBINT1 := 10;
     I20  : SUBINT2 := 20;

     TASK T1 IS
          ENTRY E1 (I : OUT SUBINT1);
     END T1;

     TASK BODY T1 IS
     BEGIN
          LOOP
               BEGIN
                    SELECT
                         ACCEPT E1 (I : OUT SUBINT1) DO
                              I := SUBINT1'FIRST;
                         END E1;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN ACCEPT E1");
               END;
          END LOOP;
     END T1;

BEGIN

     TEST ("C95086A", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
                      "AT THE TIME OF CALL WHEN THE VALUE OF AN " &
                      "ACTUAL OUT SCALAR PARAMETER DOES NOT " &
                      "SATISFY THE RANGE CONSTRAINTS OF THE FORMAL " &
                      "PARAMETER");

     BEGIN
          T1.E1 (SUBINT1(I20));
          IF I20 /= IDENT_INT (-10) THEN
               FAILED ("OUT PARAM DID NOT GET CORRECT VALUE - 1");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED ON CALL TO E1 - 1");
     END;

     BEGIN
          I20 := IDENT_INT (20);
          T1.E1 (I20);
          IF I20 /= IDENT_INT (-10) THEN
               FAILED ("OUT PARAM DID NOT GET CORRECT VALUE - 2");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED ON CALL TO E1 - 2");
     END;

     RESULT;

END C95086A;
