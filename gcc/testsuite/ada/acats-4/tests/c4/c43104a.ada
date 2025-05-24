-- C43104A.ADA

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
--     CHECK THAT WITH THE TYPE OF THE AGGREGATE RESOLVED, THE
--     DISCRIMINANT MAY BE USED TO DECIDE TO WHICH OF THE VARIANT'S
--     SUBTYPES THE AGGREGATE BELONGS.

-- HISTORY:
--     DHH 08/08/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43104A IS

     TYPE INT IS RANGE 0 .. 10;

     TYPE VAR_REC(BOOL : BOOLEAN := TRUE) IS
          RECORD
               CASE BOOL IS
                    WHEN TRUE =>
                         X : INTEGER;
                    WHEN FALSE =>
                         Y : INT;
               END CASE;
          END RECORD;

     SUBTYPE S_TRUE IS VAR_REC(TRUE);
     SUBTYPE S_FALSE IS VAR_REC(FALSE);

     PROCEDURE CHECK(P : IN S_TRUE) IS
     BEGIN
          IF P.BOOL = FALSE THEN
               FAILED("WRONG PROCEDURE ENTERED");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED("EXCEPTION RAISED INSIDE PROCEDURE");

     END CHECK;

BEGIN
     TEST("C43104A", "CHECK THAT WITH THE TYPE OF THE AGGREGATE " &
                     "RESOLVED, THE DISCRIMINANT MAY BE USED TO " &
                     "DECIDE TO WHICH OF THE VARIANT'S SUBTYPES " &
                     "THE AGGREGATE BELONGS");

     CHECK((TRUE, 1));

     BEGIN

          CHECK((FALSE, 2));
          FAILED("PROCEDURE CALL USING '(FALSE, 2)' DID NOT RAISE " &
                 "EXCEPTION");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED("INCORRECT EXCEPTION RAISED ON PROCEDURE CALL " &
                      "USING '(FALSE,2)'");
     END;

     RESULT;
END C43104A;
