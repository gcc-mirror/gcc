-- C45631C.DEP

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
--     CHECK THAT FOR TYPE LONG_INTEGER 'ABS A' EQUALS A IF A IS
--     POSITIVE AND EQUALS -A IF A IS NEGATIVE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     LONG_INTEGER.

--     IF "LONG_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_LONG" MUST BE REJECTED.

-- HISTORY:
--     RJW 02/26/86 CREATED ORIGINAL TEST.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.

WITH REPORT; USE REPORT;

PROCEDURE C45631C IS

     CHECK_LONG : LONG_INTEGER;                      -- N/A => ERROR.

     FUNCTION IDENT (X : LONG_INTEGER) RETURN LONG_INTEGER IS
     BEGIN
          IF X >= LONG_INTEGER (INTEGER'FIRST) AND
             X <= LONG_INTEGER (INTEGER'LAST) THEN
               RETURN LONG_INTEGER (IDENT_INT (INTEGER (X)));
          ELSIF EQUAL (3, 3) THEN
               RETURN X;
          END IF;
          RETURN 0;
     END IDENT;

BEGIN

     TEST ( "C45631C", "CHECK THAT FOR TYPE LONG_INTEGER 'ABS A' " &
                       "EQUALS A IF A IS POSITIVE AND EQUALS -A IF " &
                       "A IS NEGATIVE" );

     DECLARE

          P : LONG_INTEGER := IDENT (1);
          N : LONG_INTEGER := IDENT (-1);
          Z : LONG_INTEGER := IDENT (0);
     BEGIN

          IF ABS P = P THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR P - 1" );
          END IF;

          IF ABS N = -N THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR N - 1" );
          END IF;

          IF ABS Z = Z THEN
               NULL;
          ELSE
               FAILED ( "'ABS TEST FOR Z - 1" );
          END IF;

          IF ABS (Z) = -Z THEN
               NULL;
          ELSE
               FAILED ( "'ABS TEST FOR Z - 2");
          END IF;

          IF "ABS" (RIGHT => P) = P THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR P - 2" );
          END IF;

          IF "ABS" (N) = -N THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR N - 2 " );
          END IF;

          IF "ABS" (Z) = Z THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR Z - 3" );
          END IF;

          IF ABS (IDENT (-LONG_INTEGER'LAST)) = LONG_INTEGER'LAST
             THEN
               NULL;
          ELSE
               FAILED ( "'ABS' TEST FOR -LONG_INTEGER'LAST" );
          END IF;
     END;

     RESULT;

END C45631C;
