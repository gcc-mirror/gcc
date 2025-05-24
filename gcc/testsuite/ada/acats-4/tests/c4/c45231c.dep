-- C45231C.DEP

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
--     CHECK THAT THE RELATIONAL AND MEMBERSHIP OPERATIONS YIELD
--     CORRECT RESULTS FOR PREDEFINED TYPE LONG_INTEGER (INCLUDING
--     THE CASE IN WHICH THE RELATIONAL OPERATORS ARE REDEFINED).

--     SUBTESTS ARE:
--       (A). TESTS FOR RELATIONAL OPERATORS.
--       (B). TESTS FOR MEMBERSHIP OPERATORS.
--       (C). TESTS FOR MEMBERSHIP OPERATORS IN THE CASE IN WHICH THE
--            RELATIONAL OPERATORS ARE REDEFINED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     LONG_INTEGER.

--     IF "LONG_INTEGER" IS NOT SUPPORTED THEN THE DECLARATION OF
--     "CHECK_LONG" MUST BE REJECTED.

-- HISTORY:
--     RJW 02/04/86 CREATED ORIGINAL TEST.
--     DHH 01/08/87 ENTERED APPLICABILITY CRITERIA AND FORMATTED HEADER.

WITH REPORT; USE REPORT;

PROCEDURE C45231C IS

     CHECK_LONG : LONG_INTEGER;                        -- N/A => ERROR.

     FUNCTION IDENT (X : LONG_INTEGER) RETURN LONG_INTEGER IS
     BEGIN
          RETURN LONG_INTEGER (IDENT_INT (INTEGER (X)));
     END IDENT;

BEGIN

     TEST ( "C45231C", "CHECK THAT THE RELATIONAL AND " &
                       "MEMBERSHIP OPERATIONS YIELD CORRECT " &
                       "RESULTS FOR PREDEFINED TYPE LONG_INTEGER " &
                       "(INCLUDING THE CASE IN WHICH THE " &
                       "RELATIONAL OPERATORS ARE REDEFINED)" );

     DECLARE -- (A)

          I1A, I1B   : LONG_INTEGER          := IDENT (1);
          I2         : LONG_INTEGER          := IDENT (2);
          CI2        : CONSTANT LONG_INTEGER := 2;


     BEGIN -- (A)

          IF (I2 = CI2) AND (NOT (I2 /= CI2)) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 1" );
          END IF;

          IF (I2 /= 4) AND (NOT (I2 = 4)) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 2" );
          END IF;

          IF (I1A = I1B) AND (NOT (I1A /= I1B)) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 3" );
          END IF;

          IF (I2 >= CI2) AND (NOT (I2 < CI2)) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 4");
          END IF;

          IF (I2 <= 4) AND (NOT (I2 > 4)) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 5" );
          END IF;

          IF (I1A >= I1B) AND (I1A <= I1B) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 6" );
          END IF;

          IF ">" (LEFT => CI2, RIGHT => I1A) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 7" );
          END IF;

          IF "<" (LEFT => I1A, RIGHT => I2) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 8" );
          END IF;

          IF ">=" (LEFT => I1A, RIGHT => I1A ) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 9 ");
          END IF;

          IF "<=" (LEFT => I1A, RIGHT => CI2) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 10 ");
          END IF;

          IF "=" (LEFT => I1A, RIGHT => I1B ) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 11 ");
          END IF;

          IF "/=" (LEFT => CI2, RIGHT => 4) THEN
               NULL;
          ELSE
               FAILED ( "RELATIONAL TEST - 12 ");
          END IF;

     END; -- (A)

     ----------------------------------------------------------------

     DECLARE -- (B)

          SUBTYPE ST IS LONG_INTEGER RANGE -10 .. 10;

          I1 : LONG_INTEGER := IDENT (1);
          I5 : LONG_INTEGER := IDENT (5);

          CI2  : CONSTANT LONG_INTEGER := 2;
          CI10 : CONSTANT LONG_INTEGER := 10;


     BEGIN -- (B)

          IF (I1 IN ST) AND (I1 NOT IN CI2 .. CI10) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - B.1" );
          END IF;

          IF (IDENT (11) NOT IN ST) AND (CI2 IN I1 .. I5) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - B.2" );
          END IF;

          IF NOT (I5 NOT IN CI2 .. 10) AND NOT (IDENT (-11) IN ST) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - B.3" );
          END IF;

          IF NOT (I1 IN CI2 .. CI10) AND NOT (I5 NOT IN ST) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - B.4" );
          END IF;

          IF (I1 NOT IN I5 .. I1) AND NOT (I5 IN I5 .. I1) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - B.5" );
          END IF;

     END; -- (B)

     -------------------------------------------------------------

     DECLARE -- (C)

          SUBTYPE ST IS LONG_INTEGER RANGE -10 .. 10;

          I1 : LONG_INTEGER := IDENT (1);
          I5 : LONG_INTEGER := IDENT (5);

          CI2  : CONSTANT LONG_INTEGER := 2;
          CI10 : CONSTANT LONG_INTEGER := 10;


          FUNCTION ">" ( L, R : LONG_INTEGER ) RETURN BOOLEAN IS
          BEGIN
               RETURN LONG_INTEGER'POS (L) <= LONG_INTEGER'POS (R);
          END;

          FUNCTION ">=" ( L, R : LONG_INTEGER ) RETURN BOOLEAN IS
          BEGIN
               RETURN LONG_INTEGER'POS (L) < LONG_INTEGER'POS (R);
          END;

          FUNCTION "<" ( L, R : LONG_INTEGER ) RETURN BOOLEAN IS
          BEGIN
               RETURN  LONG_INTEGER'POS (L) >= LONG_INTEGER'POS (R);
          END;

          FUNCTION "<=" ( L, R : LONG_INTEGER ) RETURN BOOLEAN IS
          BEGIN
               RETURN LONG_INTEGER'POS (L) > LONG_INTEGER'POS (R);
          END;

     BEGIN -- (C)

          IF (I1 IN ST) AND (I1 NOT IN CI2 .. CI10) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - C.1" );
          END IF;

          IF (IDENT (11) NOT IN ST) AND (CI2 IN I1 .. I5) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - C.2" );
          END IF;

          IF NOT (I5 NOT IN CI2 .. 10) AND NOT (IDENT (-11) IN ST) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - C.3" );
          END IF;

          IF NOT (I1 IN CI2 .. CI10) AND NOT (I5 NOT IN ST) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - C.4" );
          END IF;

          IF (I1 NOT IN I5 .. I1) AND NOT (I5 IN I5 .. I1) THEN
               NULL;
          ELSE
               FAILED ( "MEMBERSHIP TEST - C.5" );
          END IF;

     END; -- (C)

     RESULT;

END C45231C;
