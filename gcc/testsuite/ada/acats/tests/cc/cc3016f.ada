-- CC3016F.ADA

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
--     OFFICE, 3E 114, THE PENTAGON, WASHINGTON DC 20301-3081.

-- OBJECTIVE:
--   CHECK THAT AN INSTANTIATED PACKAGE HAS THE PROPERTIES REQUIRED
--   OF A PACKAGE.

--   CHECK THAT IF THE PARENT TYPE IN A DERIVED TYPE DEFINITION IS
--   A GENERIC FORMAL TYPE, THE OPERATIONS DECLARED FOR THE DERIVED
--   TYPE IN THE TEMPLATE ARE DETERMINED BY THE DECLARATION OF THE
--   FORMAL TYPE.  THE OPERATIONS DECLARED FOR DERIVED TYPE IN THE
--   INSTANCE ARE DETERMINED BY THE ACTUAL TYPE DENOTED BY THE FORMAL
--   PARAMETER.  SEE AI-00398.

-- HISTORY:
--   DAS  8 OCT 90   INITIAL VERSION.
--   JRL 02/19/93  ADDED USE CLAUSES FOR INSTANCES TO ENSURE DIRECT
--                 OPERATOR VISIBILITY. CHANGED NT4'LAST TO P4.NT4'LAST
--                 IN ASSIGNMENT STATEMENT FOR P4.X IN EXAMPLE_4.
--                 CORRECTED ABE ERRORS IN EXAMPLE_2 AND EXAMPLE_3.
--                 CHANGED R3."+" FROM MULTIPLICATION TO SUBTRACTION TO
--                 AVOID CONSTRAINT_ERROR.

WITH REPORT;

PROCEDURE CC3016F IS
BEGIN
     REPORT.TEST ("CC3016F", "CHECK THAT IF THE PARENT TYPE IN A " &
                             "DERIVED TYPE DEFINITION IS A GENERIC " &
                             "FORMAL TYPE, THE OPERATIONS DECLARED " &
                             "FOR THE DERIVED TYPE IN THE TEMPLATE " &
                             "ARE DETERMINED BY THE DECLARATION OF " &
                             "THE FORMAL TYPE, AND THAT THE " &
                             "OPERATIONS DECLARED FOR THE DERIVED " &
                             "TYPE IN THE INSTANCE ARE DETERMINED BY " &
                             "THE ACTUAL TYPE DENOTED BY THE FORMAL " &
                             "PARAMETER (AI-00398)");
EXAMPLE_2:
     DECLARE
          GENERIC
               TYPE PRIV IS PRIVATE;
          PACKAGE GP2 IS
               TYPE NT2 IS NEW PRIV;
          END GP2;

          PACKAGE R2 IS
               TYPE T2 IS RANGE 1..10;
               FUNCTION F RETURN T2;
          END R2;

          PACKAGE P2 IS NEW GP2 (PRIV => R2.T2);
          USE P2;

          XX1 : P2.NT2;
          XX2 : P2.NT2;
          XX3 : P2.NT2;

          PACKAGE BODY R2 IS
               FUNCTION F RETURN T2 IS
               BEGIN
                    RETURN T2'LAST;
               END F;
          END R2;
     BEGIN
          XX1 := 5;                   -- IMPLICIT CONVERSION FROM
                                      -- UNIVERSAL INTEGER TO P2.NT2
                                      -- IN P2.
          XX2 := XX1 + XX1;           -- PREDEFINED "+" DECLARED FOR
                                      -- P2.NT2.
          XX3 := P2.F;                -- FUNCTION F DERIVED WITH THE
                                      -- INSTANCE.

     END EXAMPLE_2;

EXAMPLE_3:
     DECLARE
          GENERIC
               TYPE T3 IS RANGE <>;
          PACKAGE GP3 IS
               TYPE NT3 IS NEW T3;
               X : NT3 := 5;
               Y : NT3 := X + 3;      -- USES PREDEFINED "+" EVEN IN
                                      -- INSTANCES
          END GP3;

          PACKAGE R3 IS
               TYPE S IS RANGE 1..10;
               FUNCTION "+" (LEFT : IN S; RIGHT : IN S) RETURN S;
          END R3;

          PACKAGE P3 IS NEW GP3 ( T3 => R3.S );
          USE P3;

          Z : P3.NT3;

          PACKAGE BODY R3 IS
               FUNCTION "+" (LEFT : IN S; RIGHT : IN S) RETURN S IS
               BEGIN  -- IMPLEMENT AS SUBTRACTION, NOT ADDITION
                    RETURN LEFT - RIGHT;
               END "+";
          END R3;
     BEGIN
          Z := P3.X + 3;     -- USES REDEFINED "+"

          IF ( P3.Y /= P3.NT3'(8) ) THEN
               REPORT.FAILED ("PREDEFINED ""+"" NOT USED TO COMPUTE " &
                             "P3.Y");
          END IF;

          IF (Z /= P3.NT3'(2) ) THEN
               REPORT.FAILED ("REDEFINED ""+"" NOT USED TO COMPUTE Z");
          END IF;
     END EXAMPLE_3;

EXAMPLE_4:
     DECLARE
          GENERIC
               TYPE T4 IS LIMITED PRIVATE;
          PACKAGE GP4 IS
               TYPE NT4 IS NEW T4;
               X : NT4;
          END GP4;

          PACKAGE P4 IS NEW GP4 (BOOLEAN);
          USE P4;

     BEGIN
          P4.X := P4.NT4'LAST;
          IF ( P4.X  OR (NOT P4.X) ) THEN
               REPORT.COMMENT ("P4.X CORRECTLY HAS A BOOLEAN TYPE");
          END IF;
     END EXAMPLE_4;

EXAMPLE_5:
     DECLARE
          GENERIC
               TYPE T5 (D : POSITIVE) IS PRIVATE;
          PACKAGE GP5 IS
               TYPE NT5 IS NEW T5;
               X : NT5 (D => 5);
               Y : POSITIVE := X.D;  -- REFERS TO DISCRIMINANT OF NT5
          END GP5;

          TYPE REC (A : POSITIVE) IS
               RECORD
                    D : POSITIVE := 7;
               END RECORD;
          PACKAGE P5 IS NEW GP5 (T5 => REC);
               -- P5.Y INITIALIZED WITH VALUE USING COMPONENT SELECTION
               -- OPERATION FOR THE DISCRIMINANT, I.E. FOR PARENT TYPE
               -- T5 WHICH DENOTES REC.

          W1 : POSITIVE := P5.X.D;     -- VALUE IS 7
          W2 : POSITIVE := P5.X.A;     -- VALUE IS 5
          W3 : POSITIVE := P5.Y;       -- VALUE IS 5;
     BEGIN
          IF ( ( W1 /= 7 ) OR ( W2 /= 5 ) OR  (W3 /= 5 ) ) THEN
               REPORT.FAILED ("INCORRECT COMPONENT SELECTION");
          END IF;
     END EXAMPLE_5;

     REPORT.RESULT;

END CC3016F;
