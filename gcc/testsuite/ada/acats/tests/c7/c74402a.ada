-- C74402A.ADA

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
-- CHECK THAT A SUBPROGRAM PARAMETER OF A LIMITED TYPE MAY HAVE A
-- DEFAULT EXPRESSION, EVEN IF THE SUBPROGRAM IS DECLARED OUTSIDE
-- THE PACKAGE THAT DECLARES THE LIMITED TYPE.
-- (SEE ALSO 6.4.2/T1 FOR TESTS OF OTHER LIMITED TYPES.)

-- DSJ 5/6/83
-- SPS 10/24/83

WITH REPORT;
PROCEDURE C74402A IS

     USE REPORT;

BEGIN

     TEST("C74402A", "CHECK THAT A SUBPROGRAM PARAMETER OF A LIMITED " &
                     "TYPE MAY HAVE A DEFAULT EXPRESSION, EVEN IF "    &
                     "THE SUBPROGRAM IS DECLARED OUTSIDE THE PACKAGE " &
                     "THAT DECLARES THE LIMITED TYPE");

     DECLARE

          PACKAGE PACK1 IS

               TYPE LP1 IS LIMITED PRIVATE;
               TYPE LP2 IS ARRAY (1 .. 2) OF LP1;
               TYPE LP3 IS
                    RECORD
                         C1, C2 : LP2;
                    END RECORD;

               FUNCTION F1 RETURN LP1;
               FUNCTION F2 RETURN LP2;
               FUNCTION F3 RETURN LP3;

               PROCEDURE G1 (X : LP1 := F1);      -- LEGAL
               PROCEDURE G2 (X : LP2 := F2);      -- LEGAL
               PROCEDURE G3 (X : LP3 := F3);      -- LEGAL

          PRIVATE

               TYPE LP1 IS NEW INTEGER;

          END PACK1;

          PACKAGE BODY PACK1 IS

               FUNCTION F1 RETURN LP1 IS
               BEGIN
                    RETURN LP1'(1);
               END F1;

               FUNCTION F2 RETURN LP2 IS
               BEGIN
                    RETURN LP2'(2,3);
               END F2;

               FUNCTION F3 RETURN LP3 IS
               BEGIN
                    RETURN LP3'((4,5),(6,7));
               END F3;

               PROCEDURE G1 (X : LP1 := F1) IS
               BEGIN
                    IF X /= LP1'(1) THEN
                         FAILED("WRONG DEFAULT VALUE - LP1");
                    END IF;
               END G1;

               PROCEDURE G2 (X : LP2 := F2) IS
               BEGIN
                    IF X /= LP2'(2,3) THEN
                         FAILED("WRONG DEFAULT VALUE - LP2");
                    END IF;
               END G2;

               PROCEDURE G3 (X : LP3 := F3) IS
               BEGIN
                    IF X /= LP3'((4,5),(6,7)) THEN
                         FAILED("WRONG DEFAULT VALUE - LP3");
                    END IF;
               END G3;

          BEGIN

               G1;            -- LEGAL, DEFAULT USED
               G2;            -- LEGAL, DEFAULT USED
               G3;            -- LEGAL, DEFAULT USED

               G1(F1);        -- LEGAL
               G2(F2);        -- LEGAL
               G3(F3);        -- LEGAL

          END PACK1;

          USE PACK1;

          PROCEDURE G4 (X : LP1 := F1) IS
          BEGIN
               G1;            -- LEGAL, DEFAULT USED
               G1(X);
          END G4;

          PROCEDURE G5 (X : LP2 := F2) IS
          BEGIN
               G2;            -- LEGAL, DEFAULT USED
               G2(X);
          END G5;

          PROCEDURE G6 (X : LP3 := F3) IS
          BEGIN
               G3;            -- DEFAULT USED
               G3(X);
          END G6;

     BEGIN

          G4;                 -- LEGAL, DEFAULT USED
          G5;                 -- LEGAL, DEFAULT USED
          G6;                 -- LEGAL, DEFAULT USED

          G4(F1);             -- LEGAL
          G5(F2);             -- LEGAL
          G6(F3);             -- LEGAL

     END;

     RESULT;

END C74402A;
