-- CC1018A.ADA

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
-- CHECK THAT A FORMAL OUT PARAMETER OF A GENERIC FORMAL SUBPROGRAM CAN 
-- HAVE A FORMAL LIMITED TYPE AND AN ARRAY TYPE WITH LIMITED COMPONENTS.

-- AH  10/3/86

WITH REPORT; USE REPORT;
PROCEDURE CC1018A IS
     TYPE INT IS RANGE 1..10;
     TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INT;
     INT_OBJ : INT := 4;
     ARR_OBJ : ARR(1..5) := (2, 8, 2, 8, 2);

     GENERIC
          TYPE GLP IS LIMITED PRIVATE;
          TYPE GARR IS ARRAY (INTEGER RANGE <>) OF GLP;
          LP_OBJ : IN OUT GLP;
          GA_OBJ : IN OUT GARR;
          WITH PROCEDURE P (X : OUT GLP; Y : OUT GARR);
          WITH FUNCTION SAME (LEFT, RIGHT : GLP) RETURN BOOLEAN;
     PROCEDURE GEN_PROC;

     PROCEDURE GET_VALUES (X1 : OUT INT; Y1 : OUT ARR) IS
     BEGIN
          X1 := 4;
          Y1 := (2, 8, 2, 8, 2);
     END GET_VALUES;

     FUNCTION SAME_VALUE (LEFT, RIGHT : INT) RETURN BOOLEAN IS 
     BEGIN
          RETURN LEFT = RIGHT;
     END SAME_VALUE;

     PROCEDURE GEN_PROC IS
          LP : GLP;
          A : GARR(1..5);
     BEGIN
          P(LP, A);
          IF NOT SAME(LP, LP_OBJ) THEN
               FAILED ("LIMITED PRIVATE TYPE HAS INCORRECT VALUE");
          END IF;

          FOR INDEX IN A'RANGE LOOP
               IF NOT SAME(A(INDEX), GA_OBJ(INDEX)) THEN
                    FAILED ("LIMITED PRIVATE TYPE COMPONENT " & 
                            "HAS INCORRECT VALUE");
               END IF;
          END LOOP;
     END GEN_PROC;

     PROCEDURE TEST_LP IS NEW GEN_PROC(INT, ARR, INT_OBJ, ARR_OBJ,
                                       GET_VALUES, SAME_VALUE);

BEGIN
     TEST ("CC1018A", "A GENERIC FORMAL SUBPROGRAM OUT PRARAMETER " & 
                      "CAN HAVE A LIMITED TYPE");
     TEST_LP;

     RESULT;
END CC1018A;
