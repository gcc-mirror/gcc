-- C84005A.ADA

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
--     CHECK THAT TWO POTENTIALLY VISIBLE HOMOGRAPHS OF A SUBPROGRAM
--     IDENTIFIER CAN BE MADE DIRECTLY VISIBLE BY A USE CLAUSE, AND THAT
--     WHEN DIFFERENT FORMAL PARAMETER NAMES ARE USED THE SUBPROGRAMS
--     ARE REFERENCED CORRECTLY.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C84005A IS

     PACKAGE PACK1 IS
          FUNCTION FUNK(A : INTEGER) RETURN INTEGER;
          PROCEDURE PROK(A : INTEGER; B : OUT INTEGER);
     END PACK1;

     PACKAGE PACK2 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER;
          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER);
     END PACK2;

     USE PACK1, PACK2;
     VAR1, VAR2 : INTEGER;

     PACKAGE BODY PACK1 IS
          FUNCTION FUNK(A : INTEGER) RETURN INTEGER IS
          BEGIN
               IF EQUAL (A,A) THEN
                    RETURN (1);
               ELSE
                    RETURN (0);
               END IF;
          END FUNK;

          PROCEDURE PROK(A : INTEGER; B : OUT INTEGER) IS
          BEGIN
               IF EQUAL (A,A) THEN
                    B := 1;
               ELSE
                    B := 0;
               END IF;
          END PROK;
     END PACK1;

     PACKAGE BODY PACK2 IS
          FUNCTION FUNK(X : INTEGER) RETURN INTEGER IS
          BEGIN
               IF EQUAL (X,X) THEN
                    RETURN (2);
               ELSE
                    RETURN (0);
               END IF;
          END FUNK;

          PROCEDURE PROK(X : INTEGER; Y : OUT INTEGER) IS
          BEGIN
               IF EQUAL (X,X) THEN
                    Y := 2;
               ELSE
                    Y := 0;
               END IF;
          END PROK;
     END PACK2;

BEGIN
     TEST ("C84005A", "CHECK THAT TWO POTENTIALLY VISIBLE HOMOGRAPHS " &
                      "OF A SUBPROGRAM IDENTIFIER CAN BE MADE " &
                      "DIRECTLY VISIBLE BY A USE CLAUSE, AND THAT " &
                      "WHEN DIFFERENT FORMAL PARAMETER NAMES ARE " &
                      "USED, THE SUBPROGRAMS ARE REFERENCED CORRECTLY");

     IF FUNK(A => 3) /= IDENT_INT(1) THEN
          FAILED("PACK1.FUNK RETURNS INCORRECT RESULT");
     END IF;

     IF FUNK(X => 3) /= IDENT_INT(2) THEN
          FAILED("PACK2.FUNK RETURNS INCORRECT RESULT");
     END IF;

     PROK(A => 3, B => VAR1);
     PROK(X => 3, Y => VAR2);

     IF VAR1 /= IDENT_INT(1) THEN
          FAILED("PACK1.PROK RETURNS INCORRECT RESULT");
     END IF;

     IF VAR2 /= IDENT_INT(2) THEN
          FAILED("PACK2.PROK RETURNS INCORRECT RESULT");
     END IF;

     RESULT;
END C84005A;
