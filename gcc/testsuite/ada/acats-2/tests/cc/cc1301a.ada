-- CC1301A.ADA

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
-- CHECK THAT DEFAULT GENERIC SUBPROGRAM PARAMETERS WORK CORRECTLY,
-- INCLUDING OVERLOADED AND PREDEFINED OPERATOR_SYMBOLS,
-- AND SUBPROGRAMS HIDDEN AT THE INSTANTIATION.
-- BOTH KINDS OF DEFAULTS ARE TESTED, FOR BOTH PROCEDURES
-- AND FUNCTIONS.

-- DAT 8/14/81
-- JBG 5/5/83
-- JBG 8/3/83

WITH REPORT; USE REPORT;

PROCEDURE CC1301A IS

     FUNCTION "-" (R, S : INTEGER) RETURN INTEGER;

     FUNCTION NEXT (X : INTEGER) RETURN INTEGER;

     PROCEDURE BUMP (X : IN OUT INTEGER);

     GENERIC
          WITH FUNCTION "*" (A, B : INTEGER) RETURN INTEGER IS "-";
          WITH FUNCTION "+" (R, S: INTEGER) RETURN INTEGER IS 
                                                       STANDARD."+";
          WITH FUNCTION "-" (A, B : INTEGER) RETURN INTEGER IS <> ;
          WITH FUNCTION NEXTO (Q : INTEGER) RETURN INTEGER IS NEXT ;
          WITH PROCEDURE BUMPO (A : IN OUT INTEGER) IS BUMP;
          WITH FUNCTION NEXT (Q : INTEGER) RETURN INTEGER  IS <> ;
          WITH PROCEDURE BUMP (Q : IN OUT INTEGER) IS <> ;
          TYPE INTEGER IS RANGE <> ;
          WITH FUNCTION "*" (A , B : INTEGER) RETURN INTEGER IS <> ;
          WITH FUNCTION "-" (A, B : INTEGER) RETURN INTEGER IS <> ;
          WITH FUNCTION NEXT (Q : INTEGER) RETURN INTEGER IS <> ;
          WITH PROCEDURE BUMP (Z : IN OUT INTEGER) IS <> ;
     PACKAGE PKG IS
          SUBTYPE INT IS STANDARD.INTEGER;
          DIFF : INT := -999;
     END PKG;

     TYPE NEWINT IS NEW INTEGER RANGE -1000 .. 1000;

     FUNCTION PLUS (Q1, Q2 : INTEGER) RETURN INTEGER RENAMES "+";

     FUNCTION "+" (X, Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN PLUS (X, PLUS (Y, -10));
          -- (X + Y - 10)
     END "+";

     FUNCTION "-" (R, S : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN - R + S;
          -- (-R + S - 10)
     END "-";

     FUNCTION NEXT (X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X + 1;
          -- (X + 1 - 10)
          -- (X - 9)
     END NEXT;

     PROCEDURE BUMP (X : IN OUT INTEGER) IS
     BEGIN
          X := NEXT (X);
          -- (X := X - 9)
     END BUMP;

     PACKAGE BODY PKG IS
          W : INTEGER;
          WI : INT;
     BEGIN
          W := NEXT (INTEGER'(3) * 4 - 2);
          -- (W := (4 ** 3 - 2) - 1)
          -- (W := 61)
          BUMP (W);
          -- (W := 61 + 7)
          -- (W := 68)
          WI := NEXT (INT'(3) * 4 - 2 + NEXTO (0));
          -- (3 * 4) => (3 - 4) => (-3 + 4 - 10) = -9
          -- ((-9) - 2) => (2 + 2 - (-9) - 20) = -7
          -- (-7 + (-9)) => -16
          -- (WI := 7 - (-16)) => (WI := 23)
          BUMPO (WI);
          -- (WI := 23 - 9) (= 14)
          BUMP (WI);
          -- (WI := 14 - 9) (= 5)
          DIFF := STANDARD."-" (INT(W), WI);
          -- (DIFF := 68 - 5) (= 63)
     END PKG;

     FUNCTION "*" (Y, X : NEWINT) RETURN NEWINT IS
     BEGIN
          RETURN X ** INTEGER(Y);
          -- (X,Y) (Y ** X)
     END "*";

     FUNCTION NEXT (Z : NEWINT) RETURN NEWINT IS
     BEGIN
          RETURN Z - 1;
          -- (Z - 1)
     END NEXT;

     PROCEDURE BUMP (ZZ : IN OUT NEWINT) IS
     BEGIN
          FAILED ("WRONG PROCEDURE CALLED");
     END BUMP;
BEGIN
     TEST ("CC1301A", "DEFAULT GENERIC SUBPROGRAM PARAMETERS");

     DECLARE
          PROCEDURE BUMP (QQQ : IN OUT NEWINT) IS
          BEGIN
               QQQ := QQQ + 7;
               -- (QQQ + 7)
          END BUMP;

          FUNCTION NEXT (Q7 : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN Q7 - 17;
               -- (-Q7 + 17 - 10)
               -- (7 - Q7)
          END NEXT;

          FUNCTION "-" (Q3, Q4 : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN -Q3 + Q4 + Q4;
               -- (-Q3 + Q4 - 10 + Q4 - 10) = (Q4 + Q4 - Q3 - 20)
          END "-";

          PACKAGE P1 IS NEW PKG (INTEGER => NEWINT);

     BEGIN
          IF P1.DIFF /= 63 THEN
               FAILED ("WRONG DEFAULT SUBPROGRAM PARAMETERS");
          END IF;
     END;

     RESULT;
END CC1301A;
