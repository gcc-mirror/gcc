-- C43103A.ADA

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
-- CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN A VARIANT PART,
-- ITS VALUE CAN BE GIVEN BY A NON-STATIC EXPRESSION.

-- EG  02/13/84

WITH REPORT;

PROCEDURE C43103A IS

     USE REPORT;

BEGIN

     TEST("C43103A","CHECK THAT IF A DISCRIMINANT DOES NOT GOVERN " &
                    "A VARIANT PART, ITS VALUE CAN BE GIVEN BY A "  &
                    "NON-STATIC EXPRESSION");

     BEGIN

          COMMENT ("CASE A : DISCRIMINANT THAT IS NOT USED INSIDE " &
                   "THE RECORD");

CASE_A :  DECLARE

               TYPE R1 (A : INTEGER) IS
                    RECORD
                         B : STRING(1 .. 2);
                         C : INTEGER;
                    END RECORD;

               A1 : R1(IDENT_INT(5)) := (IDENT_INT(5), "AB", -2);

          BEGIN

               IF A1.A /= IDENT_INT(5) OR A1.B /= "AB" OR 
                  A1.C /= -2 THEN
                    FAILED ("CASE A : INCORRECT VALUES IN RECORD");
               END IF;

          END CASE_A;

          COMMENT ("CASE B : DISCRIMINANT THAT IS USED AS AN ARRAY " &
                   "INDEX BOUND");

CASE_B :  DECLARE

               SUBTYPE STB IS INTEGER RANGE 1 .. 10;
               TYPE TB IS ARRAY(STB RANGE <>) OF INTEGER;
               TYPE R2 (A : STB) IS
                    RECORD
                         B : TB(1 .. A);
                         C : BOOLEAN;
                    END RECORD;

               B1 : R2(IDENT_INT(2)) := (IDENT_INT(2), (-1, -2), FALSE);

          BEGIN

               IF B1.B'LAST /= IDENT_INT(2) THEN
                    FAILED ("CASE B : INCORRECT UPPER BOUND");
               ELSIF B1.A /= IDENT_INT(2) OR B1.B /= (-1, -2) OR
                     B1.C /= FALSE THEN
                    FAILED ("CASE B : INCORRECT VALUES IN RECORD");
               END IF;

          END CASE_B;

          COMMENT ("CASE C : DISCRIMINANT THAT IS USED IN A " &
                   "DISCRIMINANT CONSTRAINT");

CASE_C :  DECLARE

               SUBTYPE STC IS INTEGER RANGE 1 .. 10;
               TYPE TC IS ARRAY(STC RANGE <>) OF INTEGER;
               TYPE R3 (A : STC) IS
                    RECORD
                         B : TC(1 .. A);
                         C : INTEGER := -4;
                    END RECORD;
               TYPE R4 (A : INTEGER) IS
                    RECORD
                         B : R3(A);
                         C : INTEGER;
                    END RECORD;

               C1 : R4(IDENT_INT(3)) := (IDENT_INT(3), 
                                         (IDENT_INT(3), (1, 2, 3), 4),
                                         5);

          BEGIN

               IF C1.B.B /= (1, 2, 3) OR C1.B.C /= 4 OR
                  C1.C   /= 5 THEN
                    FAILED ("CASE C : INCORRECT VALUES IN RECORD");
               END IF;

          END CASE_C;

     END;

     RESULT;

END C43103A;
