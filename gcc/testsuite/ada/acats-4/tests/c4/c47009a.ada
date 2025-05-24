-- C47009A.ADA

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
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A
--     CONSTRAINED ACCESS TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED
--     WHEN THE VALUE OF THE OPERAND IS NOT NULL AND THE DESIGNATED
--     OBJECT HAS INDEX BOUNDS OR DISCRIMINANT VALUES THAT DO NOT EQUAL
--     THOSE SPECIFIED IN THE ACCESS TYPE'S CONSTRAINT.

-- HISTORY:
--     RJW 7/23/86
--     DWC 07/24/87  REVISED TO MAKE THE ACCESS TYPE UNCONSTRAINED
--                   AND TO PREVENT DEAD VARIABLE OPTIMIZATION.

WITH REPORT; USE REPORT;
PROCEDURE C47009A IS

BEGIN

     TEST( "C47009A", "WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION " &
                      "DENOTES A CONSTRAINED ACCESS TYPE, CHECK " &
                      "THAT CONSTRAINT_ERROR IS RAISED WHEN THE " &
                      "VALUE OF THE OPERAND IS NOT NULL AND THE " &
                      "DESIGNATED OBJECT HAS INDEX BOUNDS OR " &
                      "DISCRIMINANT VALUES THAT DO NOT EQUAL THOSE " &
                      "SPECIFIED IN THE ACCESS TYPE'S CONSTRAINT" );

     DECLARE

          TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;
          TYPE ACC1 IS ACCESS ARR;
          SUBTYPE ACC1S IS ACC1 (IDENT_INT (1) .. IDENT_INT (5));
          A : ACC1;
          B : ARR (IDENT_INT (2) .. IDENT_INT (6));

     BEGIN
          A := ACC1S'(NEW ARR'(B'FIRST .. B'LAST => 0));
          IF A'FIRST = 1 THEN
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC1 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC1 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC1" );
     END;

     DECLARE

          TYPE ARR IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>)
               OF INTEGER;
          TYPE ACC2 IS ACCESS ARR;
          SUBTYPE ACC2S IS ACC2 (IDENT_INT (1) .. IDENT_INT (5),
                                   IDENT_INT (1) .. IDENT_INT (1));
          A : ACC2;
          B : ARR (IDENT_INT (1) .. IDENT_INT (5),
                   IDENT_INT (2) .. IDENT_INT (2));

     BEGIN
          A := ACC2S'(NEW ARR'(B'RANGE => (B'RANGE (2) => 0)));
          IF A'FIRST = 1 THEN
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC2 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC2 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC2" );
     END;

     DECLARE

          TYPE REC (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE ACC3 IS ACCESS REC;
          SUBTYPE ACC3S IS ACC3 (IDENT_INT (3));
          A : ACC3;
          B : REC (IDENT_INT (5)) := (D => (IDENT_INT (5)));

     BEGIN
          A := ACC3S'(NEW REC'(B));
          IF A = NULL THEN
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC3 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC3 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC3" );
     END;

     DECLARE

          TYPE REC (D1,D2 : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE ACC4 IS ACCESS REC;
          SUBTYPE ACC4S IS ACC4 (IDENT_INT (4), IDENT_INT (5));
          A : ACC4;
          B : REC (IDENT_INT (5), IDENT_INT (4)) :=
              (D1 => (IDENT_INT (5)), D2 => (IDENT_INT (4)));

     BEGIN
          A := ACC4S'(NEW REC'(B));
          IF A = NULL THEN
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC4 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC4 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR DISC VALUES " &
                        "DIFFERENT FROM THOSE OF TYPE ACC4" );
     END;

     DECLARE

          PACKAGE PKG IS
               TYPE REC (D : INTEGER) IS PRIVATE;

               B : CONSTANT REC;
          PRIVATE
               TYPE REC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;

               B : CONSTANT REC := (D => (IDENT_INT (4)));
          END PKG;

          USE PKG;

          TYPE ACC5 IS ACCESS REC;
          SUBTYPE ACC5S IS ACC5 (IDENT_INT (3));
          A : ACC5;

     BEGIN
          A := ACC5S'(NEW REC'(B));
          IF A = NULL THEN
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC5 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                        "DIFFERENT FROM THOSE OF TYPE ACC5 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR DISC VALUES " &
                        "DIFFERENT FROM THOSE OF TYPE ACC5" );
     END;

     DECLARE

          PACKAGE PKG1 IS
               TYPE REC (D : INTEGER) IS LIMITED PRIVATE;
               TYPE ACC6 IS ACCESS REC;
               SUBTYPE ACC6S IS ACC6 (IDENT_INT (6));

               FUNCTION F RETURN ACC6;
          PRIVATE
               TYPE REC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;

          PACKAGE BODY PKG1 IS

               FUNCTION F RETURN ACC6 IS
               BEGIN
                    RETURN NEW REC'(D => IDENT_INT (5));
               END F;

          END PKG1;

          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

               A : ACC6;

          BEGIN
               A := ACC6S'(F);
               IF A = NULL THEN
                    FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                             "DIFFERENT FROM THOSE OF TYPE ACC6 - 1" );
               ELSE
                    FAILED ( "NO EXCEPTION RAISED FOR INDEX BOUNDS " &
                             "DIFFERENT FROM THOSE OF TYPE ACC6 - 2" );
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR DISC " &
                             "VALUES DIFFERENT FROM THOSE OF TYPE " &
                             "ACC6" );
          END PKG2;

     BEGIN
          NULL;
     END;

     RESULT;
END C47009A;
