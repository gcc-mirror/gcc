-- C32115A.ADA

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
--    CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING A CONSTRAINED
--    ACCESS TYPE IS DECLARED WITH AN INITIAL NON-NULL ACCESS VALUE,
--    CONSTRAINT_ERROR IS RAISED IF AN INDEX BOUND OR A DISCRIMINANT
--    VALUE OF THE DESIGNATED OBJECT DOES NOT EQUAL THE CORRESPONDING
--    VALUE SPECIFIED FOR THE ACCESS SUBTYPE.

-- HISTORY:
--    RJW 07/20/86  CREATED ORIGINAL TEST.
--    JET 08/05/87  ADDED DEFEAT OF DEAD VARIABLE OPTIMIZATION.
--    PWN 11/30/94  REMOVED TEST ILLEGAL IN ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE C32115A IS

     PACKAGE PKG IS
          TYPE PRIV (D : INTEGER) IS PRIVATE;

     PRIVATE
          TYPE PRIV (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
     END PKG;

     USE PKG;

     TYPE ACCP IS ACCESS PRIV (IDENT_INT (1));

     TYPE REC (D : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE ACCR IS ACCESS REC (IDENT_INT (2));

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;

     TYPE ACCA IS ACCESS ARR (IDENT_INT (1) .. IDENT_INT (2));

     TYPE ACCN IS ACCESS ARR (IDENT_INT (1) .. IDENT_INT (0));

BEGIN
     TEST ("C32115A", "CHECK THAT WHEN A VARIABLE OR CONSTANT " &
                      "HAVING A CONSTRAINED ACCESS TYPE IS " &
                      "DECLARED WITH AN INITIAL NON-NULL ACCESS " &
                      "VALUE, CONSTRAINT_ERROR IS RAISED IF AN " &
                      "INDEX BOUND OR A DISCRIMINANT VALUE OF THE " &
                      "DESIGNATED OBJECT DOES NOT EQUAL THE " &
                      "CORRESPONDING VALUE SPECIFIED FOR THE " &
                      "ACCESS SUBTYPE" );

     BEGIN
          DECLARE
               AC1 : CONSTANT ACCP := NEW PRIV (D => (IDENT_INT (2)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC1'" );
               IF AC1 /= NULL THEN
                    COMMENT ("DEFEAT 'AC1' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC1'" );
     END;

     BEGIN
          DECLARE
               AC2 : ACCP := NEW PRIV (D => (IDENT_INT (2)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC2'" );
               IF AC2 /= NULL THEN
                    COMMENT ("DEFEAT 'AC2' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC2'" );
     END;

     BEGIN
          DECLARE
               AC3 : CONSTANT ACCP := NEW PRIV (D => (IDENT_INT (0)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC3'" );
               IF AC3 /= NULL THEN
                    COMMENT ("DEFEAT 'AC3' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC3'" );
     END;

     BEGIN
          DECLARE
               AC4 : ACCP := NEW PRIV (D => (IDENT_INT (0)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC4'" );
               IF AC4 /= NULL THEN
                    COMMENT ("DEFEAT 'AC4' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC4'" );
     END;

     BEGIN
          DECLARE
               AC5 : CONSTANT ACCR := NEW REC'(D => (IDENT_INT (1)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC5'" );
               IF AC5 /= NULL THEN
                    COMMENT ("DEFEAT 'AC5' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC5'" );
     END;

     BEGIN
          DECLARE
               AC6 : ACCR := NEW REC' (D => (IDENT_INT (1)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC6'" );
               IF AC6 /= NULL THEN
                    COMMENT ("DEFEAT 'AC6' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC6'" );
     END;

     BEGIN
          DECLARE
               AC7 : CONSTANT ACCR := NEW REC'(D => (IDENT_INT (3)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC7'" );
               IF AC7 /= NULL THEN
                    COMMENT ("DEFEAT 'AC7' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC7'" );
     END;

     BEGIN
          DECLARE
               AC8 : ACCR := NEW REC' (D => (IDENT_INT (3)));
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC8'" );
               IF AC8 /= NULL THEN
                    COMMENT ("DEFEAT 'AC8' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC8'" );
     END;

     BEGIN
          DECLARE
               AC9 : CONSTANT ACCA :=
                    NEW ARR'(IDENT_INT (1) .. IDENT_INT (1) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC9'" );
               IF AC9 /= NULL THEN
                    COMMENT ("DEFEAT 'AC9' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC9'" );
     END;

     BEGIN
          DECLARE
               AC10 : ACCA :=
                    NEW ARR'(IDENT_INT (1) .. IDENT_INT (1) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC10'" );
               IF AC10 /= NULL THEN
                    COMMENT ("DEFEAT 'AC10' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC10'" );
     END;

     BEGIN
          DECLARE
               AC11 : CONSTANT ACCA :=
                    NEW ARR' (IDENT_INT (0) .. IDENT_INT (2) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC11'" );
               IF AC11 /= NULL THEN
                    COMMENT ("DEFEAT 'AC11' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC11'" );
     END;

     BEGIN
          DECLARE
               AC12 : ACCA :=
                    NEW ARR'(IDENT_INT (0) .. IDENT_INT (2) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC12'" );
               IF AC12 /= NULL THEN
                    COMMENT ("DEFEAT 'AC12' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC12'" );
     END;


     BEGIN
          DECLARE
               AC15 : CONSTANT ACCN :=
                    NEW ARR' (IDENT_INT (0) .. IDENT_INT (0) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC15'" );
               IF AC15 /= NULL THEN
                    COMMENT ("DEFEAT 'AC15' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'AC15'" );
     END;

     BEGIN
          DECLARE
               AC16 : ACCN :=
                    NEW ARR'(IDENT_INT (0) .. IDENT_INT (0) => 0);
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC16'" );
               IF AC16 /= NULL THEN
                    COMMENT ("DEFEAT 'AC16' OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'AC16'" );
     END;

     RESULT;
END C32115A;
