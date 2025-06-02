-- C47009B.ADA

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
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN ACCESS
--     TYPE, CHECK THAT CONSTRAINT_ERROR IS NOT RAISED WHEN THE VALUE
--     OF THE OPERAND IS NULL.

-- HISTORY:
--     RJW 07/23/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   CONSTRAINTS OF B SUBTYPES TO VALUES WHICH ARE
--                   CLOSER TO THE VALUES OF THE A SUBTYPES.  INDENTED
--                   THE EXCEPTION STATEMENTS IN SUBTEST 11.

WITH REPORT; USE REPORT;
PROCEDURE C47009B IS

BEGIN

     TEST( "C47009B", "WHEN THE TYPE MARK IN A QUALIFIED " &
                      "EXPRESSION DENOTES AN ACCESS TYPE, " &
                      "CHECK THAT CONSTRAINT_ERROR IS NOT " &
                      "RAISED WHEN THE VALUE OF THE OPERAND IS NULL" );

     DECLARE

          TYPE ACC1 IS ACCESS BOOLEAN;
          A : ACC1;

     BEGIN
          A := ACC1'(NULL);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE ACC1" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE ACC1" );
     END;

     DECLARE

          TYPE ACC2 IS ACCESS INTEGER;
          A : ACC2;

     BEGIN
          A := ACC2'(NULL);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE ACC2" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE ACC2" );
     END;

     DECLARE

          TYPE CHAR IS ('A', 'B');
          TYPE ACC3 IS ACCESS CHAR;
          A : ACC3;

     BEGIN
          A := ACC3'(NULL);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE ACC3" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE ACC3" );
     END;

     DECLARE

          TYPE FLOAT1 IS DIGITS 5 RANGE -1.0 .. 1.0;
          TYPE ACC4 IS ACCESS FLOAT1;
          A : ACC4;

     BEGIN
          A := ACC4'(NULL);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE ACC4" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE ACC4" );
     END;

     DECLARE

          TYPE FIXED IS DELTA 0.5 RANGE -1.0 .. 1.0;
          TYPE ACC5 IS ACCESS FIXED;
          A : ACC5;

     BEGIN
          A := ACC5'(NULL);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR TYPE ACC5" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR TYPE ACC5" );
     END;

     DECLARE

          TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;
          TYPE ACC6 IS ACCESS ARR;
          SUBTYPE ACC6A IS ACC6 (IDENT_INT (1) .. IDENT_INT (5));
          SUBTYPE ACC6B IS ACC6 (IDENT_INT (2) .. IDENT_INT (10));
          A : ACC6A;
          B : ACC6B;

     BEGIN
          A := ACC6A'(B);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " &
                        "TYPE ACC6" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                        "TYPE ACC6" );
     END;

     DECLARE

          TYPE ARR IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>)
               OF INTEGER;
          TYPE ACC7 IS ACCESS ARR;
          SUBTYPE ACC7A IS ACC7 (IDENT_INT (1) .. IDENT_INT (5),
                                 IDENT_INT (1) .. IDENT_INT (1));
          SUBTYPE ACC7B IS ACC7 (IDENT_INT (1) .. IDENT_INT (15),
                                 IDENT_INT (1) .. IDENT_INT (10));
          A : ACC7A;
          B : ACC7B;

     BEGIN
          A := ACC7A'(B);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " &
                        "TYPE ACC7" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                        "TYPE ACC7" );
     END;

     DECLARE

          TYPE REC (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE ACC8 IS ACCESS REC;
          SUBTYPE ACC8A IS ACC8 (IDENT_INT (5));
          SUBTYPE ACC8B IS ACC8 (IDENT_INT (6));
          A : ACC8A;
          B : ACC8B;

     BEGIN
          A := ACC8A'(B);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " &
                        "TYPE ACC8" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                        "TYPE ACC8" );
     END;

     DECLARE

          TYPE REC (D1,D2 : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE ACC9 IS ACCESS REC;
          SUBTYPE ACC9A IS ACC9 (IDENT_INT (4), IDENT_INT (5));
          SUBTYPE ACC9B IS ACC9 (IDENT_INT (5), IDENT_INT (4));
          A : ACC9A;
          B : ACC9B;

     BEGIN
          A := ACC9A'(B);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " &
                        "TYPE ACC9" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                        "TYPE ACC9" );
     END;

     DECLARE

          PACKAGE PKG IS
               TYPE REC (D : INTEGER) IS PRIVATE;

          PRIVATE
               TYPE REC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;

          END PKG;

          USE PKG;

          TYPE ACC10 IS ACCESS REC;
          SUBTYPE ACC10A IS ACC10 (IDENT_INT (10));
          SUBTYPE ACC10B IS ACC10 (IDENT_INT (9));
          A : ACC10A;
          B : ACC10B;

     BEGIN
          A := ACC10A'(B);
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF " &
                        "TYPE ACC10" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                        "TYPE ACC10" );
     END;

     DECLARE

          PACKAGE PKG1 IS
               TYPE REC (D : INTEGER) IS LIMITED PRIVATE;

          PRIVATE
               TYPE REC (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;

          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

               TYPE ACC11 IS ACCESS REC;
               SUBTYPE ACC11A IS ACC11 (IDENT_INT (11));
               SUBTYPE ACC11B IS ACC11 (IDENT_INT (12));
               A : ACC11A;
               B : ACC11B;

          BEGIN
               A := ACC11A'(B);
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED FOR SUBTYPES OF" &
                             " TYPE ACC11" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED FOR SUBTYPES OF " &
                             "TYPE ACC11" );
          END PKG2;

     BEGIN
          NULL;
     END;

     RESULT;
END C47009B;
