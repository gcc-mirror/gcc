-- CC1223A.ADA

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
--     FOR A FORMAL FIXED POINT TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT: ASSIGNMENT, MEMBERSHIP TESTS,
--     QUALIFICATION, EXPLICIT CONVERSION TO AND FROM OTHER NUMERIC
--     TYPES, AND REAL LITERALS (IMPLICIT CONVERSION FROM UNIVERSAL REAL
--     TO THE FORMAL TYPE), 'FIRST, 'LAST, 'SIZE, 'ADDRESS, 'DELTA, 'FORE, 
--     'AFT, 'MACHINE_ROUNDS, 'MACHINE_OVERFLOWS.

-- HISTORY:
--     RJW 09/30/86  CREATED ORIGINAL TEST.
--     JLH 09/25/87  REFORMATTED HEADER.
--     RJW 08/21/89  MODIFIED CHECKS FOR 'MANTISSA AND 'AFT.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE CC1223A IS

     TYPE FIXED IS DELTA 0.1 RANGE -100.0 .. 100.0;

BEGIN
     TEST ( "CC1223A",  "FOR A FORMAL FIXED POINT TYPE, CHECK " &
                        "THAT THE BASIC OPERATIONS ARE " &
                        "IMPLICITLY DECLARED AND ARE THEREFORE " &
                        "AVAILABLE WITHIN THE GENERIC UNIT" );

     DECLARE -- (A). CHECKS FOR ASSIGNMENT, MEMBERSHIP TESTS AND
             --      QUALIFICATION.

          GENERIC
               TYPE T IS DELTA <>;
               TYPE T1 IS DELTA <>;
               F  : T;
               F1 : T1;
          PROCEDURE P (F2 : T; STR : STRING);

          PROCEDURE P (F2 : T; STR : STRING) IS
               SUBTYPE ST IS T RANGE -1.0 .. 1.0;
               F3, F4  : T;

               FUNCTION FUN (X : T) RETURN BOOLEAN IS
               BEGIN
                    RETURN IDENT_BOOL (TRUE);
               END FUN;

               FUNCTION FUN (X : T1) RETURN BOOLEAN IS
               BEGIN
                    RETURN IDENT_BOOL (FALSE);
               END FUN;

          BEGIN
               F3 := F;
               F4 := F2;
               F3 := F4;

               IF F3 /= F2 THEN
                    FAILED ( "INCORRECT RESULTS FOR ASSIGNMENT " &
                             "WITH TYPE - " & STR);
               END IF;

               IF F IN ST THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR ""IN"" WITH " &
                             "TYPE  - " & STR);
               END IF;

               IF F2 NOT IN ST THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR ""NOT IN"" WITH " &
                             "TYPE  - " & STR);
               END IF;

               IF T'(F) /= F THEN
                    FAILED ( "INCORRECT RESULTS FOR QUALIFICATION " &
                             "WITH TYPE - " & STR & " - 1" );
               END IF;

               IF FUN (T'(1.0)) THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR QUALIFICATION " &
                             "WITH TYPE - " & STR & " - 2" );
               END IF;

          END P;

          PROCEDURE P1 IS NEW P (FIXED, FIXED, 0.0, 0.0);
          PROCEDURE P2 IS NEW P (DURATION, DURATION, 0.0, 0.0);

     BEGIN
          P1 (2.0, "FIXED");
          P2 (2.0, "DURATION");
     END; -- (A).

     DECLARE -- (B) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER
             --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM
             --     REAL LITERAL.

          GENERIC
               TYPE T IS DELTA <>;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS

               FL0  : FLOAT := 0.0;
               FL2  : FLOAT := 2.0;
               FLN2 : FLOAT := -2.0;

               I0  : INTEGER := 0;
               I2  : INTEGER := 2;
               IN2 : INTEGER := -2;

               T0  : T := 0.0;
               T2  : T := 2.0;
               TN2 : T := -2.0;

               FUNCTION IDENT (X : T) RETURN T IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN X;
                    ELSE
                         RETURN T'FIRST;
                    END IF;
               END IDENT;

          BEGIN
               IF T0 + 1.0 /= 1.0 THEN
                    FAILED ( "INCORRECT RESULTS FOR IMPLICIT " &
                             "CONVERSION WITH TYPE " & STR & " - 1" );
               END IF;

               IF T2 + 1.0 /= 3.0 THEN
                    FAILED ( "INCORRECT RESULTS FOR IMPLICIT " &
                             "CONVERSION WITH TYPE " & STR & " - 2" );
               END IF;

               IF TN2 + 1.0 /= -1.0 THEN
                    FAILED ( "INCORRECT RESULTS FOR IMPLICIT " &
                             "CONVERSION WITH TYPE " & STR & " - 3" );
               END IF;

               IF T (FL0) /= T0 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF T (FL2) /= IDENT (T2) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF T (FLN2) /= TN2 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE -2.0 WITH TYPE " & STR);
               END IF;

               IF T (I0) /= IDENT (T0) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "INTEGER VALUE 0 WITH TYPE " & STR);
               END IF;

               IF T (I2) /= T2 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "INTEGER VALUE 2 WITH TYPE " & STR);
               END IF;

               IF T (IN2) /= IDENT (TN2) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "INTEGER VALUE -2 WITH TYPE " & STR);
               END IF;

               IF FLOAT (T0) /= FL0 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF FLOAT (IDENT (T2)) /= FL2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF FLOAT (TN2) /= FLN2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE -2.0 WITH TYPE " & STR);
               END IF;

               IF INTEGER (IDENT (T0)) /= I0 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "INTEGER VALUE 0 WITH TYPE " & STR);
               END IF;

               IF INTEGER (T2) /= I2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "INTEGER VALUE 2 WITH TYPE " & STR);
               END IF;

               IF INTEGER (IDENT (TN2)) /= IN2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "INTEGER VALUE -2 WITH TYPE " & STR);
               END IF;

          END P;

          PROCEDURE P1 IS NEW P (FIXED);
          PROCEDURE P2 IS NEW P (DURATION);

     BEGIN
           P1 ( "FIXED" );
           P2 ( "DURATION" );
     END; -- (B).

     DECLARE -- (C) CHECKS FOR ATTRIBUTES.

          GENERIC
               TYPE T IS DELTA <>;
               F, L, D : T;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS

               F1 : T;
               A  : ADDRESS := F'ADDRESS;
               S  : INTEGER := F'SIZE;

               I  : INTEGER;

               B1 : BOOLEAN := T'MACHINE_ROUNDS;
               B2 : BOOLEAN := T'MACHINE_OVERFLOWS;

          BEGIN
               IF T'DELTA /= D THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'DELTA" );
               END IF;

               IF T'FIRST /= F THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'FIRST" );
               END IF;

               IF T'LAST /= L THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'LAST" );
               END IF;

               IF T'FORE < 2 THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'FORE" );
               END IF;

               IF T'AFT <= 0 THEN
                    FAILED ( "INCORRECT VALUE FOR " & STR & "'AFT" );
               END IF;

          END P;

          PROCEDURE P1 IS
               NEW P (FIXED, FIXED'FIRST, FIXED'LAST, FIXED'DELTA);
          PROCEDURE P2 IS
               NEW P (DURATION, DURATION'FIRST, DURATION'LAST,
                      DURATION'DELTA);

     BEGIN
           P1 ( "FIXED" );
           P2 ( "DURATION" );
     END; -- (C).

     RESULT;
END CC1223A;
