-- CC1222A.ADA

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
-- FOR A FORMAL FLOATING POINT TYPE, CHECK THAT THE FOLLOWING BASIC 
-- OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
-- WITHIN THE GENERIC UNIT: ASSIGNMENT, MEMBERSHIP TESTS, 
-- QUALIFICATION, EXPLICIT CONVERSION TO AND FROM OTHER NUMERIC TYPES, 
-- AND REAL LITERALS (IMPLICIT CONVERSION FROM UNIVERSAL REAL TO THE 
-- FORMAL TYPE), 'FIRST, 'LAST, 'SIZE, 'ADDRESS, 'DIGITS, 'MACHINE_RADIX,
-- 'MACHINE_MANTISSA, 'MACHINE_EMAX, 'MACHINE_EMIN, 'MACHINE_ROUNDS,
-- 'MACHINE_OVERFLOWS.

-- R.WILLIAMS 9/30/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE CC1222A IS

     TYPE NEWFLT IS NEW FLOAT;

BEGIN
     TEST ( "CC1222A",  "FOR A FORMAL FLOATING POINT TYPE, CHECK " &
                        "THAT THE BASIC OPERATIONS ARE " &
                        "IMPLICITLY DECLARED AND ARE THEREFORE " &
                        "AVAILABLE WITHIN THE GENERIC UNIT" );

     DECLARE -- (A). CHECKS FOR ASSIGNMENT, MEMBERSHIP TESTS AND 
             --      QUALIFICATION.

          GENERIC
               TYPE T IS DIGITS <>;
               TYPE T1 IS DIGITS <>;
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

          PROCEDURE P1 IS NEW P (FLOAT,  FLOAT,  0.0, 0.0);
          PROCEDURE P2 IS NEW P (NEWFLT, NEWFLT, 0.0, 0.0);
     
     BEGIN
          P1 (2.0, "FLOAT");
          P2 (2.0, "NEWFLT");
     END; -- (A).         

     DECLARE -- (B) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER 
             --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM 
             --     REAL LITERAL.

          GENERIC
               TYPE T IS DIGITS <>;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS

               TYPE FIXED IS DELTA 0.1 RANGE -100.0 .. 100.0;
               FI0  : FIXED := 0.0;
               FI2  : FIXED := 2.0;
               FIN2 : FIXED := -2.0; 

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

               IF T (FI0) /= T0 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FIXED VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF T (FI2) /= IDENT (T2) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FIXED VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF T (FIN2) /= TN2 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FIXED VALUE -2.0 WITH TYPE " & STR);
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

               IF FIXED (T0) /= FI0 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FIXED VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF FIXED (IDENT (T2)) /= FI2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FIXED VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF FIXED (TN2) /= FIN2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FIXED VALUE -2.0 WITH TYPE " & STR);
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

          PROCEDURE P1 IS NEW P (FLOAT);
          PROCEDURE P2 IS NEW P (NEWFLT);

     BEGIN
           P1 ( "FLOAT" );
           P2 ( "NEWFLT" );
     END; -- (B).         

     DECLARE -- (C) CHECKS FOR ATTRIBUTES.

          GENERIC
               TYPE T IS DIGITS <>;
               F, L : T;
               D : INTEGER;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS

               F1 : T;
               A  : ADDRESS := F'ADDRESS;
               S  : INTEGER := F'SIZE;

               I  : INTEGER;
               I1 : INTEGER := T'MACHINE_RADIX;
               I2 : INTEGER := T'MACHINE_MANTISSA;
               I3 : INTEGER := T'MACHINE_EMAX;
               I4 : INTEGER := T'MACHINE_EMIN;

               B1 : BOOLEAN := T'MACHINE_ROUNDS;
               B2 : BOOLEAN := T'MACHINE_OVERFLOWS;

          BEGIN
               IF T'DIGITS /= D THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'DIGITS" );
               END IF;

               IF T'FIRST /= F THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'FIRST" );
               END IF;

               IF T'LAST /= L THEN
                    FAILED ( "INCORRECT VALUE FOR " &
                              STR & "'LAST" );
               END IF;

          END P;

          PROCEDURE P1 IS 
               NEW P (FLOAT, FLOAT'FIRST, FLOAT'LAST, FLOAT'DIGITS);
          PROCEDURE P2 IS 
               NEW P (NEWFLT, NEWFLT'FIRST, NEWFLT'LAST, 
                      NEWFLT'DIGITS);

     BEGIN
           P1 ( "FLOAT" );
           P2 ( "NEWFLT" );
     END; -- (C).         

     RESULT;
END CC1222A;
