-- CC1221D.ADA

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
--     FOR A FORMAL INTEGER TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT:  EXPLICIT CONVERSION TO AND FROM REAL
--     TYPES AND IMPLICIT CONVERSION FROM INTEGER LITERALS.

-- HISTORY:
--     BCB 11/12/87  CREATED ORIGINAL TEST FROM SPLIT OF CC1221A.ADA

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CC1221D IS

     SUBTYPE SUBINT IS INTEGER RANGE -100 .. 100;
     TYPE INT IS RANGE -300 .. 300;
     SUBTYPE SINT1 IS INT
          RANGE INT (IDENT_INT (-4)) .. INT (IDENT_INT (4));
     TYPE INT1 IS RANGE -6 .. 6;

BEGIN
     TEST ( "CC1221D", "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
                       "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
                       "DECLARED AND ARE THEREFORE AVAILABLE " &
                       "WITHIN THE GENERIC UNIT:  EXPLICIT " &
                       "CONVERSION TO AND FROM REAL TYPES AND " &
                       "IMPLICIT CONVERSION FROM INTEGER LITERALS");

     DECLARE -- (D) CHECKS FOR EXPLICIT CONVERSION TO AND FROM OTHER
             --     NUMERIC TYPES, AND IMPLICIT CONVERSION FROM
             --     INTEGER LITERALS.

          GENERIC
               TYPE T IS RANGE <>;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS

               TYPE FIXED IS DELTA 0.1 RANGE -100.0 .. 100.0;
               FI0  : FIXED := 0.0;
               FI2  : FIXED := 2.0;
               FIN2 : FIXED := -2.0;

               FL0  : FLOAT := 0.0;
               FL2  : FLOAT := 2.0;
               FLN2 : FLOAT := -2.0;

               T0  : T := 0;
               T2  : T := 2;
               TN2 : T := -2;

               FUNCTION IDENT (X : T) RETURN T IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN X;
                    ELSE
                         RETURN T'FIRST;
                    END IF;
               END IDENT;

          BEGIN
               IF T0 + 1 /= 1 THEN
                    FAILED ( "INCORRECT RESULTS FOR IMPLICIT " &
                             "CONVERSION WITH TYPE " & STR & " - 1" );
               END IF;

               IF T2 + 1 /= 3 THEN
                    FAILED ( "INCORRECT RESULTS FOR IMPLICIT " &
                             "CONVERSION WITH TYPE " & STR & " - 2" );
               END IF;

               IF TN2 + 1 /= -1 THEN
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

               IF T (FL0) /= IDENT (T0) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF T (FL2) /= T2 THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF T (FLN2) /= IDENT (TN2) THEN
                    FAILED ( "INCORRECT CONVERSION FROM " &
                             "FLOAT VALUE -2.0 WITH TYPE " & STR);
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

               IF FLOAT (IDENT (T0)) /= FL0 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE 0.0 WITH TYPE " & STR);
               END IF;

               IF FLOAT (T2) /= FL2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE 2.0 WITH TYPE " & STR);
               END IF;

               IF FLOAT (IDENT (TN2)) /= FLN2 THEN
                    FAILED ( "INCORRECT CONVERSION TO " &
                             "FLOAT VALUE -2.0 WITH TYPE " & STR);
               END IF;

          END P;

          PROCEDURE P1 IS NEW P (SUBINT);
          PROCEDURE P2 IS NEW P (SINT1);
          PROCEDURE P3 IS NEW P (INT1);

     BEGIN
           P1 ( "SUBINT" );
           P2 ( "SINT" );
           P3 ( "INT1" );
     END; -- (D).

     RESULT;
END CC1221D;
