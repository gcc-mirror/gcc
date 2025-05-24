-- C46052A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO AN 
-- ENUMERATION TYPE IF THE VALUE OF THE OPERAND DOES NOT BELONG TO THE
-- RANGE OF ENUMERATION VALUES FOR THE TARGET SUBTYPE.

-- R.WILLIAMS 9/9/86

WITH REPORT; USE REPORT;
PROCEDURE C46052A IS
     
     TYPE ENUM IS (A, AB, ABC, ABCD);
     E : ENUM := ENUM'VAL (IDENT_INT (0));
     
     FUNCTION IDENT (E : ENUM) RETURN ENUM IS
     BEGIN
          RETURN ENUM'VAL (IDENT_INT (ENUM'POS (E)));
     END IDENT;

BEGIN
     TEST ( "C46052A", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                       "CONVERSION TO AN ENUMERATION TYPE IF THE " &
                       "VALUE OF THE OPERAND DOES NOT BELONG TO " &
                       "THE RANGE OF ENUMERATION VALUES FOR THE " &
                       "TARGET SUBTYPE" );

     DECLARE
          SUBTYPE SENUM IS ENUM RANGE AB .. ABCD;
     BEGIN
          E := IDENT (SENUM (E));
          FAILED ( "NO EXCEPTION RAISED FOR 'SENUM (E)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'SENUM (E)'" );
     END;

     DECLARE
          SUBTYPE NOENUM IS ENUM RANGE ABCD .. AB;
     BEGIN
          E := IDENT (NOENUM (E));
          FAILED ( "NO EXCEPTION RAISED FOR 'NOENUM (E)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'NOENUM (E)'" );
     END;

     DECLARE
          SUBTYPE SCHAR IS CHARACTER RANGE 'C' .. 'R';
          A : CHARACTER := IDENT_CHAR ('A');
     BEGIN
          A := IDENT_CHAR (SCHAR (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'SCHAR (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'SCHAR (A)'" );
     END;

     DECLARE
          SUBTYPE FRANGE IS BOOLEAN RANGE FALSE .. FALSE;
          T : BOOLEAN := IDENT_BOOL (TRUE);
     BEGIN
          T := IDENT_BOOL (FRANGE (T));
          FAILED ( "NO EXCEPTION RAISED FOR 'FRANGE (T)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'FRANGE (T)'" );
     END;

     RESULT;
END C46052A;
