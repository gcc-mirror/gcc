-- C46054A.ADA

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
-- ACCESS SUBTYPE IF THE OPERAND VALUE IS NOT NULL AND THE 
-- DISCRIMINANTS OR INDEX BOUNDS OF THE DESIGNATED OBJECT DO NOT
-- MATCH THOSE OF THE TARGET TYPE.

-- R.WILLIAMS 9/9/86

WITH REPORT; USE REPORT;
PROCEDURE C46054A IS
     
BEGIN
     TEST ( "C46054A", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                       "CONVERSION TO AN ACCESS SUBTYPE IF THE " &
                       "OPERAND VALUE IS NOT NULL AND THE " &
                       "DISCRIMINANTS OR INDEX BOUNDS OF THE " &
                       "DESIGNATED OBJECT DO NOT MATCH THOSE OF " &
                       "THE TARGET TYPE" );

     DECLARE
          TYPE REC (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          
          TYPE ACREC IS ACCESS REC;
          A : ACREC (IDENT_INT (0)) := NEW REC (IDENT_INT (0));

          SUBTYPE ACREC3 IS ACREC (IDENT_INT (3));

          PROCEDURE PROC (A : ACREC) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (A.D);
          END PROC;

     BEGIN
          PROC (ACREC3 (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'ACREC3 (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'ACREC3 (A)'" );
     END;

     DECLARE
          TYPE REC (D1, D2 : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          
          TYPE ACREC IS ACCESS REC;

          A : ACREC (IDENT_INT (3), IDENT_INT (1)) := 
              NEW REC (IDENT_INT (3), IDENT_INT (1));

          SUBTYPE ACREC13 IS ACREC (IDENT_INT (1), IDENT_INT (3));

          PROCEDURE PROC (A : ACREC) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (A.D1);
          END PROC;

     BEGIN
          PROC (ACREC13 (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'ACREC13 (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'ACREC13 (A)'" );
     END;

     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          
          TYPE ACARR IS ACCESS ARR;
          A : ACARR (IDENT_INT (0) .. IDENT_INT (1)) := 
              NEW ARR'(IDENT_INT (0) .. IDENT_INT (1) => 0);

          SUBTYPE ACARR02 IS ACARR (IDENT_INT (0) .. IDENT_INT (2));

          PROCEDURE PROC (A : ACARR) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (A'LAST);
          END PROC;

     BEGIN
          PROC (ACARR02 (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'ACARR02 (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'ACARR02 (A)'" );
     END;

     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          
          TYPE ACARR IS ACCESS ARR;
          A : ACARR (IDENT_INT (1) .. IDENT_INT (0),
                     IDENT_INT (4) .. IDENT_INT (5)) := 
              NEW ARR'(IDENT_INT (1) .. IDENT_INT (0) =>
                      (IDENT_INT (4) .. IDENT_INT (5) => 0));

          SUBTYPE NACARR IS ACARR (IDENT_INT (0) .. IDENT_INT (1),
                                   IDENT_INT (5) .. IDENT_INT (4));

          PROCEDURE PROC (A : NACARR) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (A'LAST (1));
          END PROC;

     BEGIN
          PROC (NACARR (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'NACARR (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'NACARR (A)'" );
     END;

     DECLARE
          PACKAGE PKG1 IS
               TYPE PRIV (D : INTEGER) IS PRIVATE;
               TYPE ACPRV IS ACCESS PRIV;
               SUBTYPE ACPRV3 IS ACPRV (IDENT_INT (3));

          PRIVATE
               TYPE PRIV (D : INTEGER) IS
                    RECORD
                         NULL;
               END RECORD;
          END PKG1;
          
          USE PKG1;
          
          PACKAGE PKG2 IS
               A : ACPRV (IDENT_INT (0)) := NEW PRIV (IDENT_INT (0));
          END PKG2;

          USE PKG2;

          PROCEDURE PROC (A : ACPRV) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (A.D);
          END PROC;

     BEGIN
          PROC (ACPRV3 (A));
          FAILED ( "NO EXCEPTION RAISED FOR 'ACPRV3 (A)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'ACPRV3 (A)'" );
     END;

     RESULT;
END C46054A;
