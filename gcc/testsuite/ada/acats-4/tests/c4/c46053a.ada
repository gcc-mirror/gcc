-- C46053A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR CONVERSION TO A 
-- CONSTRAINED RECORD, PRIVATE, OR LIMITED PRIVATE SUBTYPE IF THE 
-- DISCRIMINANTS OF THE TARGET SUBTYPE DO NOT EQUAL THOSE OF THE 
-- OPERAND.

-- R.WILLIAMS 9/9/86

WITH REPORT; USE REPORT;
PROCEDURE C46053A IS
     
BEGIN
     TEST ( "C46053A", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                       "CONVERSION TO A CONSTRAINED RECORD, " &
                       "PRIVATE, OR LIMITED PRIVATE SUBTYPE IF " &
                       "THE DISCRIMINANTS OF THE TARGET SUBTYPE DO " &
                       "NOT EQUAL THOSE OF THE OPERAND" );

     DECLARE
          TYPE REC (D : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          
          SUBTYPE REC3 IS REC (IDENT_INT (3));
          R : REC (IDENT_INT (1));

          PROCEDURE PROC (R : REC) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (R.D);
          END PROC;
               
     BEGIN
          PROC (REC3 (R));
          FAILED ( "NO EXCEPTION RAISED FOR 'REC3 (R)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'REC3 (R)'" );
     END;

     DECLARE
          PACKAGE PKG1 IS
               TYPE PRIV (D : INTEGER) IS PRIVATE;
               SUBTYPE PRIV3 IS PRIV (IDENT_INT (3));
          PRIVATE
               TYPE PRIV  (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;
          
          USE PKG1;

          PACKAGE PKG2 IS
               P : PRIV (IDENT_INT (0));
          END PKG2;

          USE PKG2;

          PROCEDURE PROC (P : PRIV) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (P.D);
          END PROC;
               
     BEGIN
          PROC (PRIV3 (P));
          FAILED ( "NO EXCEPTION RAISED FOR 'PRIV3 (P)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'PRIV3 (P)'" );
     END;

     DECLARE
          PACKAGE PKG1 IS
               TYPE LIM (D : INTEGER) IS LIMITED PRIVATE;
               SUBTYPE LIM3 IS LIM (IDENT_INT (3));
          PRIVATE
               TYPE LIM  (D : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;
          
          USE PKG1;

          PACKAGE PKG2 IS
               L : LIM (IDENT_INT (0));
               I : INTEGER;
          END PKG2;

          USE PKG2;

          PROCEDURE PROC (L : LIM) IS
               I : INTEGER;
          BEGIN
               I := IDENT_INT (L.D);
          END PROC;
               
     BEGIN
          PROC (LIM3 (L));
          FAILED ( "NO EXCEPTION RAISED FOR 'LIM3 (L)'" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR 'LIM3 (L)'" );
     END;

     RESULT;
END C46053A;
