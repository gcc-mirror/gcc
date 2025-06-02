-- C46043B.ADA

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
-- UNCONSTRAINED ARRAY TYPE IF, FOR A NON-NULL DIMENSION OF THE 
-- OPERAND TYPE, ONE BOUND DOES NOT BELONG TO THE CORRESPONDING INDEX
-- SUBTYPE OF THE TARGET TYPE.

-- R.WILLIAMS 9/8/86

WITH REPORT; USE REPORT;
PROCEDURE C46043B IS
          
     SUBTYPE SUBINT IS INTEGER RANGE IDENT_INT (0) .. IDENT_INT (9);

BEGIN
     TEST ( "C46043B", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                       "CONVERSION TO AN UNCONSTRAINED ARRAY TYPE " &
                       "IF, FOR A NON-NULL DIMENSION OF THE OPERAND " &
                       "TYPE, ONE BOUND DOES NOT BELONG TO THE " &
                       "CORRESPONDING INDEX SUBTYPE OF THE TARGET " &
                       "TYPE" );

     DECLARE
          TYPE ARR1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          A1 : ARR1 (IDENT_INT (1) .. IDENT_INT (10));

          TYPE ARR2 IS ARRAY (SUBINT RANGE <>) OF INTEGER;
          
          PROCEDURE CHECK (A : ARR2) IS
          BEGIN               
               FAILED ( "NO EXCEPTION RAISED WITH ONE DIMENSIONAL " &
                        "ARRAYS" );
          END CHECK;
               
     BEGIN
          A1 := (A1'RANGE => 0);
          CHECK (ARR2 (A1));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WITH ONE " &
                        "DIMENSIONAL ARRAYS" );
     END;

     DECLARE
          TYPE ARR1 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          A1 : ARR1 (IDENT_INT (1) .. IDENT_INT (10), 
                     IDENT_INT (1) .. IDENT_INT (1));

          TYPE ARR2 IS ARRAY (SUBINT RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          
          PROCEDURE CHECK (A : ARR2) IS
          BEGIN               
               FAILED ( "NO EXCEPTION RAISED WITH TWO DIMENSIONAL " &
                        "ARRAYS" );
          END CHECK;
               
     BEGIN
          A1 := (A1'RANGE (1) => (A1'RANGE (2) => 0));
          CHECK (ARR2 (A1));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WITH TWO " &
                        "DIMENSIONAL ARRAYS" );
     END;

     DECLARE
          TYPE ARR1 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          A1 : ARR1 (IDENT_INT (1) .. IDENT_INT (10), 
                     IDENT_INT (1) .. IDENT_INT (0));

          TYPE ARR2 IS ARRAY (SUBINT RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          
          PROCEDURE CHECK (A : ARR2) IS
          BEGIN               
               FAILED ( "NO EXCEPTION RAISED WITH NULL ARRAYS - 1" );
          END CHECK;
               
     BEGIN
          A1 := (A1'RANGE (1) => (A1'RANGE (2) => 0));
          CHECK (ARR2 (A1));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " & 
                        "WITH NULL ARRAYS - 1" );
     END;

     DECLARE
          TYPE ARR1 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF 
               INTEGER;
          A1 : ARR1 (IDENT_INT (1) .. IDENT_INT (10), 
                     IDENT_INT (1) .. IDENT_INT (0));

          SUBTYPE NOINT IS INTEGER 
               RANGE IDENT_INT (1) .. IDENT_INT (0);

          TYPE ARR2 IS ARRAY (SUBINT RANGE <>, NOINT RANGE <>) OF 
               INTEGER;
          
          PROCEDURE CHECK (A : ARR2) IS
          BEGIN               
               FAILED ( "NO EXCEPTION RAISED WITH NULL ARRAYS - 2" );
          END CHECK;
               
     BEGIN
          A1 := (A1'RANGE (1) => (A1'RANGE (2) => 0));
          CHECK (ARR2 (A1));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " & 
                        "WITH NULL ARRAYS - 2" );
     END;

     RESULT;
END C46043B;
