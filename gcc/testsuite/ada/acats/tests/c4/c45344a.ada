-- C45344A.ADA

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
-- CHECK THAT THE CORRECT RESULT IS PRODUCED WHEN A FUNCTION RETURNS 
-- THE RESULT OF A CATENATION WHOSE BOUNDS ARE NOT DEFINED STATICALLY.

-- R.WILLIAMS 9/1/86

WITH REPORT; USE REPORT;
PROCEDURE C45344A IS

BEGIN
     TEST ( "C45344A", "CHECK THAT THE CORRECT RESULT IS PRODUCED " &
                       "WHEN A FUNCTION RETURNS THE RESULT OF A " &
                       "CATENATION WHOSE BOUNDS ARE NOT DEFINED " &
                       "STATICALLY" );

     DECLARE
          SUBTYPE INT IS INTEGER RANGE IDENT_INT (1) .. IDENT_INT (30);

          TYPE ARR IS ARRAY (INT RANGE <>) OF INTEGER;
          SUBTYPE CARR IS ARR (1 .. 9);
          C : CARR;
          
          AR1 : ARR (IDENT_INT (2) .. IDENT_INT (4)) :=
                    (IDENT_INT (2) .. IDENT_INT (4) => 1);

          AR2 : ARR (IDENT_INT (6) .. IDENT_INT (6)) :=
                    (IDENT_INT (6) .. IDENT_INT (6) => 2);

          AR3 : ARR (IDENT_INT (4) .. IDENT_INT (2));
     
          FUNCTION F (A, B : ARR; N : NATURAL) RETURN ARR IS
          BEGIN
               IF N = 0 THEN
                    RETURN A & B;
               ELSE
                    RETURN F (A & B, B, N - 1);
               END IF;
          END F;
     
          FUNCTION G (A : INTEGER; B : ARR; N : NATURAL) RETURN ARR IS
          BEGIN
               IF N = 0 THEN
                    RETURN A & B;
               ELSE
                    RETURN G (A, A & B, N - 1);
               END IF;
          END G;

          FUNCTION H (A : ARR; B : INTEGER; N : NATURAL) RETURN ARR IS
          BEGIN
               IF N = 0 THEN
                    RETURN A & B;
               ELSE
                    RETURN H (A & B, B, N - 1);
               END IF;
          END H;
     
          PROCEDURE CHECK (X, Y : ARR; F, L : INTEGER; STR : STRING) IS
               OK : BOOLEAN := TRUE;
          BEGIN
               IF X'FIRST /= F AND X'LAST /= L THEN
                    FAILED ( "INCORRECT RANGE FOR " & STR);
               ELSE
                    FOR I IN F .. L LOOP
                         IF X (I) /= Y (I) THEN
                              OK := FALSE;
                         END IF;
                    END LOOP;
     
                    IF NOT OK THEN
                         FAILED ( "INCORRECT VALUE FOR " & STR);
                    END IF;
               END IF;
          END CHECK;

     BEGIN
          C := (1 .. 4 => 1, 5 .. 9 => 2);
          CHECK (F (AR1, AR2, IDENT_INT (3)), C, 2, 8, "F - 1" );
          CHECK (F (AR3, AR2, IDENT_INT (3)), C, 6, 9, "F - 2" );
          CHECK (F (AR2, AR3, IDENT_INT (3)), C, 6, 6, "F - 3" );

          C := (1 ..4 => 5, 5 .. 9 => 1);
          CHECK (G (5, AR1, IDENT_INT (3)), C, 1, 7, "G - 1" );
          CHECK (G (5, AR3, IDENT_INT (3)), C, 1, 4, "G - 2" );

          CHECK (H (AR3, 5, IDENT_INT (3)), C, 1, 4, "H - 1" );

          C := (1 ..4 => 1, 5 .. 9 => 5);
          CHECK (H (AR1, 5, IDENT_INT (3)), C, 2, 8, "H - 2" );
     END;

     RESULT;
END C45344A;
