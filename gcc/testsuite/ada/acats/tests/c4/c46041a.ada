-- C46041A.ADA

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
-- CHECK ARRAY CONVERSIONS WHEN THE TARGET TYPE IS AN UNCONSTRAINED 
-- ARRAY TYPE AND THE OPERAND TYPE REQUIRES CONVERSION OF THE INDEX
-- BOUNDS.

-- R.WILLIAMS 9/8/86

WITH REPORT; USE REPORT;
PROCEDURE C46041A IS
     
     TYPE INT IS RANGE -100 .. 100;
     TYPE NEWINTEGER IS NEW INTEGER;

     TYPE DAY IS (SUN, MON, TUE, WED, THU, FRI, SAT);

     TYPE NDAY1 IS NEW DAY RANGE SUN .. FRI;
     TYPE NDAY2 IS NEW DAY RANGE MON .. SAT;
     
     TYPE NNDAY1 IS NEW NDAY1;

     FUNCTION IDENT (X : INT) RETURN INT IS
     BEGIN
          RETURN INT'VAL (IDENT_INT (INT'POS (X)));
     END IDENT;

     FUNCTION IDENT (X : NEWINTEGER) RETURN NEWINTEGER IS
     BEGIN
          RETURN NEWINTEGER'VAL (IDENT_INT (NEWINTEGER'POS (X)));
     END IDENT;

     FUNCTION IDENT (X : NDAY1) RETURN NDAY1 IS
     BEGIN
          RETURN NDAY1'VAL (IDENT_INT (NDAY1'POS (X)));
     END IDENT;

     FUNCTION IDENT (X : NDAY2) RETURN NDAY2 IS
     BEGIN
          RETURN NDAY2'VAL (IDENT_INT (NDAY2'POS (X)));
     END IDENT;

     FUNCTION IDENT (X : NNDAY1) RETURN NNDAY1 IS
     BEGIN
          RETURN NNDAY1'VAL (IDENT_INT (NNDAY1'POS (X)));
     END IDENT;

BEGIN
     TEST ( "C46041A", "CHECK ARRAY CONVERSIONS WHEN THE TARGET " &
                       "TYPE IS AN UNCONSTRAINED ARRAY TYPE AND " &
                       "THE OPERAND TYPE REQUIRES CONVERSION OF " &
                       "THE INDEX BOUNDS" );

     DECLARE

          TYPE UNARR1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     
          TYPE UNARR2 IS ARRAY (INTEGER RANGE <>, NDAY1 RANGE <>) 
               OF INTEGER;
     
          TYPE ARR1 IS ARRAY (INT RANGE <>) OF INTEGER;
          A1 : ARR1 (IDENT (11) .. IDENT (20)) := 
                    (IDENT (11) .. IDENT (20) => 0);

          TYPE ARR2 IS ARRAY (INT RANGE <>, NDAY2 RANGE <>) 
               OF INTEGER;
          A2 : ARR2 (IDENT (11) .. IDENT (20), 
                     IDENT (TUE) .. IDENT (THU)) :=
                    (IDENT (11) .. IDENT (20) => 
                    (IDENT (TUE) .. IDENT (THU) => 0));
     
          TYPE ARR3 IS ARRAY (NEWINTEGER RANGE <>, NNDAY1 RANGE <>)
               OF INTEGER;
          A3 : ARR3 (IDENT (11) .. IDENT (20),
                     IDENT (TUE) .. IDENT (THU)) :=
                    (IDENT (11) .. IDENT (20) => 
                    (IDENT (TUE) .. IDENT (THU) => 0));

          PROCEDURE CHECK (A : UNARR1) IS
          BEGIN
               IF A'FIRST /= 11 OR A'LAST /= 20 THEN
                    FAILED ( "INCORRECT CONVERSION OF UNARR1 (A1)" );
               END IF;
          END CHECK;

          PROCEDURE CHECK (A : UNARR2; STR : STRING) IS
          BEGIN
               IF A'FIRST (1) /= 11 OR A'LAST /= 20 OR
                  A'FIRST (2) /= TUE OR A'LAST (2) /= THU THEN
                    FAILED ( "INCORRECT CONVERSION OF UNARR2 (A" & 
                              STR & ")" );
               END IF;
          END CHECK;

     BEGIN
          BEGIN
               CHECK (UNARR1 (A1));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED BY 'UNARR1 (A1)'" );
          END;
                             
          BEGIN
               CHECK (UNARR2 (A2), "2");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED BY 'UNARR2 (A2)'" );
          END;
                             
          BEGIN
               CHECK (UNARR2 (A3), "3");                             
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED BY 'UNARR2 (A3)'" );
          END;
                             
     END;

     RESULT;
END C46041A;
