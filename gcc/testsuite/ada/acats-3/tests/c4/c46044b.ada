-- C46044B.ADA

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
-- CHECK THAT CONSTRAINT ERROR IS RAISED FOR CONVERSION TO A  
-- CONSTRAINED ARRAY TYPE IF THE TARGET TYPE IS NON-NULL AND
-- CORRESPONDING DIMENSIONS OF THE TARGET AND OPERAND DO NOT HAVE
-- THE SAME LENGTH. ALSO, CHECK THAT CONSTRAINT_ERROR IS RAISED IF
-- THE TARGET TYPE IS NULL AND THE OPERAND TYPE IS NON-NULL.

-- R.WILLIAMS 9/8/86

WITH REPORT; USE REPORT;
PROCEDURE C46044B IS

     TYPE ARR1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;

     SUBTYPE CARR1A IS ARR1 (IDENT_INT (1) .. IDENT_INT (6));
     C1A : CARR1A := (CARR1A'RANGE => 0);

     SUBTYPE CARR1B IS ARR1 (IDENT_INT (2) .. IDENT_INT (5));
     C1B : CARR1B := (CARR1B'RANGE => 0);

     SUBTYPE CARR1N IS ARR1 (IDENT_INT (1) .. IDENT_INT (0));
     C1N : CARR1N := (CARR1N'RANGE => 0);

     TYPE ARR2 IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <>) OF 
          INTEGER;

     SUBTYPE CARR2A IS ARR2 (IDENT_INT (1) .. IDENT_INT (2),
                             IDENT_INT (1) .. IDENT_INT (2));
     C2A : CARR2A := (CARR2A'RANGE (1) => (CARR2A'RANGE (2) => 0));

     SUBTYPE CARR2B IS ARR2 (IDENT_INT (0) .. IDENT_INT (2),
                             IDENT_INT (0) .. IDENT_INT (2));
     C2B : CARR2B := (CARR2B'RANGE (1) => (CARR2B'RANGE (2) => 0));

     SUBTYPE CARR2N IS ARR2 (IDENT_INT (2) .. IDENT_INT (1),
                             IDENT_INT (1) .. IDENT_INT (2));
     C2N : CARR2N := (CARR2N'RANGE (1) => (CARR2N'RANGE (2) => 0));
     
     PROCEDURE CHECK1 (A : ARR1; STR : STRING) IS
     BEGIN
          FAILED ( "NO EXCEPTION RAISED - " & STR );
     END CHECK1;

     PROCEDURE CHECK2 (A : ARR2; STR : STRING) IS
     BEGIN
          FAILED ( "NO EXCEPTION RAISED - " & STR );
     END CHECK2;

BEGIN
     TEST ( "C46044B", "CHECK THAT CONSTRAINT ERROR IS RAISED FOR " &
                       "CONVERSION TO A CONSTRAINED ARRAY TYPE " &
                       "IF THE TARGET TYPE IS NON-NULL AND " &
                       "CORRESPONDING DIMENSIONS OF THE TARGET AND " &
                       "OPERAND DO NOT HAVE THE SAME LENGTH. " &
                       "ALSO, CHECK THAT CONSTRAINT_ERROR IS " &
                       "RAISED IF THE TARGET TYPE IS NULL AND " &
                       "THE OPERAND TYPE IS NON-NULL" );

     BEGIN -- (A).
          C1A := C1B;
          CHECK1 (C1A, "(A)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (A)" );
     END;

     BEGIN -- (B).
          CHECK1 (CARR1A (C1B), "(B)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (B)" );
     END; 

     BEGIN -- (C).
          C1B := C1A;
          CHECK1 (C1B, "(C)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (C)" );
     END;

     BEGIN -- (D).
          CHECK1 (CARR1B (C1A), "(D)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (D)" );
     END;

     BEGIN -- (E).
          C1A := C1N;
          CHECK1 (C1A, "(E)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (E)" );
     END;

     BEGIN -- (F).
          CHECK1 (CARR1A (C1N), "(F)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (F)" );
     END;

     BEGIN -- (G).
          C2A := C2B;
          CHECK2 (C2A, "(G)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (G)" );
     END;

     BEGIN -- (H).
          CHECK2 (CARR2A (C2B), "(H)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (H)" );
     END;

     BEGIN -- (I).
          C2B := C2A;
          CHECK2 (C2B, "(I)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (I)" );
     END;

     BEGIN -- (J).
          CHECK2 (CARR2A (C2B), "(J)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (J)" );
     END;

     BEGIN -- (K).
          C2A := C2N;
          CHECK2 (C2A, "(K)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (K)" );
     END;

     BEGIN -- (L).
          CHECK2 (CARR2A (C2N), "(L)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (L)" );
     END;

     BEGIN -- (M).
          C1N := C1A;
          CHECK1 (C1N, "(M)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (M)" );
     END;

     BEGIN -- (N).
          CHECK1 (CARR1N (C1A), "(N)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (N)" );
     END;

     BEGIN -- (O).
          C2N := C2A;
          CHECK2 (C2N, "(O)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (O)" );
     END;

     BEGIN -- (P).
          CHECK2 (CARR2N (C2A), "(P)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - (P)" );
     END;

     RESULT;
END C46044B;
