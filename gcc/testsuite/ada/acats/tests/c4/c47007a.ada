-- C47007A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A CONSTRAINED 
-- ARRAY TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE BOUNDS 
-- OF THE OPERAND ARE NOT THE SAME AS THE BOUNDS OF THE TYPE MARK.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47007A IS

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;

     TYPE TARR IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) 
          OF INTEGER;

     TYPE NARR IS NEW ARR;

     TYPE NTARR IS NEW TARR;

BEGIN

     TEST( "C47007A", "WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION " &
                      "DENOTES A CONSTRAINED ARRAY TYPE, CHECK THAT " &
                      "CONSTRAINT_ERROR IS RAISED WHEN THE BOUNDS " &
                      "OF THE OPERAND ARE NOT THE SAME AS THE " &
                      "BOUNDS OF THE TYPE MARK" );

     DECLARE  
          
          SUBTYPE SARR IS ARR (IDENT_INT (1) .. IDENT_INT (1));
          A : ARR (IDENT_INT (2) .. IDENT_INT (2));
     BEGIN
          A := SARR'(A'RANGE => 0);
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE SARR" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE SARR" );
     END;

     DECLARE  
          
          SUBTYPE NULLA IS ARR (IDENT_INT (1) .. IDENT_INT (0));
          A : ARR (IDENT_INT (2) .. IDENT_INT (1));

     BEGIN
          A := NULLA'(A'FIRST .. A'LAST => 0);
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE NULLA" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE NULLA" );
     END;

     DECLARE  

          SUBTYPE STARR IS TARR (IDENT_INT (1) .. IDENT_INT (1), 
                                 IDENT_INT (1) .. IDENT_INT (5));
          A : TARR (IDENT_INT (2) .. IDENT_INT (6),
                    IDENT_INT (1) .. IDENT_INT (1));
     BEGIN
          A := STARR'(A'RANGE => (A'RANGE (2) => 0));
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE STARR" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE STARR" );
     END;

     DECLARE  
          
          SUBTYPE NULLT IS TARR (IDENT_INT (1) .. IDENT_INT (5), 
                                 IDENT_INT (1) .. IDENT_INT (0));

          A : TARR (IDENT_INT (1) .. IDENT_INT (5),
                    IDENT_INT (2) .. IDENT_INT (1));
     BEGIN
          A := NULLT'(A'FIRST .. A'LAST  => 
                     (A'FIRST (2) .. A'LAST (2) => 0));
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE NULLT" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE NULLT" );
     END;

     DECLARE  
          
          SUBTYPE SNARR IS NARR (IDENT_INT (1) .. IDENT_INT (1));
          A : NARR (IDENT_INT (2) .. IDENT_INT (2));

     BEGIN
          A := SNARR'(A'RANGE => 0);
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE SNARR" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE SNARR" );
     END;

     DECLARE  
          
          SUBTYPE NULLNA IS NARR (IDENT_INT (1) .. IDENT_INT (0));
          A : NARR (IDENT_INT (2) .. IDENT_INT (1));

     BEGIN
          A := NULLNA'(A'RANGE => 0);
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE NULLNA" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE NULLNA" );
     END;

     DECLARE  
          
          SUBTYPE SNTARR IS NTARR (IDENT_INT (1) .. IDENT_INT (1), 
                                   IDENT_INT (1) .. IDENT_INT (5));

          A : NTARR (IDENT_INT (2) .. IDENT_INT (2),
                     IDENT_INT (1) .. IDENT_INT (5));
     BEGIN
          A := SNTARR'(A'RANGE => (A'RANGE (2) => 0));
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE SNTARR" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE SNTARR" );
     END;

     DECLARE  
          
          SUBTYPE NULLNT IS NTARR (IDENT_INT (1) .. IDENT_INT (5), 
                                   IDENT_INT (1) .. IDENT_INT (0));

          A : NTARR (IDENT_INT (1) .. IDENT_INT (5),
                    IDENT_INT (1) .. IDENT_INT (1));
     BEGIN
          A := NULLNT'(A'RANGE => (A'RANGE (2) => 0));
          FAILED ( "NO EXCEPTION RAISED WHEN BOUNDS NOT THE SAME AS " &
                   "THOSE OF SUBTYPE NULLNT" );
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED WHEN BOUNDS NOT " &
                        "THE SAME AS THOSE OF SUBTYPE NULLNT" );
     END;

     RESULT;
END C47007A;
