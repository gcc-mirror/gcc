-- C37002A.ADA

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
-- CHECK THAT INDEX CONSTRAINTS WITH NON-STATIC EXPRESSIONS CAN BE
-- USED TO CONSTRAIN RECORD COMPONENTS HAVING AN ARRAY TYPE.

-- RJW 2/28/86 

WITH REPORT; USE REPORT;

PROCEDURE C37002A IS

BEGIN
     TEST ( "C37002A", "CHECK THAT INDEX CONSTRAINTS WITH " &
                       "NON-STATIC EXPRESSIONS CAN BE USED TO " &
                       "CONSTRAIN RECORD COMPONENTS HAVING AN " &
                       "ARRAY TYPE" );

     DECLARE
          X : INTEGER := IDENT_INT(5);
          SUBTYPE S IS INTEGER RANGE 1 .. X;
          TYPE AR1 IS ARRAY (S) OF INTEGER;

          SUBTYPE T IS INTEGER RANGE X .. 10;
          TYPE AR2 IS ARRAY (T) OF INTEGER;
          TYPE U IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          SUBTYPE V IS INTEGER RANGE 1 .. 10;     

          TYPE R IS 
               RECORD
                    A : STRING (1 .. X);              
                    B : STRING (X .. 10);             
                    C : AR1;                          
                    D : AR2;                          
                    E : STRING (S);
                    F : U(T);
                    G : U(V RANGE 1 ..X);
                    H : STRING (POSITIVE RANGE X .. 10);
                    I : U(AR1'RANGE);
                    J : STRING (AR2'RANGE);
               END RECORD;
          RR : R;
 
     BEGIN
          IF RR.A'LAST /= 5 OR RR.B'FIRST /= 5 OR
             RR.C'LAST /= 5 OR RR.D'FIRST /= 5 OR
             RR.E'LAST /= 5 OR RR.F'FIRST /= 5 OR
             RR.G'LAST /= 5 OR RR.H'FIRST /= 5 OR
             RR.I'LAST /= 5 OR RR.J'FIRST /= 5 THEN 
          
                  FAILED("WRONG VALUE FOR NON-STATIC BOUND");

           END IF;
          
     END;

     RESULT;
END C37002A;
