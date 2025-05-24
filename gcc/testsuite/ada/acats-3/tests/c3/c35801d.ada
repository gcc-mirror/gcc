-- C35801D.ADA

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
-- CHECK THAT THE ATTRIBUTES FIRST AND LAST RETURN VALUES HAVING THE
-- SAME BASE TYPE AS THE PREFIX WHEN THE PREFIX IS A GENERIC FORMAL 
-- SUBTYPE WHOSE ACTUAL ARGUMENT IS A FLOATING POINT TYPE.

-- R.WILLIAMS 8/21/86

WITH REPORT; USE REPORT;
PROCEDURE C35801D IS
     TYPE REAL IS DIGITS 3 RANGE -100.0 .. 100.0;

     TYPE NFLT IS NEW FLOAT;

     GENERIC
          TYPE F IS DIGITS <>;
     PROCEDURE P (STR : STRING);

     PROCEDURE P (STR : STRING) IS

          SUBTYPE SF IS F RANGE -1.0 .. 1.0;
          F1 : SF := 0.0;
          F2 : SF := 0.0;
          
     BEGIN
          IF EQUAL (3, 3) THEN
               F1 := SF'FIRST;
               F2 := SF'LAST;
          END IF;

          IF F1 /= -1.0 OR F2 /= 1.0 THEN
               FAILED ( "WRONG RESULTS FROM " & STR & "'FIRST OR " &
                        STR & "'LAST" );
          END IF;
     END P;
     
     PROCEDURE NP1 IS NEW P (FLOAT);

     PROCEDURE NP2 IS NEW P (NFLT);

     PROCEDURE NP3 IS NEW P (REAL);

BEGIN
     TEST ( "C35801D", "CHECK THAT THE ATTRIBUTES FIRST AND " &
                       "LAST RETURN VALUES HAVING THE SAME " &
                       "BASE TYPE AS THE PREFIX WHEN THE " &
                       "PREFIX IS A GENERIC FORMAL SUBTYPE " &
                       "WHOSE ACTUAL ARGUMENT IS A FLOATING " &
                       "POINT TYPE" );


     NP1 ("FLOAT");
     NP2 ("NFLT");
     NP3 ("REAL");

     RESULT;     
END C35801D;
