-- A34017C.ADA

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
-- CHECK THAT IF A DERIVED TYPE DEFINITION IS GIVEN IN THE VISIBLE PART
-- OF A PACKAGE, THE TYPE MAY BE USED AS THE PARENT TYPE IN A DERIVED
-- TYPE DEFINITION IN THE PRIVATE PART OF THE PACKAGE AND IN THE BODY.

-- CHECK THAT IF A TYPE IS DECLARED IN THE VISIBLE PART OF A PACKAGE,
-- AND IS NOT A DERIVED TYPE OR A PRIVATE TYPE, IT MAY BE USED AS THE
-- PARENT TYPE IN A DERIVED TYPE DEFINITION IN THE VISIBLE PART, PRIVATE
-- PART, AND BODY.


-- DSJ 4/27/83


WITH REPORT;
PROCEDURE A34017C IS

     USE REPORT;

BEGIN

     TEST( "A34017C", "CHECK THAT A DERIVED TYPE MAY BE USED AS A " &
                      "PARENT TYPE IN THE PRIVATE PART AND BODY.  " &
                      "CHECK THAT OTHER TYPES MAY BE USED AS PARENT " &
                      "TYPES IN VISIBLE PART ALSO");

     DECLARE

          TYPE REC IS
               RECORD
                    C : INTEGER;
               END RECORD;

          PACKAGE PACK1 IS

               TYPE T1 IS RANGE 1 .. 10;
               TYPE T2 IS NEW REC;

               TYPE T3 IS (A,B,C);
               TYPE T4 IS ARRAY ( 1 .. 2 ) OF INTEGER;
               TYPE T5 IS
                    RECORD
                         X : CHARACTER;
                    END RECORD;
               TYPE T6 IS ACCESS INTEGER;

               TYPE N1 IS NEW T3;
               TYPE N2 IS NEW T4;
               TYPE N3 IS NEW T5;
               TYPE N4 IS NEW T6;

          PRIVATE

               TYPE P1 IS NEW T1;
               TYPE P2 IS NEW T2;
               TYPE P3 IS NEW T3;
               TYPE P4 IS NEW T4;
               TYPE P5 IS NEW T5;
               TYPE P6 IS NEW T6;

          END PACK1;

          PACKAGE BODY PACK1 IS
     
               TYPE Q1 IS NEW T1;
               TYPE Q2 IS NEW T2;
               TYPE Q3 IS NEW T3;
               TYPE Q4 IS NEW T4;
               TYPE Q5 IS NEW T5;
               TYPE Q6 IS NEW T6;

          END PACK1;

     BEGIN

          NULL;

     END;

     RESULT;

END A34017C;
