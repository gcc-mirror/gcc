-- C45342A.ADA

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
-- CHECK THAT CATENATION OF TWO OR MORE NON-NULL OPERANDS YIELDS THE
-- CORRECT RESULT, WITH THE CORRECT BOUNDS, WHETHER BOUNDS ARE STATIC OR
-- DYNAMIC.

-- BHS 6/27/84

WITH REPORT;
PROCEDURE C45342A IS

     USE REPORT;

     SUBTYPE S IS INTEGER RANGE 1..100;
     TYPE ARR IS ARRAY (S RANGE <>) OF INTEGER;
     
     A,B : ARR (2..9);

     FUNCTION F (AR_VAR1, AR_VAR2, AR_VAR3 : ARR) RETURN ARR IS
     BEGIN
          RETURN AR_VAR1 & AR_VAR2 & AR_VAR3;
     END F;

     PROCEDURE CAT (A : ARR; I1,I2 : INTEGER; NUM : CHARACTER) IS
     BEGIN
          IF A'FIRST /= I1 OR A'LAST /= I2 THEN
               FAILED ("INCORRECT CATENATION BOUNDS - " & NUM);
          END IF;
     END CAT;


BEGIN

     TEST ("C45342A", "CHECK THAT CATENATION OF NON-NULL OPERANDS " &
                      "YIELDS CORRECT RESULT WITH CORRECT BOUNDS"); 

     BEGIN
          A := (1,2,3,4,5,6,7,8);
          B := A(2..4) & A(2..5) & A(2..2);
          IF B /= (1,2,3,1,2,3,4,1) THEN
               FAILED ("INCORRECT CATENATION RESULT - 1");
          END IF;

          A := (8,7,6,5,4,3,2,1);
          IF F(A(2..3), A(2..4), A(2..4)) /= (8,7,8,7,6,8,7,6) THEN 
               FAILED ("INCORRECT CATENATION RESULT - 2");
          END IF;

          CAT ( A(3..5) & A(2..3), 3, 7, '3' );
     END;


     DECLARE
          DYN2 : INTEGER := IDENT_INT(2);
          DYN3 : INTEGER := IDENT_INT(3);
          DYN4 : INTEGER := IDENT_INT(4);
          DYN6 : INTEGER := IDENT_INT(6);
          
     BEGIN
          A := (1,2,3,4,5,6,7,8);
          B := A(DYN2..DYN3) & A(DYN2..DYN4) & A(DYN2..DYN4);
          IF B /= (1,2,1,2,3,1,2,3) THEN
               FAILED ("INCORRECT CATENATION RESULT - 4");
          END IF;

          A := (8,7,6,5,4,3,2,1);
          IF F ( A(DYN2..DYN6), A(DYN2..DYN3), A(DYN2..DYN2) )
                /= (8,7,6,5,4,8,7,8) THEN
               FAILED ("INCORRECT CATENATION RESULT - 5");
          END IF;

          CAT ( A(DYN3..5) & A(2..3), 3, 7, '6');
     END;  

     RESULT;

END C45342A;
