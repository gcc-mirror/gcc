-- C35704D.ADA

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
-- CHECK THAT A COMBINATION OF FIXED AND FLOAT CAN BE USED IN A
-- FLOATING POINT RANGE CONSTRAINT IN A TYPE DEFINITION.

-- JCR 4/7/82

WITH REPORT;
PROCEDURE C35704D IS

     USE REPORT;

BEGIN
     TEST ("C35704D","MIXED FIXED AND FLOAT IN FLOATING " &
           "POINT RANGE CONSTRAINT IN A TYPE DEFINITION");

     DECLARE

          TYPE F IS DIGITS 5;
          TYPE R IS DELTA 0.5 RANGE -5.0 .. 5.0;

          T1 : CONSTANT F := -4.0;
          T2 : CONSTANT F := 4.0;

          R1 : CONSTANT R := -4.0;
          R2 : CONSTANT R := 4.0;

          TYPE G1 IS DIGITS 5 RANGE T1..R2;
          TYPE G2 IS DIGITS 5 RANGE R1..T2;

     BEGIN

          IF (ABS(G1'FIRST)- 4.0) /= 0.0  OR
             (ABS(G1'LAST) - 4.0) /= 0.0  OR
             (ABS(G2'FIRST)- 4.0) /= 0.0  OR
             (ABS(G2'LAST) - 4.0) /= 0.0
     
          THEN FAILED ("MIXED FIXED AND FLOAT IN FLOAT RANGE " &
                       "CONSTRAINT");
     
          END IF;

     END;

     RESULT;
 

END C35704D;
