-- C41207A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE DISCRETE RANGE IN A SLICE CAN HAVE THE FORM
--     A'RANGE, WHERE A IS A CONSTRAINED ARRAY SUBTYPE OR AN ARRAY
--     OBJECT.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C41207A IS

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;

     SUBTYPE A1 IS ARR(1..5);

     ARR_VAR : ARR(1..10) := (90,91,92,93,94,95,96,97,98,99);

     A2 : ARRAY(1..5) OF INTEGER := (80,81,82,83,84);

BEGIN
     TEST ("C41207A", "CHECK THAT THE DISCRETE RANGE IN A SLICE CAN " &
                      "HAVE THE FORM A'RANGE, WHERE A IS A " &
                      "CONSTRAINED ARRAY SUBTYPE OR AN ARRAY OBJECT");

     ARR_VAR (A1'RANGE) := (1,2,3,4,5);

     IF NOT (EQUAL(ARR_VAR(1),1) AND EQUAL(ARR_VAR(2),2) AND
         EQUAL(ARR_VAR(3),3) AND EQUAL(ARR_VAR(4),4) AND
         EQUAL(ARR_VAR(5),5)) THEN
          FAILED ("IMPROPER RESULT FROM SLICE ASSIGNMENT USING THE " &
                  "RANGE OF A CONSTRAINED ARRAY SUBTYPE");
     END IF;

     ARR_VAR (A2'RANGE) := (6,7,8,9,10);

     IF (NOT EQUAL(ARR_VAR(1),6) OR NOT EQUAL(ARR_VAR(2),7) OR
         NOT EQUAL(ARR_VAR(3),8) OR NOT EQUAL(ARR_VAR(4),9) OR
         NOT EQUAL(ARR_VAR(5),10)) THEN
          FAILED ("IMPROPER RESULT FROM SLICE ASSIGNMENT USING THE " &
                  "RANGE OF AN ARRAY OBJECT");
     END IF;

     RESULT;
END C41207A;
