-- CDA201C.ADA

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
--     CHECK THAT UNCHECKED_CONVERSION CAN BE INSTANTIATED FOR
--     CONVERSION BETWEEN CONSTRAINED ARRAY AND RECORD TYPES.

-- HISTORY:
--     JET 09/12/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.

WITH REPORT; USE REPORT;
WITH UNCHECKED_CONVERSION;
PROCEDURE CDA201C IS

     TYPE INT IS NEW INTEGER;

     TYPE ARR IS ARRAY (1..2) OF INTEGER;
     TYPE ARR2 IS ARRAY (ARR'RANGE) OF INT;

     TYPE REC IS RECORD
          D : INTEGER;
          I : INTEGER;
     END RECORD;

     TYPE REC2 IS RECORD
          D : INT;
          I : INT;
     END RECORD;

     A : ARR2;
     R : REC2;

     FUNCTION ARR_CONV IS NEW UNCHECKED_CONVERSION(ARR, ARR2);
     FUNCTION REC_CONV IS NEW UNCHECKED_CONVERSION(REC, REC2);

BEGIN
     TEST ("CDA201C", "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
                      "INSTANTIATED FOR CONVERSION BETWEEN " &
                      "CONSTRAINED ARRAY AND RECORD TYPES");

     A := ARR_CONV(ARR'(ARR'RANGE => IDENT_INT(-1)));

     IF A /= ARR2'(ARR'RANGE => -1) THEN
          FAILED("INCORRECT RESULT FROM ARRAY CONVERSION");
     END IF;

     R := REC_CONV(REC'(D | I => IDENT_INT(1)));

     IF R /= REC2'(D => 1, I => 1) THEN
          FAILED("INCORRECT RESULT FROM RECORD CONVERSION");
     END IF;

     RESULT;
END CDA201C;
