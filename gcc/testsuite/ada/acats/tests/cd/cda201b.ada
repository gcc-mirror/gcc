-- CDA201B.ADA

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
--     CONVERSION BETWEEN FLOAT AND BOOLEAN ARRAY TYPES.

-- HISTORY:
--     JET 09/12/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.
--     GJD 11/15/95  REMOVED USE OF OBSOLETE ADA 83 ATTRIBUTE (LARGE).

WITH REPORT; USE REPORT;
WITH UNCHECKED_CONVERSION;
PROCEDURE CDA201B IS

     TYPE BOOL_ARR IS ARRAY (1..FLOAT'SIZE) OF BOOLEAN;
     PRAGMA PACK (BOOL_ARR);

     B : BOOL_ARR;

     FUNCTION FLT_TO_BOOL IS NEW UNCHECKED_CONVERSION(FLOAT, BOOL_ARR);

     FUNCTION BOOL_TO_FLT IS NEW UNCHECKED_CONVERSION(BOOL_ARR, FLOAT);

BEGIN
     TEST ("CDA201B", "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
                      "INSTANTIATED FOR CONVERSION BETWEEN " &
                      "FLOAT AND BOOLEAN ARRAY TYPES");

     B := FLT_TO_BOOL(FLOAT'LAST + FLOAT(IDENT_INT(0)));

     FOR J IN B'RANGE LOOP
          B(J) := B(J+IDENT_INT(0));
     END LOOP;

     IF BOOL_TO_FLT(B) /= FLOAT'LAST THEN
          FAILED("INCORRECT RESULT FROM FLOAT-ARRAY-FLOAT");
     END IF;

     RESULT;
END CDA201B;
