-- CD3021A.ADA

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
--     CHECK THAT THE AGGREGATE IN AN ENUMERATION REPRESENTATION CLAUSE
--     IS NOT AMBIGUOUS EVEN IF THERE ARE SEVERAL ONE-DIMENSIONAL ARRAY
--     TYPES WITH THE ENUMERATION TYPE AS THE INDEX SUBTYPE.

-- HISTORY:
--     BCB 09/30/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP', CORRECTED
--                    CHECKS FOR FAILURE.

WITH REPORT;  USE REPORT;

PROCEDURE CD3021A IS

     TYPE ENUM IS (A,B,C);

     TYPE ARR1 IS ARRAY(ENUM) OF INTEGER;
     TYPE ARR2 IS ARRAY(ENUM) OF INTEGER;
     TYPE ARR3 IS ARRAY(ENUM) OF INTEGER;

     FOR ENUM USE (A => 1,B => 2,C => 3);

     A1 : ARR1 := (A => 5,B => 6,C => 13);
     A2 : ARR2 := (A => 1,B => 2,C => 3);
     A3 : ARR3 := (A => 0,B => 1,C => 2);

BEGIN

     TEST ("CD3021A", "CHECK THAT THE AGGREGATE IN AN ENUMERATION " &
                      "REPRESENTATION CLAUSE IS NOT AMBIGUOUS EVEN " &
                      "IF THERE ARE SEVERAL ONE-DIMENSIONAL ARRAY " &
                      "TYPES WITH THE ENUMERATION TYPE AS THE INDEX " &
                      "SUBTYPE");

     IF (A1 /= (IDENT_INT (5), IDENT_INT (6), IDENT_INT (13))) OR
          (A2 /= (IDENT_INT (1), IDENT_INT (2), IDENT_INT (3))) OR
          (A3 /= (IDENT_INT (0), IDENT_INT (1), IDENT_INT (2))) THEN
          FAILED ("INCORRECT VALUES FOR ARRAYS");
     END IF;

     RESULT;
END CD3021A;
