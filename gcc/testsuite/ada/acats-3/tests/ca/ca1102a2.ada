-- CA1102A2M.ADA

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
-- CHECK THAT MORE THAN ONE WITH_CLAUSE CAN APPEAR IN
--   A CONTEXT_SPECIFICATION.
-- CHECK THAT USE_CLAUSES CAN MENTION NAMES MADE
--   VISIBLE BY PRECEDING WITH_CLAUSES IN THE SAME
--   CONTEXT_SPECIFICATION.
-- CHECK THAT CONSECUTIVE USE_CLAUSES ARE ALLOWED.

-- SEPARATE FILES ARE:
--   CA1102A0  A LIBRARY PACKAGE DECLARATION.
--   CA1102A1  A LIBRARY PACKAGE BODY (CA1102A0).
--   CA1102A2M THE MAIN PROCEDURE.

-- WKB 6/12/81
-- BHS 7/19/84

WITH CA1102A0;
WITH REPORT; USE CA1102A0; USE REPORT;
PROCEDURE CA1102A2M IS


     INVOKED : BOOLEAN := FALSE;

BEGIN
     TEST ("CA1102A", "MORE THAN ONE WITH_CLAUSE; ALSO, A " &
           "USE_CLAUSE REFERING TO A PRECEDING WITH_CLAUSE " &
           "IN THE SAME CONTEXT_SPECIFICATION");

     P (INVOKED);
     IF NOT INVOKED THEN
          FAILED ("COMPILATION UNIT NOT MADE VISIBLE");
     END IF;

     RESULT;
END CA1102A2M;
