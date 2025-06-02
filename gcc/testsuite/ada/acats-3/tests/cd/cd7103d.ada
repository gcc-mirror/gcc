-- CD7103D.ADA

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
--     CHECK THAT THE CONSTANT FINE_DELTA = 2.0 ** (- MAX_MANTISSA).

-- HISTORY:
--     BCB 09/10/87  CREATED ORIGINAL TEST.

--     DTN 11/21/91  DELETED SUBPART (A).  CHANGED EXTENSION FROM '.TST' TO 
--                   '.ADA'.

WITH SYSTEM;
WITH REPORT;  USE REPORT;

PROCEDURE CD7103D IS

     MANTISSA_VAL : CONSTANT := 2.0 ** (-SYSTEM.MAX_MANTISSA);

BEGIN

     TEST ("CD7103D", "CHECK THAT THE CONSTANT FINE_DELTA " &
                      "= 2.0 ** (- MAX_MANTISSA)");

     IF SYSTEM.FINE_DELTA /= MANTISSA_VAL THEN
          FAILED ("INCORRECT VALUE FOR SYSTEM.FINE_DELTA");
     END IF;

     RESULT;

END CD7103D;
