-- CA1011A6M.ADA

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
-- CHECK THAT IF A SUBPROGRAM BODY IS INITIALLY COMPILED, SUBSEQUENT
-- ATTEMPTS TO COMPILE A SUBPROGRAM BODY WITH A DIFFERENT PARAMETER AND
-- RESULT TYPE PROFILE ARE ACCEPTED (SEE AI-00199).

-- SEPARATE FILES ARE:
--     CA1011A0  A LIBRARY PROCEDURE (CA1011A0).
--     CA1011A1  A LIBRARY PROCEDURE (CA1011A0).
--     CA1011A2  A LIBRARY PROCEDURE (CA1011A2).
--     CA1011A3  A LIBRARY PROCEDURE (CA1011A2).
--     CA1011A4  A LIBRARY FUNCTION  (CA1011A4).
--     CA1011A5  A LIBRARY FUNCTION  (CA1011A4).
--     CA1011A6M THE MAIN PROCEDURE.

-- BHS 7/20/84
-- JBG 5/23/85

WITH CA1011A0, CA1011A2, CA1011A4;
WITH REPORT; USE REPORT;
PROCEDURE CA1011A6M IS

     I : INTEGER := 5;
     J : FLOAT := 4.0;

BEGIN

     TEST("CA1011A", "ATTEMPTS TO RECOMPILE A SUBPROGRAM WITH " &
                     "NONCONFORMING PARAMETER OR RESULT TYPE "  &
                     "PROFILES ARE ACCEPTED");

     CA1011A0(X => I);             -- EXPECT DEFAULT Y
     IF I = 3 THEN
          COMMENT ("SECOND DECLARATION OF CA1011A0 INVOKED CORRECTLY");
     END IF;

     CA1011A2(Y => J);             -- USE DEFAULT X.
     IF J = 3.0 THEN
          COMMENT ("SECOND DECLARATION OF CA1011A2 INVOKED CORRECTLY");
     END IF;

     I := INTEGER(CA1011A4);
     IF I = 3 THEN
          COMMENT ("SECOND DECLARATION OF CA1011A4 INVOKED CORRECTLY");
     END IF;

     RESULT;

END CA1011A6M;
