-- C94002D.ADA

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
-- CHECK THAT A TASK DOES  N O T  DEPEND ON A UNIT IF IT IS DESIGNATED
-- BY A LOCAL ACCESS VARIABLE (OF THIS UNIT) WHOSE TYPE IS DECLARED
-- OUTSIDE THIS UNIT.

-- WEI  3/ 4/82
-- JBG 2/20/84
-- TBN 11/25/85     RENAMED FROM C940ACB-B.ADA.

WITH REPORT;
 USE REPORT;
PROCEDURE C94002D IS

     TASK TYPE TT1 IS
          ENTRY E1;
          ENTRY E2;
     END TT1;

     TYPE ATT1 IS ACCESS TT1;
     OUTER_TT1 : ATT1;

     TASK BODY TT1 IS
     BEGIN
          ACCEPT E1;
          ACCEPT E2;
     END TT1;

BEGIN
     TEST ("C94002D", "DEPENDENCY IS INDEPENDENT OF WHERE ACCESS " &
                      "VARIABLE IS DECLARED");

BLOCK1 :
     DECLARE
          POINTER_TT1 : ATT1 := NEW TT1;
     BEGIN
          OUTER_TT1 := POINTER_TT1;
          POINTER_TT1.ALL.E1;
     END BLOCK1;         -- MAY DEADLOCK HERE IF INCORRECT DEPENDENCY
                         -- RULE IS IMPLEMENTED.

     IF OUTER_TT1.ALL'TERMINATED THEN
          FAILED ("NON-DEPENDENT TASK IS TERMINATED " &
                  "IMMEDIATELY AFTER ENCLOSING UNIT HAS " &
                  "BEEN COMPLETED");
     END IF;

     OUTER_TT1.E2;       -- RELEASE TASK

     RESULT;

END C94002D;
