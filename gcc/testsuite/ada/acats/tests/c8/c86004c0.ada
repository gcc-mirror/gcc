-- C86004C0.ADA

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
--     INDEPENDENT GENERIC FUNCTION AND SUBPROGRAM FOR C86004C TEST.

-- HISTORY:
--     DHH 09/14/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
GENERIC
FUNCTION C86004C0_GEN(X : INTEGER) RETURN INTEGER;

FUNCTION C86004C0_GEN(X : INTEGER) RETURN INTEGER IS
BEGIN
     IF EQUAL(3,3) THEN
          RETURN X;
     ELSE
          RETURN 0;
     END IF;
END C86004C0_GEN;

WITH C86004C0_GEN;
PRAGMA ELABORATE(C86004C0_GEN);
FUNCTION C86004C0 IS NEW C86004C0_GEN;

WITH C86004C0;
WITH REPORT; USE REPORT;
PROCEDURE C86004C01(INTGR : INTEGER := STANDARD.C86004C0(4)) IS

     SUBTYPE INT IS INTEGER RANGE 0 .. 10;
     A : INT := STANDARD.C86004C0(10);
     B : INT := STANDARD.C86004C0(INTGR);

     PROCEDURE C86004C1 IS SEPARATE;

BEGIN
     C86004C1;
END;
