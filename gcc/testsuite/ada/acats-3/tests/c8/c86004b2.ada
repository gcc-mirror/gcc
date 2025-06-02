-- C86004B2M.ADA

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
--     CHECK THAT IF THE SPECIFICATION OF A LIBRARY SUBPROGRAM HAS A
--     "WITH" CLAUSE FOR A LIBRARY SUBPROGRAM M, THEN IN THE FORMAL PART
--     AND IN THE BODY (IN ANOTHER FILE), "STANDARD.M" IS A LEGAL NAME
--     FOR THE SUBPROGRAM M.

-- SEPARATE FILES ARE:
--     C86004B0  A LIBRARY FUNCTION AND A LIBRARY SUBPROGRAM
--               SPECIFICATION.
--     C86004B1  A LIBRARY SUBPROGRAM BODY FOR THE C86004B0
--               SPECIFICATION.
--     C86004B2M MAIN PROCEDURE USING THE SUBPROGRAM OF C86004B1.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

WITH C86004B1;
WITH REPORT; USE REPORT;
PROCEDURE C86004B2M IS
BEGIN
     C86004B1(IDENT_INT(0));
END C86004B2M;
