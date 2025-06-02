-- CA2003A0M.ADA

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
-- CHECK THAT A SUBUNIT HAS VISIBILITY OF IDENTIFIERS DECLARED
--   PRIOR TO ITS BODY_STUB.

-- SEPARATE FILES ARE:
--   CA2003A0M THE MAIN PROCEDURE.
--   CA2003A1  A SUBUNIT PROCEDURE BODY.

-- WKB 6/26/81
-- JRK 6/26/81

WITH REPORT;
USE REPORT;
PROCEDURE CA2003A0M IS

     I : INTEGER := 1;

     PROCEDURE CA2003A1 IS SEPARATE;

     PACKAGE P IS
          I : INTEGER := 2;
     END P;

BEGIN
     TEST ("CA2003A", "A SUBUNIT HAS VISIBILITY OF IDENTIFIERS " &
                      "DECLARED BEFORE ITS BODY_STUB");


     CA2003A1;

     RESULT;
END CA2003A0M;
