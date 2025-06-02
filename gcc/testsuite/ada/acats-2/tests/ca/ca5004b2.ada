-- CA5004B2M.ADA

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
-- CHECK THAT PRAGMA ELABORATE IS ACCEPTED AND OBEYED EVEN IF THE UNIT
--    NAMED IN THE PRAGMA DOES NOT YET HAVE A BODY IN THE LIBRARY OR IF 
--    ITS BODY IS OBSOLETE.
-- CHECK THAT MORE THAN ONE NAME IS ALLOWED IN A PRAGMA ELABORATE.
--
-- SPECIAL INSTRUCTIONS:  
--     1. Compile CA5004B0.ADA
--     2. Compile CA5004B1.ADA
--     3. Compile CA5004B2M.ADA
--     4. Bind/Link main unit CA5004B2M
--     5. Execute the resulting file
--
-- TEST FILES:
--        CA5004B0.ADA
--        CA5004B1.ADA
--     => CA5004B2M.ADA

-- BHS 8/03/84
-- JRK 9/20/84
-- PWN 11/30/94 ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.
-- PWN 05/31/96 Split test into files without duplicate unit names.
-- TMB 11/20/96 ADDED PROCEDURE DECL TO CA5004B0 TO INSURE IT MAKES
--               THE OLD BODY OBSOLETE
-- TMB 12/2/96  MADE NAME OF MAIN PROCEDURE SAME AS FILE NAME
-- RLB 03/11/99 Split first test file in order to prevent good units
--              from being made obsolete.

-------------------------------------------------------------

PACKAGE CA5004B0 IS          -- OLD BODY NOW OBSOLETE.

     I : INTEGER := 2;
     B : BOOLEAN := TRUE;

     FUNCTION F RETURN BOOLEAN;
     PROCEDURE P;

END CA5004B0;

---------------------------------------------------------

PACKAGE CA5004B1 IS

     J : INTEGER := 3;

     PROCEDURE P (X : INTEGER);

END CA5004B1;                -- NO BODY GIVEN YET.

----------------------------------------------------------

WITH HEADER; USE HEADER;
WITH CA5004B0, CA5004B1;
USE CA5004B0, CA5004B1;
PRAGMA ELABORATE (HEADER, CA5004B0, CA5004B1);
PACKAGE CA5004B2 IS

     K1 : INTEGER := CA5004B0.I;
     K2 : INTEGER := CA5004B1.J;

     PROCEDURE REQUIRE_BODY;

END CA5004B2;


PACKAGE BODY CA5004B2 IS

     PROCEDURE REQUIRE_BODY IS
     BEGIN
       NULL;
     END;

BEGIN

     IF K1 /= 4 THEN
          WRONG ("OBSOLETE BODY");
     END IF;

     IF K2 /= 5 THEN
          WRONG ("NO BODY");
     END IF;

END CA5004B2;

--------------------------------------------------

WITH REPORT, CA5004B2;
USE REPORT, CA5004B2;
PROCEDURE CA5004B2M IS
BEGIN

     RESULT;

END CA5004B2M;

----------------------------------------------------

PACKAGE BODY CA5004B0 IS

     FUNCTION F RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END F;

     PROCEDURE P IS
     BEGIN
        RETURN;
     END P;

BEGIN

     I := 4;

END CA5004B0;

---------------------------------------------------

PACKAGE BODY CA5004B1 IS

     PROCEDURE P (X : INTEGER) IS
     BEGIN
          NULL;
     END P;

BEGIN

     J := 5;

END CA5004B1;
