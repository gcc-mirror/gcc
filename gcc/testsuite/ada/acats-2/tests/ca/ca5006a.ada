-- CA5006A.ADA

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
-- CHECK THAT A PROGRAM IS NOT REJECTED JUST BECAUSE THERE IS NO WAY TO
-- ELABORATE SECONDARY UNITS SO PROGRAM_ERROR WILL BE AVOIDED.

-- R.WILLIAMS 9/22/86

-----------------------------------------------------------------------

PACKAGE CA5006A0 IS
   FUNCTION P_E_RAISED RETURN BOOLEAN;
   PROCEDURE SHOW_PE_RAISED;
END CA5006A0;

-----------------------------------------------------------------------

WITH REPORT; USE REPORT;
PRAGMA ELABORATE (REPORT);
PACKAGE BODY CA5006A0 IS
   RAISED : BOOLEAN := FALSE;

   FUNCTION P_E_RAISED RETURN BOOLEAN IS
   BEGIN
      RETURN RAISED;
   END P_E_RAISED;

   PROCEDURE SHOW_PE_RAISED IS
   BEGIN
      RAISED := TRUE;
   END SHOW_PE_RAISED;

BEGIN
     TEST ( "CA5006A", "CHECK THAT A PROGRAM IS NOT REJECTED JUST " &
                       "BECAUSE THERE IS NO WAY TO ELABORATE " &
                       "SECONDARY UNITS SO PROGRAM_ERROR WILL BE " &
                       "AVOIDED" );

                         
END CA5006A0;

-----------------------------------------------------------------------

PACKAGE CA5006A1 IS
  FUNCTION F RETURN INTEGER;
END CA5006A1;

-----------------------------------------------------------------------

PACKAGE CA5006A2 IS
  FUNCTION G RETURN INTEGER;
END CA5006A2;

-----------------------------------------------------------------------

WITH REPORT; USE REPORT;
WITH CA5006A0; USE CA5006A0;
WITH CA5006A2; USE CA5006A2;
PRAGMA ELABORATE(CA5006A0);

PACKAGE BODY CA5006A1 IS
   X : INTEGER;

   FUNCTION F RETURN INTEGER IS
   BEGIN
      RETURN IDENT_INT(0);
   END F;

BEGIN
   X := G;
   IF NOT P_E_RAISED THEN
      FAILED ( "G CALLED" );
   END IF;
EXCEPTION
   WHEN PROGRAM_ERROR =>
      COMMENT ( "PROGRAM_ERROR RAISED IN CA5006A1" );
      SHOW_PE_RAISED;
   WHEN OTHERS =>
      FAILED ( "OTHER ERROR RAISED IN CA5006A1" );
END CA5006A1;

-----------------------------------------------------------------------

WITH REPORT; USE REPORT;
WITH CA5006A0; USE CA5006A0;
WITH CA5006A1; USE CA5006A1;
PRAGMA ELABORATE(CA5006A0);

PACKAGE BODY CA5006A2 IS
   X : INTEGER;

   FUNCTION G RETURN INTEGER IS
   BEGIN
      RETURN IDENT_INT(1);
   END G;

BEGIN
   X := F;
   IF NOT P_E_RAISED THEN
      FAILED ( "F CALLED" );
   END IF;
EXCEPTION
   WHEN PROGRAM_ERROR =>
      COMMENT ( "PROGRAM_ERROR RAISED IN CA5006A2" );
      SHOW_PE_RAISED;
   WHEN OTHERS =>
      FAILED ( "OTHER ERROR RAISED IN CA5006A2" );
END CA5006A2;

-----------------------------------------------------------------------

WITH REPORT; USE REPORT; 
WITH CA5006A0; USE CA5006A0;
WITH CA5006A1; 
WITH CA5006A2; 

PROCEDURE CA5006A IS
BEGIN
   IF NOT P_E_RAISED THEN
      FAILED ( "PROGRAM_ERROR NEVER RAISED" );
   END IF;

   RESULT;
END CA5006A;
