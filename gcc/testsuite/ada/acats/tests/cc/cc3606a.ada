-- CC3606A.ADA

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
--     CHECK THAT THE DEFAULT EXPRESSIONS OF A FORMAL SUBPROGRAM'S
--     FORMAL PARAMETERS ARE USED WHEN THE FORMAL SUBPROGRAM IS
--     CALLED IN THE INSTANTIATED UNIT (RATHER THAN ANY DEFAULT
--     ASSOCIATED WITH ACTUAL SUBPROGRAM'S PARAMETERS).

-- HISTORY:
--     BCB 09/29/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CC3606A IS

     X : BOOLEAN;
     Y : BOOLEAN;

     FUNCTION FUNC (A : INTEGER := 35) RETURN BOOLEAN IS
     BEGIN
          RETURN (A = 7);
     END FUNC;

     PROCEDURE PROC (B : INTEGER := 35) IS
     BEGIN
          IF B /= 7 THEN
               FAILED ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
                       "PROCEDURE NOT USED - 1");
          END IF;
     END PROC;

     FUNCTION FUNC1 (C : INTEGER := 35) RETURN BOOLEAN IS
     BEGIN
          RETURN (C = 7);
     END FUNC1;

     PROCEDURE PROC3 (D : INTEGER := 35) IS
     BEGIN
          IF D /= 7 THEN
               FAILED ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
                       "PROCEDURE NOT USED - 2");
          END IF;
     END PROC3;

     GENERIC
          WITH FUNCTION FUNC (A : INTEGER := 7) RETURN BOOLEAN;
     FUNCTION GENFUNC RETURN BOOLEAN;

     FUNCTION GENFUNC RETURN BOOLEAN IS
     BEGIN
          IF NOT FUNC THEN
               FAILED ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
                       "FUNCTION NOT USED - 1");
          END IF;
          RETURN TRUE;
     END GENFUNC;

     GENERIC
          WITH PROCEDURE PROC (B : INTEGER := 7);
     PACKAGE PKG IS
     END PKG;

     PACKAGE BODY PKG IS
     BEGIN
          PROC;
     END PKG;

     GENERIC
          WITH FUNCTION FUNC1 (C : INTEGER := 7) RETURN BOOLEAN;
     PROCEDURE PROC2;

     PROCEDURE PROC2 IS
     BEGIN
          IF NOT FUNC1 THEN
               FAILED ("DEFAULT EXPRESSION OF FORMAL PARAMETER " &
                       "FUNCTION NOT USED - 2");
          END IF;
     END PROC2;

     GENERIC
          WITH PROCEDURE PROC3 (D : INTEGER := 7) IS <>;
     FUNCTION GENFUNC1 RETURN BOOLEAN;

     FUNCTION GENFUNC1 RETURN BOOLEAN IS
     BEGIN
          PROC3;
          RETURN TRUE;
     END GENFUNC1;

     FUNCTION NEWFUNC IS NEW GENFUNC(FUNC);

     PACKAGE PACK IS NEW PKG(PROC);

     PROCEDURE PROC4 IS NEW PROC2(FUNC1);

     FUNCTION NEWFUNC1 IS NEW GENFUNC1;

BEGIN

     TEST ("CC3606A", "CHECK THAT THE DEFAULT EXPRESSIONS OF A " &
                      "FORMAL SUBPROGRAM'S FORMAL PARAMETERS ARE " &
                      "USED WHEN THE FORMAL SUBPROGRAM IS CALLED IN " &
                      "THE INSTANTIATED UNIT (RATHER THAN ANY " &
                      "DEFAULT ASSOCIATED WITH ACTUAL SUBPROGRAM'S " &
                      "PARAMETERS)");

     X := NEWFUNC;
     Y := NEWFUNC1;
     PROC4;

     RESULT;
END CC3606A;
