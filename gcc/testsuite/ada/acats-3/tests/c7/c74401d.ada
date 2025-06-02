-- C74401D.ADA

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
-- CHECK THAT AN OUT PARAMETER HAVING A LIMITED TYPE IS ALLOWED FOR
-- FORMAL SUBPROGRAM PARAMETERS.  (ONLY THE CASE OF PRACTICAL INTEREST,
-- NAMELY, LIMITED PRIVATE TYPES, IS CHECKED HERE.)

-- CHECK THAT AN OUT PARAMETER IN A RENAMING DECLARATION CAN HAVE A
-- LIMITED PRIVATE TYPE WHEN IT RENAMES A GENERIC FORMAL SUBPROGRAM.

-- JBG 5/1/85

WITH REPORT; USE REPORT;
PROCEDURE C74401D IS

     PACKAGE P IS
          TYPE LP IS LIMITED PRIVATE;
          PROCEDURE P1 (X : OUT LP);
          PROCEDURE P2 (X : OUT LP);
          FUNCTION EQ (L, R : LP) RETURN BOOLEAN;
          VAL1 : CONSTANT LP;
          VAL2 : CONSTANT LP;
     PRIVATE
          TYPE LP IS NEW INTEGER;
          VAL1 : CONSTANT LP := LP(IDENT_INT(3));
          VAL2 : CONSTANT LP := LP(IDENT_INT(-3));
     END P;

     PACKAGE BODY P IS
          PROCEDURE P1 (X : OUT LP) IS
          BEGIN
               X := 3;
          END P1;

          PROCEDURE P2 (X : OUT LP) IS
          BEGIN
               X := -3;
          END P2;

          FUNCTION EQ (L, R : LP) RETURN BOOLEAN IS
          BEGIN
               RETURN L = R;
          END EQ;
     END P;

     GENERIC
          WITH PROCEDURE P3 (Y : OUT P.LP);
          TYPE GLP IS LIMITED PRIVATE;
          WITH PROCEDURE P4 (Y : OUT GLP);
          VAL_P3 : IN OUT P.LP;
          VAL_P4 : IN OUT GLP;
     PACKAGE GPACK IS
          PROCEDURE RENAMED (X : OUT GLP) RENAMES P4;   -- OK. RENAMING.
     END GPACK;

     PACKAGE BODY GPACK IS
     BEGIN
          P3 (VAL_P3);
          P4 (VAL_P4);
     END GPACK;

BEGIN

     TEST ("C74401D", "CHECK THAT GENERIC FORMAL SUBPROGRAMS CAN HAVE "&
                      "LIMITED PRIVATE OUT PARAMETERS");

     DECLARE
          VAR1 : P.LP;
          VAR2 : P.LP;
          PACKAGE PACK IS NEW GPACK (P.P1, P.LP, P.P2, VAR1, VAR2);
     BEGIN
          IF NOT P.EQ (VAR1, P.VAL1) THEN
               FAILED ("P1 INVOCATION INCORRECT");
          END IF;

          IF NOT P.EQ (VAR2, P.VAL2) THEN
               FAILED ("P2 INVOCATION INCORRECT");
          END IF;

          P.P1 (VAR2);        -- RESET VALUE OF VAR2.
          PACK.RENAMED (VAR2);

          IF NOT P.EQ (VAR2, P.VAL2) THEN
               FAILED ("RENAMED INVOCATION INCORRECT");
          END IF;
     END;

     RESULT;

END C74401D;
