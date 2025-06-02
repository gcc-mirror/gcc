-- CC3203A.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL LIMITED/NON LIMITED PRIVATE TYPE HAS
-- DISCRIMINANTS, THE ACTUAL PARAMETER CAN HAVE DEFAULT DISCRIMINANT
-- VALUES.

-- SPS 7/9/82

WITH REPORT;
USE REPORT;

PROCEDURE CC3203A IS
BEGIN
     TEST ("CC3203A", "CHECK DEFAULT VALUES FOR LIMITED/" &
           "NON LIMITED GENERIC FORMAL PRIVATE TYPES");
     DECLARE
          SD : INTEGER := IDENT_INT(0);

          FUNCTION INIT_RC (X: INTEGER) RETURN INTEGER;

          TYPE REC (D : INTEGER := 3) IS
               RECORD NULL; END RECORD;

          TYPE RC(C : INTEGER := INIT_RC (1)) IS
               RECORD NULL; END RECORD;

          GENERIC
               TYPE PV(X : INTEGER) IS PRIVATE;
               TYPE LP(X : INTEGER) IS LIMITED PRIVATE;
          PACKAGE PACK IS
               SUBTYPE NPV IS PV;
               SUBTYPE NLP IS LP;
          END PACK;

          FUNCTION INIT_RC (X: INTEGER) RETURN INTEGER IS
               BEGIN
                    SD := SD + X;
                    RETURN SD;
          END INIT_RC;

          PACKAGE P1 IS NEW PACK (REC, RC);

          PACKAGE P2 IS
               P1VP : P1.NPV;
               P1VL : P1.NLP;
               P1VL2 : P1.NLP;
          END P2;
          USE P2;
     BEGIN

          IF P1VP.D /= IDENT_INT(3) THEN
               FAILED ("DEFAULT DISCRIMINANT VALUE WRONG");
          END IF;

          IF P1VL.C /= 1 THEN
               FAILED ("DID NOT EVALUATE DEFAULT DISCRIMINANT");
          END IF;

          IF P1VL2.C /= IDENT_INT(2) THEN
               FAILED ("DID NOT EVALUATE DEFAULT DISCRIMINANT " &
                       "WHEN NEEDED");
          END IF;
     END;

     RESULT;

END CC3203A;
