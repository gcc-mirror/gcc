-- C64104N.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED AT THE PLACE OF THE CALL
--     FOR THE CASE OF A PRIVATE TYPE IMPLEMENTED AS A SCALAR TYPE
--     WHERE THE VALUE OF THE FORMAL PARAMETER DOES NOT BELONG TO THE
--     SUBTYPE OF THE ACTUAL PARAMETER.

-- HISTORY:
--     DAVID A. TAFFS
--     CPP 07/23/84
--     RDH 04/18/90  REVISED TO CHECK THAT SUBPROGRAM IS ACTUALLY
--                   CALLED.
--     THS 09/21/90  REWORDED COMMENT STATING THAT THE TEST DOES NOT
--                   ACCEPT THE LITERAL INTERPRETATION OF 6.4.1(9).

WITH REPORT; USE REPORT;
PROCEDURE C64104N IS

BEGIN
     TEST ("C64104N", "CHECK THAT PRIVATE TYPE (SCALAR) RAISES " &
           "CONSTRAINT_ERROR WHEN ACTUAL AND FORMAL PARAMETER " &
           "BOUNDS DIFFER");

     DECLARE

          CALLED : BOOLEAN := FALSE;

          PACKAGE P IS
               TYPE T IS PRIVATE;
               DC : CONSTANT T;

               GENERIC PACKAGE PP IS
               END PP;
          PRIVATE
               TYPE T IS NEW INTEGER;
               DC : CONSTANT T := -1;
          END P;

          PROCEDURE Q (X : IN OUT P.T) IS
          BEGIN
               CALLED := TRUE;
               X := P.DC;
               IF P. "=" (X, P.DC) THEN
                    COMMENT("PROCEDURE Q WAS CALLED");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("EXCEPTION RAISED INSIDE SUBPROGRAM");
          END Q;

          GENERIC
               Y : IN OUT P.T;
          PACKAGE CALL IS
          END CALL;

          PACKAGE BODY CALL IS
          BEGIN
               Q (Y);
          END CALL;

-- NOTE CALL HAS VARIABLE OF A PRIVATE TYPE AS AN OUT PARAMETER.  
-- THIS TEST DOES NOT ACCEPT THE LITERAL INTERPRETATION OF 6.4.1(9).
-- REFER TO ADA IMPLEMENTOR'S GUIDE 6.4.1 SEMANTIC RAMIFICATION 19
-- AND AI-00025 FOR CLARIFICATION AS TO WHY THE LITERAL
-- INTERPRETATION IS REJECTED.

          PACKAGE BODY P IS
               Z : T RANGE 0..1 := 0;
               PACKAGE BODY PP IS
                    PACKAGE CALL_Q IS NEW CALL(Z);
               END PP;
          END P;

     BEGIN
          BEGIN
               DECLARE
                    PACKAGE CALL_Q_NOW IS NEW P.PP;    -- EXCEPTION
               BEGIN
                    FAILED ("NO EXCEPTION RAISED");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED("SUBPROGRAM Q WAS NOT CALLED");
                    END IF;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED");
          END;

          RESULT;

     END;
END C64104N;
