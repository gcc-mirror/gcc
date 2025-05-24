-- C64104O.ADA

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
-- OBJECTIVE
--    CHECK THAT CONSTRAINT_ERROR IS RAISED AT THE PLACE OF THE CALL
--    FOR THE CASE OF A PRIVATE TYPE IMPLEMENTED AS AN ACCESS TYPE WHERE
--    THE ACTUAL BOUNDS OR DISCRIMINANTS OF THE DESIGNATED OBJECT DIFFER
--    FROM THOSE OF THE FORMAL.

-- HISTORY
--    CPP 7/23/84 CREATED ORIGINAL TEST.
--    DHH 8/31/87 ADDED COMMENT IN PROCEDURE Q SO THAT CODE WILL NOT BE
--                OPTIMIZED OUT OF EXISTENCE.


WITH REPORT;  USE REPORT;
PROCEDURE C64104O IS

BEGIN

     TEST ("C64104O", "CHECK THAT PRIVATE TYPE (ACCESS) RAISES " &
           "CONSTRAINT_ERROR WHEN ACTUAL AND FORMAL PARAMETER BOUNDS " &
           "DIFFER");

     DECLARE


          CALLED : BOOLEAN := FALSE;

          PACKAGE P IS
               TYPE T IS PRIVATE;
               DC : CONSTANT T;
               GENERIC PACKAGE PP IS
               END PP;
          PRIVATE
               TYPE T IS ACCESS STRING;
               DC : CONSTANT T := NEW STRING'("AAA");
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
                    FAILED ("EXCEPTION RAISED INSIDE SUBPROGRAM");
          END Q;

          GENERIC
               Y : IN OUT P.T;
          PACKAGE CALL IS
          END CALL;

          PACKAGE BODY CALL IS
          BEGIN
               Q(Y);
          END CALL;

          PACKAGE BODY P IS
               Z : T(1..5) := NEW STRING'("CCCCC");
               PACKAGE BODY PP IS
                    PACKAGE CALL_Q IS NEW CALL(Z);
               END PP;
          END P;

     BEGIN
          BEGIN
               DECLARE
                    PACKAGE CALL_Q_NOW IS NEW P.PP;
               BEGIN
                    FAILED ("NO EXCEPTION RAISED");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("SUBPROGRAM Q WAS NOT CALLED");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END;

          RESULT;
     END;

END C64104O;
