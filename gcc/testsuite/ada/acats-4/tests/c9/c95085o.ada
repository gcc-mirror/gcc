-- C95085O.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED AFTER AN ENTRY CALL FOR THE
-- CASE OF A PRIVATE TYPE IMPLEMENTED AS AN ACCESS TYPE WHERE THE VALUE
-- OF THE FORMAL PARAMETER DOES NOT BELONG TO THE SUBTYPE OF THE ACTUAL
-- PARAMETER.

-- JWC 10/30/85
-- JRK 1/15/86      ENSURE THAT EXCEPTION RAISED AFTER CALL, NOT BEFORE
--                  CALL.

WITH REPORT; USE REPORT;
PROCEDURE C95085O IS

BEGIN

     TEST ("C95085O", "CHECK THAT PRIVATE TYPE (ACCESS) RAISES " &
                      "CONSTRAINT_ERROR AFTER CALL WHEN FORMAL " &
                      "PARAMETER VALUE IS NOT IN ACTUAL'S SUBTYPE");

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

          TASK TSK IS
               ENTRY E (X : IN OUT P.T);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (X : IN OUT P.T) DO
                         CALLED := TRUE;
                         X := P.DC;
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK BODY");
          END TSK;

          GENERIC
               Y : IN OUT P.T;
          PACKAGE CALL IS
          END CALL;

          PACKAGE BODY CALL IS
          BEGIN
               TSK.E (Y);
               FAILED ("EXCEPTION NOT RAISED AFTER RETURN");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END CALL;

          PACKAGE BODY P IS
               Z : T (1..5) := NEW STRING'("CCCCC");
               PACKAGE BODY PP IS
                    PACKAGE CALL_Q IS NEW CALL (Z);
               END PP;
          END P;

     BEGIN

          BEGIN
               DECLARE
                    PACKAGE CALL_Q_NOW IS NEW P.PP;    -- START HERE.
               BEGIN
                    NULL;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER INVOKED");
          END;

     END;

     RESULT;
END C95085O;
