-- C95095D.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM AND ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (D) A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE
--         PART, AN ENTRY IS DECLARED IN A TASK, AND THE
--         PARAMETERS ARE ORDERED DIFFERENTLY.

-- JWC 7/24/85
-- JRK 10/2/85
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C95095D IS


BEGIN
     TEST ("C95095D", "SUBPROGRAM/ENTRY OVERLOADING WITH " &
                      "MINIMAL DIFFERENCES");

     --------------------------------------------------

     -- A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE
     -- PART, AN ENTRY IS DECLARED IN A TASK, AND THE
     -- PARAMETERS ARE ORDERED DIFFERENTLY.

     DECLARE
          S : STRING (1..2) := "12";

          I : INTEGER := 0;

          PROCEDURE E (I1 : INTEGER; I2 : IN OUT INTEGER;
                       B1 : BOOLEAN) IS
          BEGIN
               S (1) := 'A';
          END E;

          TASK T IS
               ENTRY E (B1 : BOOLEAN; I1 : INTEGER;
                        I2 : IN OUT INTEGER);
          END T;

          TASK BODY T IS
          BEGIN
               E (5, I, TRUE);          -- PROCEDURE CALL.
               ACCEPT E (B1 : BOOLEAN; I1 : INTEGER;
                         I2 : IN OUT INTEGER) DO
                    S (2) := 'B';
               END E;
               E (TRUE, 5, I);          -- ENTRY CALL; SELF-BLOCKING.
               -- NOTE THAT A CALL IN WHICH ALL ACTUAL PARAMETERS
               -- ARE NAMED_ASSOCIATIONS IS AMBIGUOUS.
               FAILED ("TASK DID NOT BLOCK ITSELF");
          END T;

     BEGIN

          T.E (TRUE, 5, I);

          DELAY 10.0 * Impdef.One_Second;
          ABORT T;

          IF S /= "AB" THEN
               FAILED ("PROCEDURES/ENTRIES " &
                       "DIFFERING ONLY IN PARAMETER " &
                       "TYPE ORDER CAUSED CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;
END C95095D;
