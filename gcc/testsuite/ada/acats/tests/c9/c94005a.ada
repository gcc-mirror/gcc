-- C94005A.ADA

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
-- CHECK THAT IF A TASK TYPE IS DECLARED IN A LIBRARY PACKAGE, A MAIN
--   PROGRAM THAT DECLARES OBJECTS OF THAT TYPE DOES WAIT FOR
--   TERMINATION OF SUCH OBJECTS.

-- THIS TEST CONTAINS RACE CONDITIONS.

-- JRK 10/8/81
-- SPS 11/21/82
-- JWC 11/15/85    MADE THE LIBRARY PACKAGE NAME UNIQUE, C94005A_PKG.
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.


WITH SYSTEM; USE SYSTEM;
PACKAGE C94005A_PKG IS

     TASK TYPE TT IS
          ENTRY E;
     END TT;

END C94005A_PKG;

with Impdef;
WITH REPORT; USE REPORT;
PACKAGE BODY C94005A_PKG IS

     TASK BODY TT IS
          I : INTEGER := IDENT_INT (0);
     BEGIN
          ACCEPT E;
          FOR J IN 1..60 LOOP
               I := IDENT_INT (I);
               DELAY 1.0 * Impdef.One_Second;
          END LOOP;
          RESULT;   -- FAILURE IF THIS MESSAGE IS NOT WRITTEN.
     END TT;

END C94005A_PKG;


WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH C94005A_PKG;
PROCEDURE C94005A IS

     T : C94005A_PKG.TT;


BEGIN
     TEST ("C94005A", "CHECK THAT IF A TASK TYPE IS DECLARED IN A " &
                      "LIBRARY PACKAGE, A MAIN PROGRAM THAT " &
                      "DECLARES OBJECTS OF THAT TYPE DOES WAIT FOR " &
                      "TERMINATION OF SUCH OBJECTS");

     COMMENT ("THE INVOKING SYSTEM'S JOB CONTROL LOG MUST BE " &
              "EXAMINED TO SEE IF THIS TEST REALLY TERMINATES");

     T.E;

     IF T'TERMINATED THEN
          COMMENT ("TEST INCONCLUSIVE BECAUSE TASK T PREMATURELY " &
                   "TERMINATED");
     END IF;

     -- TASK T SHOULD WRITE THE RESULT MESSAGE.

END C94005A;
