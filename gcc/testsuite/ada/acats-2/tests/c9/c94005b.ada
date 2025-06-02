-- C94005B.ADA

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
-- CHECK THAT IF A TASK TYPE IS DECLARED IN A LIBRARY PACKAGE, ANY
--   BLOCKS, SUBPROGRAMS, OR TASKS THAT DECLARE OBJECTS OF THAT TYPE
--   DO WAIT FOR TERMINATION OF SUCH OBJECTS.
-- SUBTESTS ARE:
--   (A)  IN A MAIN PROGRAM BLOCK.
--   (B)  IN A LIBRARY FUNCTION.
--   (C)  IN A MAIN PROGRAM TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- JRK 10/8/81
-- SPS 11/2/82
-- SPS 11/21/82
-- JWC 11/15/85    MADE THE LIBRARY PACKAGE NAME UNIQUE, C94005B_PKG.
-- PWN 01/31/95    REMOVED PRAGMA PRIORITY FOR ADA 9X.


WITH SYSTEM; USE SYSTEM;
PACKAGE C94005B_PKG IS

     GLOBAL : INTEGER;

     TASK TYPE TT IS
          ENTRY E (I : INTEGER);
     END TT;

END C94005B_PKG;

with Impdef;
PACKAGE BODY C94005B_PKG IS

     TASK BODY TT IS
          LOCAL : INTEGER;
     BEGIN
          ACCEPT E (I : INTEGER) DO
               LOCAL := I;
          END E;
          DELAY 60.0 * Impdef.One_Second;    -- SINCE THE PARENT UNIT HAS HIGHER PRIORITY
                         -- AT THIS POINT, IT WILL RECEIVE CONTROL AND
                         -- TERMINATE IF THE ERROR IS PRESENT.
          GLOBAL := LOCAL;
     END TT;

END C94005B_PKG;


WITH REPORT; USE REPORT;
WITH C94005B_PKG; USE C94005B_PKG;
FUNCTION F RETURN INTEGER IS

     T : TT;

BEGIN

     T.E (IDENT_INT(2));
     RETURN 0;

END F;

with Impdef;
WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH C94005B_PKG; USE C94005B_PKG;
WITH F;
PROCEDURE C94005B IS


BEGIN
     TEST ("C94005B", "CHECK THAT IF A TASK TYPE IS DECLARED IN A " &
                      "LIBRARY PACKAGE, ANY BLOCKS, SUBPROGRAMS, OR " &
                      "TASKS THAT DECLARE OBJECTS OF THAT TYPE DO " &
                      "WAIT FOR TERMINATION OF SUCH OBJECTS");

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (A)

          T : TT;

     BEGIN -- (A)

          T.E (IDENT_INT(1));

     END; -- (A)

     IF GLOBAL /= 1 THEN
          FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                  "BLOCK EXIT - (A)");
     END IF;

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (B)

          I : INTEGER;

     BEGIN -- (B)

          I := F ;

          IF GLOBAL /= 2 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "FUNCTION EXIT - (B)");
          END IF;

     END; -- (B)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C)

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS
               T : TT;
          BEGIN
               T.E (IDENT_INT(3));
          END TSK;

     BEGIN -- (C)

          WHILE NOT TSK'TERMINATED LOOP
               DELAY 0.1 * Impdef.One_Second;
          END LOOP;

          IF GLOBAL /= 3 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "TASK EXIT - (C)");
          END IF;

     END; -- (C)

     --------------------------------------------------

     RESULT;
END C94005B;
