-- C35503D.TST

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
--     CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULT FOR THE
--     LARGEST/SMALLEST INTEGER LITERAL FOR THE LONGEST INTEGER TYPE.

-- HISTORY:
--     RJW 02/26/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C35503D IS

     TYPE INT IS RANGE MIN_INT .. MAX_INT;

     FUNCTION IDENT (X:INT) RETURN INT IS
          BEGIN
               IF EQUAL (3,3) THEN
                    RETURN X;
               END IF;
               RETURN 0;
          END IDENT;

BEGIN
     TEST ("C35503D", "CHECK THAT 'IMAGE' AND 'VALUE' YIELD " &
                      "CORRECT RESULTS FOR THE LARGEST/SMALLEST "&
                      "INTEGER LITERAL FOR THE LARGEST INTEGER TYPE");

     -- MIN_INT IS THE DECIMAL LITERAL FOR SYSTEM.MIN_INT.
     -- MAX_INT IS THE DECIMAL LITERAL FOR SYSTEM.MAX_INT.

     BEGIN
          IF INT'VALUE (IDENT_STR("$MIN_INT")) /= MIN_INT THEN
               FAILED("INCORRECT RESULTS FOR 'VALUE' - MIN_INT");
          END IF;
     EXCEPTION
           WHEN CONSTRAINT_ERROR =>
               FAILED("CONSTRAINT_ERROR RAISED FOR 'VALUE' - MIN_INT");
           WHEN OTHERS =>
               FAILED("OTHER EXCEPTION RAISED FOR 'VALUE' - MIN_INT");
     END;

     BEGIN
           IF INT'IMAGE (IDENT(MIN_INT)) /= "$MIN_INT" THEN
                FAILED("INCORRECT RESULTS FOR 'IMAGE' - MIN_INT");
           END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED("EXCEPTION RAISED FOR 'IMAGE' - MIN_INT");
     END;

     BEGIN
           IF INT'VALUE (IDENT_STR("$MAX_INT")) /= MAX_INT THEN
               FAILED("INCORRECT RESULTS FOR 'VALUE' - MAX_INT");
           END IF;
     EXCEPTION
           WHEN CONSTRAINT_ERROR =>
               FAILED("CONSTRAINT_ERROR RAISED FOR 'VALUE' - MAX_INT");
           WHEN OTHERS =>
               FAILED("OTHER EXCEPTION RAISED FOR 'VALUE' - MAX_INT");
     END;

     BEGIN
           IF INT'IMAGE (IDENT(MAX_INT)) /= ' ' & "$MAX_INT" THEN
               FAILED("INCORRECT RESULTS FOR 'IMAGE' - MAXINT");
           END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED("EXCEPTION RAISED FOR 'IMAGE' - MAXINT");
     END;

     RESULT;
END C35503D;
