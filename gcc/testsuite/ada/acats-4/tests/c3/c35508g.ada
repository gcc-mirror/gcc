-- C35508G.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35508G IS

BEGIN
     TEST ("C35508G", "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "BOOLEAN TYPE" );

     BEGIN
          IF BOOLEAN'PRED (IDENT_BOOL(TRUE)) /= FALSE THEN
               FAILED ( "INCORRECT VALUE FOR PRED OF TRUE" );
          END IF;
          IF BOOLEAN'SUCC (IDENT_BOOL(FALSE)) /= TRUE THEN
               FAILED ( "INCORRECT VALUE FOR SUCC OF FALSE" );
          END IF;
     END;

     DECLARE
          TYPE NEWBOOL IS NEW BOOLEAN;
     BEGIN
          IF NEWBOOL'PRED (TRUE) /= FALSE THEN
               FAILED ( "INCORRECT VALUE FOR NEWBOOL'PRED OF TRUE" );
          END IF;
          IF NEWBOOL'SUCC (FALSE) /= TRUE THEN
               FAILED ( "INCORRECT VALUE FOR NEWBOOL'SUCC OF FALSE" );
          END IF;
     END;

     DECLARE

          SUBTYPE SBOOL IS BOOLEAN RANGE IDENT_BOOL(TRUE) ..
                                                      IDENT_BOOL(TRUE);

     BEGIN
          BEGIN
               IF SBOOL'PRED (IDENT_BOOL(TRUE)) /= FALSE THEN
                    FAILED ( "INCORRECT VALUE FOR SBOOL'PRED " &
                             "OF TRUE" );
               END IF;
          END;

          BEGIN
               IF SBOOL'PRED (IDENT_BOOL(SBOOL'BASE'FIRST)) = TRUE THEN
                    FAILED("'PRED('FIRST) WRAPPED AROUNT TO TRUE");
               END IF;
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "'PRED (SBOOL'BASE'FIRST)" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR " &
                             "'PRED (SBOOL'BASE'FIRST)" );
          END;

          BEGIN
               IF SBOOL'SUCC (IDENT_BOOL(SBOOL'BASE'LAST)) = FALSE THEN
                    FAILED("'SUCC('LAST) WRAPPED AROUNT TO FALSE");
               END IF;
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "'SUCC (SBOOL'BASE'LAST)" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR " &
                             "'SUCC (SBOOL'BASE'LAST)" );
          END;
     END;

     RESULT;
END C35508G;
