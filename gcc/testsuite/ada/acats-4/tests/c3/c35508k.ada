-- C35508K.ADA

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
-- CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE 
-- PREFIX IS A BOOLEAN TYPE.

-- RJW 3/19/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE C35508K IS

     TYPE NEWBOOL IS NEW BOOLEAN;

BEGIN
     TEST ("C35508K", "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
                      "CORRECT RESULTS  WHEN THE PREFIX IS A " &
                      "BOOLEAN TYPE" );

     BEGIN
          IF BOOLEAN'POS (IDENT_BOOL(FALSE)) /= 0 THEN
               FAILED ( "WRONG POS FOR 'FALSE'" );
          END IF;
          IF BOOLEAN'POS (IDENT_BOOL(TRUE)) /= 1 THEN
               FAILED ( "WRONG POS FOR 'TRUE'" );
          END IF;

          IF BOOLEAN'VAL (IDENT_INT(0)) /= FALSE THEN
               FAILED ( "WRONG VAL FOR '0'" );
          END IF;
          IF BOOLEAN'VAL (IDENT_INT(1)) /= TRUE THEN
               FAILED ( "WRONG VAL FOR '1'" );
          END IF;
     END;

     BEGIN
          IF BOOLEAN'VAL (IDENT_INT(-1)) = TRUE THEN
               FAILED("'VAL(-1) WRAPPED AROUND TO TRUE");
          END IF;
          FAILED ( "NO EXCEPTION RAISED FOR VAL OF '-1'" );
     EXCEPTION     
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VAL OF '-1'" );
     END;

     BEGIN
          IF BOOLEAN'VAL (IDENT_INT(2)) = FALSE THEN
               FAILED("BOOLEAN'VAL(2) WRAPPED AROUND TO FALSE");
          END IF;
          FAILED ( "NO EXCEPTION RAISED FOR VAL OF '2'" );
     EXCEPTION     
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VAL OF '2'" );
     END;

     BEGIN
          IF NEWBOOL'POS (FALSE) /= 0 THEN
               FAILED ( "WRONG POS FOR NEWBOOL'(FALSE)" );
          END IF;
          IF NEWBOOL'POS (TRUE) /= 1 THEN
               FAILED ( "WRONG POS FOR NEWBOOL'(TRUE)" );
          END IF;

          IF NEWBOOL'VAL (0) /= FALSE THEN
               FAILED ( "WRONG NEWBOOL'VAL FOR '0'" );
          END IF;
          IF NEWBOOL'VAL (1) /= TRUE THEN
               FAILED ( "WRONG NEWBOOL'VAL FOR '1'" );
          END IF;
     END;

     BEGIN
          IF NEWBOOL'VAL (IDENT_INT(-1)) = TRUE THEN
               FAILED("NEWBOOL'VAL(-1) WRAPPED AROUND TO TRUE"); 
          END IF;
          FAILED ( "NO EXCEPTION RAISED FOR NEWBOOL'VAL OF '-1'" );
     EXCEPTION     
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NEWBOOL'VAL OF '-1'" );
     END;

     BEGIN
          IF NEWBOOL'VAL (IDENT_INT(2)) = FALSE THEN
               FAILED("NEWBOOL'VAL(2) WRAPPED AROUND TO FALSE");
          END IF;
          FAILED ( "NO EXCEPTION RAISED FOR NEWBOOL'VAL OF '2'" );
     EXCEPTION     
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NEWBOOL'VAL OF '2'" );
     END;

     RESULT;
END C35508K;
