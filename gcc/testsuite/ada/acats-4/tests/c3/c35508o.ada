-- C35508O.ADA

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
--     CHECK THAT 'FIRST' AND 'LAST' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35508O IS

BEGIN
     TEST ("C35508O", "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "BOOLEAN TYPE" );

     DECLARE
          SUBTYPE TBOOL IS BOOLEAN RANGE IDENT_BOOL(TRUE) ..
                                                     IDENT_BOOL(TRUE);
          SUBTYPE FBOOL IS BOOLEAN
               RANGE IDENT_BOOL(FALSE) .. IDENT_BOOL(FALSE);
          SUBTYPE NOBOOL IS BOOLEAN
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(FALSE);
          TYPE NEWBOOL IS NEW BOOLEAN;
          TYPE NIL IS NEW BOOLEAN RANGE IDENT_BOOL(TRUE) ..
                                                    IDENT_BOOL(FALSE);

     BEGIN
          IF IDENT_BOOL(BOOLEAN'FIRST) /= FALSE THEN
               FAILED ( "WRONG VALUE FOR BOOLEAN'FIRST" );
          END IF;
          IF IDENT_BOOL(BOOLEAN'LAST) /= TRUE THEN
               FAILED ( "WRONG VALUE FOR BOOLEAN'LAST" );
          END IF;

          IF TBOOL'FIRST /= TRUE THEN
               FAILED ( "WRONG VALUE FOR TBOOL'FIRST" );
          END IF;
          IF TBOOL'LAST /= TRUE THEN
               FAILED ( "WRONG VALUE FOR TBOOL'LAST" );
          END IF;

          IF FBOOL'FIRST /= FALSE THEN
               FAILED ( "WRONG VALUE FOR FBOOL'FIRST" );
          END IF;
          IF FBOOL'LAST /= FALSE THEN
               FAILED ( "WRONG VALUE FOR FBOOL'LAST" );
          END IF;

          IF NOBOOL'FIRST /= TRUE THEN
               FAILED ( "WRONG VALUE FOR NOBOOL'FIRST" );
          END IF;
          IF NOBOOL'LAST /= FALSE THEN
               FAILED ( "WRONG VALUE FOR NOBOOL'LAST" );
          END IF;

          IF NEWBOOL'FIRST /= FALSE THEN
               FAILED ( "WRONG VALUE FOR NEWBOOL'FIRST" );
          END IF;
          IF NEWBOOL'LAST /= TRUE THEN
               FAILED ( "WRONG VALUE FOR NEWBOOL'LAST" );
          END IF;
          IF NIL'FIRST /= TRUE THEN
               FAILED ( "WRONG VALUE FOR NIL'FIRST" );
          END IF;
          IF NIL'LAST /= FALSE THEN
               FAILED ( "WRONG VALUE FOR NIL'LAST" );
          END IF;

     END;

     RESULT;
END C35508O;
