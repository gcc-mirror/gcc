-- C35503O.ADA

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
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35503O IS

BEGIN
     TEST ("C35503O", "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
                      "INTEGER TYPE" );

     DECLARE
          SUBTYPE SINTEGER IS INTEGER;
          SUBTYPE SMALL IS INTEGER RANGE IDENT_INT(-10) ..
                                                         IDENT_INT(10);
          SUBTYPE NOINTEGER IS INTEGER
            RANGE IDENT_INT(5) .. IDENT_INT(-7);

          TYPE INT IS RANGE -6 .. 6;
          SUBTYPE SINT IS INT
            RANGE INT(IDENT_INT(-4)) .. INT(IDENT_INT(4));
          SUBTYPE NOINT IS INT
            RANGE INT(IDENT_INT(1)) .. INT(IDENT_INT(-1));
          TYPE NEWINT IS NEW INTEGER RANGE IDENT_INT(-9) ..
                                                          IDENT_INT(-2);
          SUBTYPE SNEWINT IS NEWINT RANGE -7 .. -5;
          SUBTYPE NONEWINT IS NEWINT RANGE 3 .. -15;

     BEGIN
          IF SINTEGER'FIRST /= INTEGER'FIRST THEN
               FAILED ( "WRONG VALUE FOR SINTEGER'FIRST" );
          END IF;
          IF SINTEGER'LAST /= INTEGER'LAST THEN
               FAILED ( "WRONG VALUE FOR SINTEGER'LAST" );
          END IF;

          IF SMALL'FIRST /= -10 THEN
               FAILED ( "WRONG VALUE FOR SMALL'FIRST" );
          END IF;
          IF SMALL'LAST /= 10 THEN
               FAILED ( "WRONG VALUE FOR SMALL'LAST" );
          END IF;

          IF NOINTEGER'FIRST /= 5 THEN
               FAILED ( "WRONG VALUE FOR NOINTEGER'FIRST" );
          END IF;
          IF NOINTEGER'LAST /= -7 THEN
               FAILED ( "WRONG VALUE FOR NOINTEGER'LAST" );
          END IF;

          IF INT'FIRST /= -6 THEN
               FAILED ( "WRONG VALUE FOR INT'FIRST" );
          END IF;
          IF INT'LAST /= 6 THEN
               FAILED ( "WRONG VALUE FOR INT'LAST" );
          END IF;

          IF SINT'FIRST /= -4 THEN
               FAILED ( "WRONG VALUE FOR SINT'FIRST" );
          END IF;
          IF SINT'LAST /= 4 THEN
               FAILED ( "WRONG VALUE FOR SINT'LAST" );
          END IF;

          IF NOINT'FIRST /= 1 THEN
               FAILED ( "WRONG VALUE FOR NOINT'FIRST" );
          END IF;
          IF NOINT'LAST /= -1 THEN
               FAILED ( "WRONG VALUE FOR NOINT'LAST" );
          END IF;

          IF NEWINT'FIRST /= -9 THEN
               FAILED ( "WRONG VALUE FOR NEWINT'FIRST" );
          END IF;
          IF NEWINT'LAST /= -2 THEN
               FAILED ( "WRONG VALUE FOR NEWINT'LAST" );
          END IF;

          IF SNEWINT'FIRST /= -7 THEN
               FAILED ( "WRONG VALUE FOR SNEWINT'FIRST" );
          END IF;
          IF SNEWINT'LAST /= -5 THEN
               FAILED ( "WRONG VALUE FOR SNEWINT'LAST" );
          END IF;

          IF NONEWINT'FIRST /= 3 THEN
               FAILED ( "WRONG VALUE FOR NONEWINT'FIRST" );
          END IF;
          IF NONEWINT'LAST /= -15 THEN
               FAILED ( "WRONG VALUE FOR NONEWINT'LAST" );
          END IF;
     END;

     RESULT;
END C35503O;
