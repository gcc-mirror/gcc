-- C45242B.ADA

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
--     CHECK THAT NO EXCEPTION IS RAISED WHEN A FLOATING POINT LITERAL
--     OPERAND IN A COMPARISON OR A FLOATING POINT LITERAL LEFT OPERAND
--     IN A MEMBERSHIP TEST BELONGS TO THE BASE TYPE BUT IS OUTSIDE
--     THE RANGE OF THE SUBTYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- HISTORY:
--     PWB 09/04/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT, SYSTEM; USE REPORT;
PROCEDURE C45242B IS

BEGIN

     TEST ("C45242B", "NO EXCEPTION IS RAISED WHEN A FLOATING " &
                      "LITERAL USED IN A COMPARISON OR AS THE " &
                      "LEFT OPERAND IN A MEMBERSHIP TEST " &
                      "BELONGS TO THE BASE TYPE BUT IS OUTSIDE " &
                      "THE RANGE OF THE SUBTYPE");

     DECLARE
          N : FLOAT := FLOAT (IDENT_INT (1));
          SUBTYPE FLOAT_1 IS FLOAT RANGE -1.0 .. N;
          NUM : FLOAT_1 := N;
     BEGIN    -- PRE-DEFINED FLOAT COMPARISON

          IF EQUAL(3,3) THEN
               NUM := FLOAT_1'(0.5);
          END IF;

          IF 2.0 > NUM THEN
               COMMENT ("NO EXCEPTION RAISED FOR PRE-DEFINED FLOAT " &
                        "COMPARISON");
          ELSE
               FAILED ("WRONG RESULT FROM PRE-DEFINED FLOAT " &
                       "COMPARISON");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR PRE-DEFINED " &
                       "FLOAT COMPARISON");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR PRE-DEFINED " &
                       "FLOAT COMPARISON");
     END;  -- PRE-DEFINED FLOAT COMPARISON

     DECLARE
          N : FLOAT := FLOAT (IDENT_INT (1));
          SUBTYPE FLOAT_1 IS FLOAT RANGE -1.0 .. N;
     BEGIN    -- PRE-DEFINED FLOAT MEMBERSHIP

          IF 2.0 IN FLOAT_1 THEN
               FAILED ("WRONG RESULT FROM PRE-DEFINED FLOAT " &
                       "MEMBERSHIP");
          ELSE
               COMMENT ("NO EXCEPTION RAISED FOR PRE-DEFINED FLOAT " &
                        "MEMBERSHIP");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR PRE-DEFINED " &
                       "FLOAT MEMBERSHIP");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR PRE-DEFINED " &
                       "FLOAT MEMBERSHIP");
     END;  -- PRE-DEFINED FLOAT MEMBERSHIP

     DECLARE -- PRECISE FLOAT COMPARISON
          TYPE FINE_FLOAT IS DIGITS SYSTEM.MAX_DIGITS;
          N : FINE_FLOAT := 0.5 * FINE_FLOAT (IDENT_INT (1));
          SUBTYPE SUB_FINE IS FINE_FLOAT RANGE -0.5 .. N;
          NUM : SUB_FINE := N;
     BEGIN
          IF EQUAL(3,3) THEN
               NUM := 0.25;
          END IF;

          IF 0.75 > NUM THEN
               COMMENT ("NO EXCEPTION RAISED FOR FINE_FLOAT " &
                        "COMPARISON");
          ELSE
               FAILED ("WRONG RESULT FROM FINE_FLOAT COMPARISON");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR " &
                       "FINE_FLOAT COMPARISON");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR  " &
                       "FINE_FLOAT COMPARISON");
     END;  --  FINE_FLOAT COMPARISON

     DECLARE -- PRECISE FLOAT MEMBERSHIP
          TYPE FINE_FLOAT IS DIGITS SYSTEM.MAX_DIGITS;
          N : FINE_FLOAT := 0.5 * FINE_FLOAT (IDENT_INT (1));
          SUBTYPE SUB_FINE IS FINE_FLOAT RANGE -0.5 .. N;
     BEGIN

          IF 0.75 IN SUB_FINE THEN
               FAILED ("WRONG RESULT FROM FINE_FLOAT MEMBERSHIP");
          ELSE
               COMMENT ("NO EXCEPTION RAISED FOR FINE_FLOAT " &
                        "MEMBERSHIP");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR " &
                       "FINE_FLOAT MEMBERSHIP");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR  " &
                       "FINE_FLOAT MEMBERSHIP");
     END;  --  FINE_FLOAT MEMBERSHIP

     RESULT;

END C45242B;
