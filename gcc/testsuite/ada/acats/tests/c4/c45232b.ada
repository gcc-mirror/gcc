-- C45232B.ADA

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
-- CHECK THAT NO EXCEPTION IS RAISED WHEN AN INTEGER LITERAL IN
-- A COMPARISON  BELONGS TO THE BASE TYPE BUT IS OUTSIDE THE 
-- SUBTYPE OF THE OTHER OPERAND.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- P. BRASHEAR  08/21/86
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT, SYSTEM; USE REPORT;
PROCEDURE C45232B IS

BEGIN

     TEST ("C45232B", "NO EXCEPTION IS RAISED WHEN AN INTEGER " & 
                      "LITERAL IN A COMPARISON BELONGS TO THE BASE " &
                      "TYPE BUT IS OUTSIDE THE SUBTYPE OF THE " &
                      "OTHER OPERAND");

     DECLARE

           TYPE INT10 IS RANGE -10 .. 5;

     BEGIN    

          IF 7 > INT10'(-10) THEN
               COMMENT ("NO EXCEPTION RAISED FOR '7 > " &
                        "INT10'(-10)'");
          ELSE
               FAILED ("WRONG RESULT FOR '7 > INT10'(-10)'");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR '7 " &
                       "> INT10'(-10)'");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR '7 > " &
                       "INT10'(-10)'");
     END;  

     DECLARE

           TYPE INT10 IS RANGE -10 .. 5;

     BEGIN    

          IF 7 NOT IN INT10 THEN
               COMMENT ("NO EXCEPTION RAISED FOR '7 NOT IN " &
                        "INT'");
          ELSE
               FAILED ("WRONG RESULT FOR '7 NOT IN INT'");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR '7 " &
                       "NOT IN INT'");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR '7 NOT IN " &
                       "INT'");
     END;  

     DECLARE

           TYPE INT700 IS RANGE -700 .. 500;

     BEGIN    
          IF 600 > INT700'(5) THEN
               COMMENT ("NO EXCEPTION RAISED FOR '600 > " &
                        "INT700'(5)'");
          ELSE
               FAILED ("WRONG RESULT FOR '600 > INT700'(5)'");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR '600 " &
                       "> INT700'(5)'");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR '600 > " &
                       "INT700'(5)'");
     END;  

     DECLARE

           TYPE INT700 IS RANGE -700 .. 500;

     BEGIN    

          IF 600 NOT IN INT700 THEN
               COMMENT ("NO EXCEPTION RAISED FOR '600 NOT IN " &
                        "INT700'");
          ELSE
               FAILED ("WRONG RESULT FOR '600 NOT IN INT700'");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED FOR '600 " &
                       "NOT IN INT700'");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED FOR '600 NOT IN " &
                       "INT700'");
     END;  

     RESULT;

END C45232B;
