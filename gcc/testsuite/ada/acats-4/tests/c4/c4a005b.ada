-- C4A005B.ADA

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
-- CHECK THAT A NONSTATIC UNIVERSAL INTEGER EXPRESSION RAISES
-- CONSTRAINT_ERROR IF DIVISION BY ZERO IS ATTEMPTED
-- OR IF THE SECOND OPERAND OF REM OR MOD IS ZERO.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- JBG 5/2/85
-- EG  10/24/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387; PREVENT DEAD VARIABLE OPTIMIZATION
-- MRM 03/30/93  REMOVE NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE C4A005B IS
BEGIN
     TEST("C4A005B", "CHECK CONSTRAINT_ERROR FOR " &
                     "NONSTATIC UNIVERSAL " &
                     "INTEGER EXPRESSIONS - DIVISION BY ZERO");
     BEGIN
          DECLARE
               X : BOOLEAN := 1 = 1/INTEGER'POS(IDENT_INT(0));
          BEGIN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - DIV");
               IF X /= IDENT_BOOL(X) THEN
                    FAILED ("WRONG RESULT - DIV");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION IN WRONG PLACE - DIV");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED FOR / BY 0");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - DIV");
     END;

     BEGIN
          DECLARE
               X : BOOLEAN := 1 = 1 REM INTEGER'POS(IDENT_INT(0));
          BEGIN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - REM");
               IF X /= IDENT_BOOL(X) THEN
                    FAILED ("WRONG RESULT - REM");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION IN WRONG PLACE - REM");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED FOR REM BY 0");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - REM");
     END;

     BEGIN
          DECLARE
               X : BOOLEAN := 1 = INTEGER'POS(IDENT_INT(1)) MOD 0;
          BEGIN
               FAILED ("CONSTRAINT_ERROR NOT RAISED - MOD");
               IF X /= IDENT_BOOL(X) THEN
                    FAILED ("WRONG RESULT - MOD");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION IN WRONG PLACE - MOD");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED FOR MOD BY 0");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - MOD");
     END;

     RESULT;

END C4A005B;
