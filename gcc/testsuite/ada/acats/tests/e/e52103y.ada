-- E52103Y.ADA

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
-- CHECK WHETHER A NULL ARRAY WITH ONE DIMENSION OF LENGTH GREATER THAN
-- INTEGER'LAST RAISES CONSTRAINT_ERROR OR NO EXCEPTION,
-- EITHER WHEN DECLARED OR  ASSIGNED.

-- CHECK THAT LENGTHS MUST MATCH IN ARRAY AND SLICE ASSIGNMENTS.
--    MORE SPECIFICALLY, TEST THAT ARRAY ASSIGNMENTS WITH MATCHING
--    LENGTHS DO NOT CAUSE  CONSTRAINT_ERROR  TO BE RAISED AND
--    ARE PERFORMED CORRECTLY.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)


-- THIS IS A SPECIAL CASE IN

--    DIVISION  D :  NULL ARRAYS WHOSE LENGTHS ARE NOT DETERMINABLE
--                   STATICALLY

-- WHICH (THE SPECIAL CASE) TREATS TWO-DIMENSIONAL ARRAYS WHOSE LENGTH
--    ALONG ONE DIMENSION IS GREATER THAN  INTEGER'LAST  AND WHOSE
--    LENGTH ALONG THE OTHER DIMENSION IS  0 .


-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- RM  07/31/81
-- SPS 03/22/83
-- JBG 05/02/83
-- JBG 06/01/85
-- EG  10/28/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- LDC 06/01/88  CHANGED HEADER COMMENT TO INDICATE CONSTRAINT_ERROR
--               IS ALLOWED.  ADDED CODE TO PREVENT DEAD VARIABLE
--               OPTIMIZATION.
-- MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT;
PROCEDURE  E52103Y  IS

     USE  REPORT ;

BEGIN

     TEST( "E52103Y","CHECK WHETHER CONSTRAINT_ERROR " &
                     "OR NO EXCEPTION IS RAISED WHEN DIMENSION OF " &
                     "AN ARRAY HAS LENGTH > INTEGER'LAST");
     BEGIN

          DECLARE

               TYPE  TA42  IS  ARRAY(
                    INTEGER RANGE IDENT_INT( 13 )..IDENT_INT( 12 ),
                    INTEGER RANGE IDENT_INT(-2)..IDENT_INT(INTEGER'LAST)
                                    )  OF BOOLEAN ;

               SUBTYPE  TA41  IS  TA42 ;

               ARR41  :  TA41 ;
               ARR42  :  TA42 ;

          BEGIN

               COMMENT ("NO EXCEPTION FOR ARRAY DECLARATION");

               -- NULL ARRAY ASSIGNMENT:

               ARR42 := ARR41 ;
               IF ARR42'LENGTH(1) /= 0 THEN
                    FOR I IN TA42'RANGE(2) LOOP
                         ARR41(13,I) := IDENT_BOOL(ARR42(13,I));
                    END LOOP;
               END IF;

               COMMENT ("NO EXCEPTION RAISED FOR NULL ARRAY " &
                        "ASSIGNMENT");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    COMMENT ("CONSTRAINT_ERROR RAISED IN LENGTH " &
                             "COMPARISON");

               WHEN  OTHERS  =>
                    FAILED( "OTHER EXCEPTION RAISED  -  SUBTEST 2" );

          END ;

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED BY DECLARATION OF " &
                        "NULL ARRAY TYPE WITH ONE DIMENSION > " &
                        "INTEGER'LAST");

          WHEN OTHERS =>
               FAILED ("SOME OTHER EXCEPTION RAISED");

     END;

     -------------------------------------------------------------------


     RESULT ;


END E52103Y;
