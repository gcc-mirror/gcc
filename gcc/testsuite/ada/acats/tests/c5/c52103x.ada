-- C52103X.ADA

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
-- CHECK THAT LENGTHS MUST MATCH IN ARRAY AND SLICE ASSIGNMENTS.
--    MORE SPECIFICALLY, TEST THAT ARRAY ASSIGNMENTS WITH MATCHING
--    LENGTHS DO NOT CAUSE  CONSTRAINT_ERROR  TO BE RAISED AND
--    ARE PERFORMED CORRECTLY.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)

-- THIS IS A SPECIAL CASE IN

--    DIVISION  C :  NON-NULL ARRAYS WHOSE LENGTHS ARE NOT DETERMINABLE
--                   STATICALLY

-- WHICH TREATS ARRAYS OF LENGTH GREATER THAN  INTEGER'LAST .
--    AN ADDITIONAL OBJECTIVE OF THIS TEST IS TO CHECK WHETHER LENGTH
--    COMPARISONS (AND LENGTH COMPUTATIONS) CAUSE
--    CONSTRAINT_ERROR TO BE RAISED.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- RM  07/31/81
-- SPS 10/26/82
-- JBG 06/15/83
-- EG  11/02/84
-- EG  10/28/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- JRK 06/24/86  FIXED COMMENTS ABOUT NUMERIC_ERROR/CONSTRAINT_ERROR.
-- MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT;
PROCEDURE  C52103X  IS

     USE  REPORT ;

BEGIN

     TEST( "C52103X" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE " &
                       "ASSIGNMENTS, THE LENGTHS MUST MATCH; ALSO " &
                       "CHECK WHETHER CONSTRAINT_ERROR " &
                       "OR STORAGE_ERROR ARE RAISED FOR LARGE ARRAYS" );

     -- IN THIS TEST WE CAN'T USE AGGREGATE ASSIGNMENT (EXCEPT WHEN
     --    THE AGGREGATES ARE STRING LITERALS); THEREFORE:
     --
     --    (1) ARRAYS WILL BE INITIALIZED BY INDIVIDUAL ASSIGNMENTS;
     --    (2) CAN'T USE NON-NULL CONSTANT ARRAYS.


     -- WE ASSUME THAT IN AN ARRAY_TYPE_DEFINITION THE INDEX PORTION
     --    AND THE COMPONENT_TYPE PORTION ARE FUNCTIONALLY ORTHOGONAL
     --    ALSO AT THE IMPLEMENTATION LEVEL, I.E. THAT THE CORRECTNESS
     --    OF THE ACCESSING MECHANISM FOR ARRAYS DOES NOT DEPEND ON
     --    COMPONENT_TYPE.  ACCORDINGLY WE ARE TESTING FOR SOME BUT
     --    NOT ALL KINDS OF COMPONENT_TYPE.  (COMPONENT_TYPES INCLUDED:
     --    INTEGER , CHARACTER , BOOLEAN .)


     -------------------------------------------------------------------

     --    (4) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

CONSTR_ERR:         -- THIS BLOCK CATCHES CONSTRAINT_ERROR
                    -- FOR THE TYPE DECLARATION.
     BEGIN

DCL_ARR:  DECLARE   -- THIS BLOCK DECLARES THE ARRAY TYPE

               TYPE  TA42  IS  ARRAY(
                    INTEGER RANGE IDENT_INT(-2)..IDENT_INT(INTEGER'LAST)
                                    )  OF BOOLEAN ;
               -- CONSTRAINT_ERROR MAY BE RAISED BY THE
               -- ARRAY TYPE DECLARATION.
               PRAGMA PACK (TA42);

               SUBTYPE  TA41  IS  TA42 ;

          BEGIN

               COMMENT ("NO CONSTRAINT_ERROR FOR TYPE " &
                        "WITH 'LENGTH = INTEGER'LAST + 3");

OBJ_DCL:       DECLARE   -- THIS BLOCK DECLARES TWO BOOLEAN ARRAYS THAT
                         -- HAVE INTEGER'LAST + 3 COMPONENTS;
                         -- STORAGE_ERROR MAY BE RAISED.
                    ARR41  :  TA41 ;
                    ARR42  :  TA42 ;

               BEGIN

               COMMENT ("NO STORAGE_ERROR OR CONSTRAINT_ERROR RAISED " &
                        "WHEN ALLOCATING TWO BIG BOOLEAN ARRAYS");
               -- INITIALIZATION OF RHS ARRAY:

               -- ONLY A SHORT INITIAL SEGMENT IS INITIALIZED,
               -- SINCE A COMPLETE INITIALIZATION MIGHT TAKE TOO LONG
               -- AND THE EXECUTION MIGHT BE ABORTED BEFORE THE LENGTH
               -- COMPARISON OF THE ARRAY ASSIGNMENT IS ATTEMPTED.

NO_EXCP:       BEGIN     -- NO EXCEPTION SHOULD OCCUR HERE.
                    FOR  I  IN  IDENT_INT(-2)..IDENT_INT(2)  LOOP
                         ARR41(I)  :=  FALSE  ;  -- VALUES ARE:: FTFFT
                    END LOOP;

                    ARR41(-1) := TRUE ;

                    ARR41( 2) := TRUE ;          -- RHS IS: F T F F T


                    -- INITIALIZATION OF UNUSED COMPONENT OF LHS ARRAY:

                    ARR42( -2 )  :=  TRUE ;

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>
                         FAILED ("CONSTRAINT_ERROR RAISED WHEN " &
                                 "ASSIGNING TO ARRAY COMPONENTS");
                    WHEN OTHERS =>
                         FAILED ("OTHER EXCEPTION RAISED - 1");

               END NO_EXCP;

DO_SLICE:      BEGIN
                    -- SLICE ASSIGNMENT:

                    ARR42(  IDENT_INT(-1)..IDENT_INT(INTEGER'LAST  )) :=
                         ARR41(
                            IDENT_INT(-2)..IDENT_INT(INTEGER'LAST-1)) ;

                    COMMENT ("NO EXCEPTION RAISED DURING SLICE " &
                             "ASSIGNMENT");

                    -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

     CHK_SLICE:     BEGIN
                         FOR  I  IN  IDENT_INT(-1)..IDENT_INT(2)  LOOP

                              IF  ARR42( I )  /=  FALSE  AND  I /= 0
                              THEN
                                   FAILED( "SLICE ASSIGNMENT NOT " &
                                           "CORRECT (VALUES)" );
                              ELSIF  ARR42( I ) /= TRUE  AND  I  = 0
                              THEN
                                   FAILED( "SLICE ASSIGNMENT NOT " &
                                           "CORRECT (VALUES)" );
                              END IF;

                         END LOOP;

                         IF  ARR42( -2 )  /=  TRUE
                         THEN
                              FAILED( "SLICE ASSIGNMENT NOT CORRECT " &
                                      "(SLIDING)" );
                         END IF;

                    EXCEPTION

                         WHEN OTHERS =>
                              FAILED ("SOME EXCEPTION RAISED - 2");

                    END CHK_SLICE;

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>
                         COMMENT ("CONSTRAINT_ERROR RAISED DURING " &
                                  "SLICE ASSIGNMENT");
                    WHEN STORAGE_ERROR =>
                         COMMENT ("STORAGE_ERROR RAISED DURING SLICE " &
                                  "ASSIGNMENT");
                    WHEN OTHERS =>
                         FAILED ("SOME EXCEPTION DURING SLICE " &
                                 "ASSIGNMENT");
               END DO_SLICE;

          END OBJ_DCL;

          EXCEPTION

               WHEN STORAGE_ERROR =>
                    COMMENT ("STORAGE_ERROR RAISED WHEN DECLARING " &
                             "TWO PACKED BOOLEAN ARRAYS WITH " &
                             "INTEGER'LAST + 3 COMPONENTS");
               WHEN CONSTRAINT_ERROR =>
                    COMMENT ("CONSTRAINT_ERROR RAISED WHEN DECLARING " &
                             "TWO PACKED BOOLEAN ARRAYS WITH " &
                             "INTEGER'LAST + 3 COMPONENTS");
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED - 3");

          END DCL_ARR;

     EXCEPTION


          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED WHEN DECLARING AN " &
                        "ARRAY TYPE WITH INTEGER'LAST + 3 COMPONENTS");

          WHEN STORAGE_ERROR =>
               FAILED ("STORAGE_ERROR RAISED FOR TYPE DECLARATION");

          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 4");

     END CONSTR_ERR;


     RESULT ;


END C52103X;
