-- C52104X.ADA

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
--    MORE SPECIFICALLY, TEST THAT ATTEMPTED ASSIGNMENTS BETWEEN
--    ARRAYS WITH NON-MATCHING LENGTHS LEAVE THE DESTINATION ARRAY
--    INTACT AND CAUSE  CONSTRAINT_ERROR  TO BE RAISED.
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
-- SPS 02/07/83
-- EG  10/28/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- JRK 06/24/86  FIXED COMMENTS ABOUT NUMERIC_ERROR/CONSTRAINT_ERROR.
-- MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X INCOMPATIBILITY

WITH REPORT;
PROCEDURE  C52104X  IS

     USE  REPORT ;

BEGIN

     TEST( "C52104X" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE " &
                       "ASSIGNMENTS, THE LENGTHS MUST MATCH; ALSO " &
                       "CHECK WHETHER CONSTRAINT_ERROR " &
                       "OR STORAGE_ERROR ARE RAISED FOR LARGE ARRAYS");

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

     --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

CONSTR_ERR:         -- THIS BLOCK CATCHES CONSTRAINT_ERROR
                    -- FOR THE SUBTYPE DECLARATION.
     BEGIN

DCL_ARR:  DECLARE        -- THIS BLOCK DECLARES THE ARRAY SUBTYPE.

               TYPE  TABOX5  IS  ARRAY( INTEGER RANGE <> )  OF BOOLEAN ;
               PRAGMA PACK (TABOX5);

               SUBTYPE  TABOX51  IS  TABOX5
                             (IDENT_INT(-6)..IDENT_INT(INTEGER'LAST-4));
               -- CONSTRAINT_ERROR MAY BE RAISED BY THIS
               -- SUBTYPE DECLARATION.

          BEGIN

               COMMENT ("NO CONSTRAINT_ERROR FOR TYPE " &
                        "WITH 'LENGTH = INTEGER'LAST + 3");

OBJ_DCL:       DECLARE   -- THIS BLOCK DECLARES TWO BOOLEAN ARRAYS THAT
                         -- HAVE INTEGER'LAST + 3 COMPONENTS;
                         -- STORAGE_ERROR MAY BE RAISED.
                    ARRX51  :  TABOX51 ;
                    ARRX52  :  TABOX5
                             (IDENT_INT(-2)..IDENT_INT( INTEGER'LAST));

               BEGIN

               COMMENT ("NO STORAGE_ERROR OR " &
                        "CONSTRAINT_ERROR RAISED WHEN ALLOCATING TWO " &
                        "BIG BOOLEAN ARRAYS");

               -- INITIALIZATION OF LHS ARRAY:

NO_EXCP:       BEGIN          -- NO EXCEPTION SHOULD OCCUR IN THIS BLOCK
                    FOR  I  IN  IDENT_INT(-2)..IDENT_INT(9)  LOOP
                         ARRX52( I )  :=  FALSE  ;
                    END LOOP;


               -- INITIALIZATION OF RHS ARRAY:

               -- ONLY A SHORT INITIAL SEGMENT IS INITIALIZED,
               -- SINCE A COMPLETE INITIALIZATION MIGHT TAKE TOO LONG
               -- AND THE EXECUTION MIGHT BE ABORTED BEFORE THE LENGTH
               -- COMPARISON OF THE ARRAY ASSIGNMENT IS ATTEMPTED.

                    FOR  I  IN  IDENT_INT(-6)..IDENT_INT(5)  LOOP
                         ARRX51( I )  :=  TRUE  ;
                    END LOOP;

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>
                         FAILED ("CONSTRAINT_ERROR RAISED WHEN " &
                                 "ASSIGNING TO ARRAY COMPONENTS");
                    WHEN OTHERS =>
                         FAILED ("OTHER EXCEPTION RAISED - 1");

               END NO_EXCP;

DO_SLICE:      BEGIN
               -- SLICE ASSIGNMENT:

                    ARRX52( IDENT_INT(-1)..IDENT_INT(INTEGER'LAST  )) :=
                         ARRX51(
                            IDENT_INT(-4)..IDENT_INT(INTEGER'LAST-4) ) ;
                    FAILED( "EXCEPTION NOT RAISED  (12)" );

               EXCEPTION

                    WHEN CONSTRAINT_ERROR =>

                         COMMENT ("CONSTRAINT_ERROR RAISED DURING " &
                                  "CHECK FOR SLICE ASSIGNMENT");

                         -- CHECKING THE VALUES AFTER THE SLICE
                         -- ASSIGNMENT:

                         FOR  I  IN  IDENT_INT(-2)..IDENT_INT(9)  LOOP

                              IF  ARRX52( I )  /=  FALSE
                              THEN
                                   FAILED( "LHS ARRAY ALTERED  (12A)");
                              END IF;

                         END LOOP;


                    WHEN STORAGE_ERROR =>
                         COMMENT ("STORAGE_ERROR RAISED DURING CHECK " &
                                  "FOR SLICE ASSIGNMENT");

                    WHEN OTHERS =>
                         FAILED ("SOME EXCEPTION RAISED DURING SLICE");

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
                        "ARRAY SUBTYPE WITH INTEGER'LAST + 3 " &
                        "COMPONENTS");

          WHEN STORAGE_ERROR =>
               FAILED ("STORAGE_ERROR RAISED FOR TYPE DECLARATION");

          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 4");

     END CONSTR_ERR;

     RESULT ;

END C52104X;
