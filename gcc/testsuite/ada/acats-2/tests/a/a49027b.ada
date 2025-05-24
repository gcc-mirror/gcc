-- A49027B.ADA

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
--   CHECK THAT A SUBTYPE CAN BE NONSTATIC IN A GENERIC TEMPLATE
--   AND STATIC IN THE CORRESPONDING INSTANCE.

--   CHECK THAT IF A GENERIC PARAMETER IS A STATIC EXPRESSION AND THE
--   CORRESPONDING (IN) PARAMETER HAS A STATIC SUBTYPE IN THE INSTANCE,
--   THEN EACH USE OF THE FORMAL PARAMETERS IN THE INSTANCE IS SAID TO
--   BE STATIC.
--
--   A NAME DENOTING A CONSTANT DECLARED IN A GENERIC INSTANCE IS
--   ALLOWED AS A PRIMARY IN A STATIC EXPRESSION IF THE CONSTANT
--   IS DECLARED BY A CONSTANT DECLARATION WITH A STATIC SUBTYPE
--   AND INITIALIZED WITH A STATIC EXPRESSION.
--
--   THIS IS A TEST BASED ON AI-00505/03-BI-WA.

-- HISTORY:
--         EDWARD V. BERARD, 27 AUGUST 1990
--         DAS   8 OCT 90   ADDED CODE TO MATCH EXAMPLE 1 IN
--                          AI-00505.
--         JRL 05/29/92  CORRECTED MINOR PROBLEM IN REPORT.TEST STRING.
--         JRL 02/18/93  EXPANDED TEXT OF REPORT.TEST STRING.
--         PWN 04/14/95  CORRECTED MINOR COPYRIGHT COMMENT PROBLEM.


WITH REPORT ;

PROCEDURE A49027B IS

BEGIN  -- A49027B

     REPORT.TEST ("A49027B", "CHECK THAT IF A GENERIC ACTUAL " &
                  "PARAMETER IS A STATIC EXPRESSION AND THE " &
                  "CORRESPONDING FORMAL PARAMETER HAS A STATIC " &
                  "SUBTYPE IN THE INSTANCE, THEN EACH USE OF THE " &
                  "FORMAL PARAMETER IN THE INSTANCE IS SAID TO BE " &
                  "STATIC. CHECK THAT A NAME DENOTING A CONSTANT " &
                  "DECLARED IN A GENERIC INSTANCE IS ALLOWED AS " &
                  "A PRIMARY IN A STATIC EXPRESSION IF THE " &
                  "CONSTANT IS DECLARED BY A CONSTANT DECLARATION " &
                  "WITH A STATIC SUBTYPE AND INITIALIZED WITH A " &
                  "STATIC EXPRESSION. (AI-00505)");

     LOCAL_BLOCK:

     DECLARE

          TYPE NUMBER IS RANGE 1 .. 10 ;
          TYPE COLOR IS (RED, ORANGE, YELLOW, GREEN, BLUE) ;
          MIDDLE_COLOR : CONSTANT COLOR := GREEN ;

          ENUMERATED_VALUE : COLOR := COLOR'LAST ;

          GENERIC

               TYPE NUMBER_TYPE IS RANGE <> ;
               X : INTEGER ;
               TYPE ENUMERATED IS (<>) ;

               FIRST_NUMBER            : IN NUMBER_TYPE ;
               SECOND_NUMBER           : IN NUMBER_TYPE ;
               THIRD_NUMBER            : IN NUMBER_TYPE ;
               FIRST_ENUMERATED        : IN ENUMERATED ;
               SECOND_ENUMERATED       : IN ENUMERATED ;
               THIRD_ENUMERATED        : IN ENUMERATED ;

               FIRST_INTEGER_VALUE     : IN INTEGER ;
               SECOND_INTEGER_VALUE    : IN INTEGER ;

          PACKAGE STATIC_TEST IS

               Y : CONSTANT INTEGER := X;
               Z : CONSTANT NUMBER_TYPE := 5;

               SUBTYPE FIRST_NUMBER_SUBTYPE IS NUMBER_TYPE
                   RANGE FIRST_NUMBER .. SECOND_NUMBER ;
               SUBTYPE SECOND_NUMBER_SUBTYPE IS NUMBER_TYPE
                   RANGE FIRST_NUMBER .. THIRD_NUMBER ;

               SUBTYPE FIRST_ENUMERATED_SUBTYPE IS ENUMERATED
                   RANGE FIRST_ENUMERATED .. SECOND_ENUMERATED ;
               SUBTYPE SECOND_ENUMERATED_SUBTYPE IS ENUMERATED
                   RANGE FIRST_ENUMERATED .. THIRD_ENUMERATED ;

               SUBTYPE THIRD_NUMBER_TYPE IS INTEGER
                   RANGE FIRST_INTEGER_VALUE .. SECOND_INTEGER_VALUE ;

          END STATIC_TEST ;

          PACKAGE NEW_STATIC_TEST IS NEW STATIC_TEST
                   (NUMBER_TYPE            => NUMBER,
                    X                      => 3,
                    ENUMERATED             => COLOR,
                    FIRST_NUMBER           => NUMBER'FIRST,
                    SECOND_NUMBER          => NUMBER'LAST,
                    THIRD_NUMBER           => NUMBER'SUCC(NUMBER'FIRST),
                    FIRST_ENUMERATED       => RED,
                    SECOND_ENUMERATED      => MIDDLE_COLOR,
                    THIRD_ENUMERATED       => COLOR'VAL (1),
                    FIRST_INTEGER_VALUE    => COLOR'POS (YELLOW),
                    SECOND_INTEGER_VALUE   => NUMBER'POS (5)) ;

          TYPE T1 IS RANGE 1 .. NEW_STATIC_TEST.Y;
          TYPE T2 IS RANGE 1 .. NEW_STATIC_TEST.Z;

          TYPE ANOTHER_NUMBER IS RANGE
                   NEW_STATIC_TEST.FIRST_NUMBER_SUBTYPE'FIRST ..
                   NEW_STATIC_TEST.FIRST_NUMBER_SUBTYPE'LAST ;

          TYPE YET_ANOTHER_NUMBER IS RANGE
                   NEW_STATIC_TEST.SECOND_NUMBER_SUBTYPE'FIRST ..
                   NEW_STATIC_TEST.SECOND_NUMBER_SUBTYPE'LAST ;

          TYPE STILL_ANOTHER_NUMBER IS RANGE
                   NEW_STATIC_TEST.THIRD_NUMBER_TYPE'FIRST ..
                   NEW_STATIC_TEST.THIRD_NUMBER_TYPE'LAST ;

     BEGIN  -- LOCAL_BLOCK

          CASE ENUMERATED_VALUE IS
                WHEN YELLOW      => NULL ;
                WHEN NEW_STATIC_TEST.FIRST_ENUMERATED_SUBTYPE'FIRST
                                 => NULL ;
                WHEN NEW_STATIC_TEST.FIRST_ENUMERATED_SUBTYPE'LAST
                                 => NULL ;
                WHEN NEW_STATIC_TEST.SECOND_ENUMERATED_SUBTYPE'LAST
                                 => NULL ;
                WHEN COLOR'LAST  => NULL ;
          END CASE ;

     END LOCAL_BLOCK ;

     REPORT.RESULT ;

END A49027B ;
