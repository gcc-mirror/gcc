-- C36205L.ADA

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
-- OBJECTIVE
--      FOR GENERIC PROCEDURES, CHECK THAT ATTRIBUTES GIVE THE
--      CORRECT VALUES FOR UNCONSTRAINED FORMAL PARAMETERS.
--      BASIC CHECKS OF ARRAY OBJECTS AND WHOLE ARRAYS PASSED AS
--      PARAMETERS TO GENERIC PROCEDURES

-- HISTORY
--      EDWARD V. BERARD, 9 AUGUST 1990
--      DAS   8 OCT 1990   ADDED OUT MODE PARAMETER TO GENERIC
--                         PROCEDURE TEST_PROCEDURE AND FORMAL
--                         GENERIC PARAMETER COMPONENT_VALUE.

WITH REPORT ;

PROCEDURE C36205L IS

     SHORT_START : CONSTANT := -100 ;
     SHORT_END   : CONSTANT := 100 ;
     TYPE SHORT_RANGE IS RANGE SHORT_START .. SHORT_END ;
     SHORT_LENGTH : CONSTANT NATURAL := (SHORT_END - SHORT_START + 1) ;

     MEDIUM_START    : CONSTANT := 1 ;
     MEDIUM_END      : CONSTANT := 100 ;
     TYPE MEDIUM_RANGE IS RANGE MEDIUM_START .. MEDIUM_END ;
     MEDIUM_LENGTH : CONSTANT NATURAL := (MEDIUM_END - MEDIUM_START
                                          + 1) ;

     TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                         SEP, OCT, NOV, DEC) ;
     TYPE DAY_TYPE IS RANGE 1 .. 31 ;
     TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
     TYPE DATE IS RECORD
          MONTH : MONTH_TYPE ;
          DAY   : DAY_TYPE ;
          YEAR  : YEAR_TYPE ;
     END RECORD ;

     TODAY : DATE := (MONTH => AUG,
                      DAY   => 9,
                      YEAR  => 1990) ;

     SUBTYPE SHORT_STRING IS STRING (1 ..5) ;

     DEFAULT_STRING : SHORT_STRING := "ABCDE" ;

     TYPE FIRST_TEMPLATE IS ARRAY (SHORT_RANGE RANGE <>,
                                   MEDIUM_RANGE RANGE <>) OF DATE ;

     TYPE SECOND_TEMPLATE IS ARRAY (MONTH_TYPE RANGE <>,
                                    DAY_TYPE RANGE <>) OF SHORT_STRING ;

     TYPE THIRD_TEMPLATE IS ARRAY (CHARACTER RANGE <>,
                                   BOOLEAN RANGE <>) OF DAY_TYPE ;

     FIRST_ARRAY      : FIRST_TEMPLATE (-10 .. 10, 27 .. 35)
                            := (-10 .. 10 =>
                               (27 .. 35 => TODAY)) ;
     SECOND_ARRAY     : SECOND_TEMPLATE (JAN .. JUN, 1 .. 25)
                            := (JAN .. JUN =>
                               (1 .. 25 => DEFAULT_STRING)) ;
     THIRD_ARRAY      : THIRD_TEMPLATE ('A' .. 'Z', FALSE .. TRUE)
                            := ('A' .. 'Z' =>
                               (FALSE .. TRUE => DAY_TYPE (9))) ;

     FOURTH_ARRAY    : FIRST_TEMPLATE (0 .. 27, 75 .. 100)
                            := (0 .. 27 =>
                               (75 .. 100 => TODAY)) ;
     FIFTH_ARRAY     : SECOND_TEMPLATE (JUL .. OCT, 6 .. 10)
                            := (JUL .. OCT =>
                               (6 .. 10 => DEFAULT_STRING)) ;
     SIXTH_ARRAY      : THIRD_TEMPLATE ('X' .. 'Z', TRUE .. TRUE)
                            := ('X' .. 'Z' =>
                               (TRUE .. TRUE => DAY_TYPE (31))) ;

     GENERIC

          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          TYPE UNCONSTRAINED_ARRAY IS ARRAY (FIRST_INDEX RANGE <>,
                    SECOND_INDEX RANGE <>) OF COMPONENT_TYPE ;
          COMPONENT_VALUE: IN  COMPONENT_TYPE;

     PROCEDURE TEST_PROCEDURE (FIRST        : IN UNCONSTRAINED_ARRAY ;
                               FFIFS        : IN FIRST_INDEX ;
                               FFILS        : IN FIRST_INDEX ;
                               FSIFS        : IN SECOND_INDEX ;
                               FSILS        : IN SECOND_INDEX ;
                               FFLEN        : IN NATURAL ;
                               FSLEN        : IN NATURAL ;
                               FFIRT        : IN FIRST_INDEX ;
                               FSIRT        : IN SECOND_INDEX ;
                               SECOND       : OUT UNCONSTRAINED_ARRAY ;
                               SFIFS        : IN FIRST_INDEX ;
                               SFILS        : IN FIRST_INDEX ;
                               SSIFS        : IN SECOND_INDEX ;
                               SSILS        : IN SECOND_INDEX ;
                               SFLEN        : IN NATURAL ;
                               SSLEN        : IN NATURAL ;
                               SFIRT        : IN FIRST_INDEX ;
                               SSIRT        : IN SECOND_INDEX ;
                               REMARKS      : IN STRING) ;

     PROCEDURE TEST_PROCEDURE (FIRST        : IN UNCONSTRAINED_ARRAY ;
                               FFIFS        : IN FIRST_INDEX ;
                               FFILS        : IN FIRST_INDEX ;
                               FSIFS        : IN SECOND_INDEX ;
                               FSILS        : IN SECOND_INDEX ;
                               FFLEN        : IN NATURAL ;
                               FSLEN        : IN NATURAL ;
                               FFIRT        : IN FIRST_INDEX ;
                               FSIRT        : IN SECOND_INDEX ;
                               SECOND       : OUT UNCONSTRAINED_ARRAY ;
                               SFIFS        : IN FIRST_INDEX ;
                               SFILS        : IN FIRST_INDEX ;
                               SSIFS        : IN SECOND_INDEX ;
                               SSILS        : IN SECOND_INDEX ;
                               SFLEN        : IN NATURAL ;
                               SSLEN        : IN NATURAL ;
                               SFIRT        : IN FIRST_INDEX ;
                               SSIRT        : IN SECOND_INDEX ;
                               REMARKS      : IN STRING) IS

     BEGIN -- TEST_PROCEDURE

          IF (FIRST'FIRST /= FFIFS) OR
             (FIRST'FIRST (1) /= FFIFS) OR
             (FIRST'FIRST (2) /= FSIFS) OR
             (SECOND'FIRST /= SFIFS) OR
             (SECOND'FIRST (1) /= SFIFS) OR
             (SECOND'FIRST (2) /= SSIFS) THEN
               REPORT.FAILED ("PROBLEMS WITH 'FIRST. " & REMARKS) ;
          END IF ;

          IF (FIRST'LAST /= FFILS) OR
             (FIRST'LAST (1) /= FFILS) OR
             (FIRST'LAST (2) /= FSILS) OR
             (SECOND'LAST /= SFILS) OR
             (SECOND'LAST (1) /= SFILS) OR
             (SECOND'LAST (2) /= SSILS) THEN
               REPORT.FAILED ("PROBLEMS WITH 'LAST. " & REMARKS) ;
          END IF ;

          IF (FIRST'LENGTH /= FFLEN) OR
             (FIRST'LENGTH (1) /= FFLEN) OR
             (FIRST'LENGTH (2) /= FSLEN) OR
             (SECOND'LENGTH /= SFLEN) OR
             (SECOND'LENGTH (1) /= SFLEN) OR
             (SECOND'LENGTH (2) /= SSLEN) THEN
               REPORT.FAILED ("PROBLEMS WITH 'LENGTH. " & REMARKS) ;
          END IF ;

          IF (FFIRT NOT IN FIRST'RANGE (1)) OR
             (FFIRT NOT IN FIRST'RANGE) OR
             (SFIRT NOT IN SECOND'RANGE (1)) OR
             (SFIRT NOT IN SECOND'RANGE) OR
             (FSIRT NOT IN FIRST'RANGE (2)) OR
             (SSIRT NOT IN SECOND'RANGE (2)) THEN
               REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE " &
                              "ATTRIBUTE.  " & REMARKS) ;
          END IF ;

          -- ASSIGN VALUES TO THE ARRAY PARAMETER OF MODE OUT
          FOR I IN SECOND'RANGE(1) LOOP
               FOR J IN SECOND'RANGE(2) LOOP
                    SECOND(I, J) := COMPONENT_VALUE;
               END LOOP;
          END LOOP;

     END TEST_PROCEDURE ;

     PROCEDURE FIRST_TEST_PROCEDURE IS NEW TEST_PROCEDURE (
          FIRST_INDEX           => SHORT_RANGE,
          SECOND_INDEX          => MEDIUM_RANGE,
          COMPONENT_TYPE        => DATE,
          UNCONSTRAINED_ARRAY   => FIRST_TEMPLATE,
          COMPONENT_VALUE       => TODAY) ;

     PROCEDURE SECOND_TEST_PROCEDURE IS NEW TEST_PROCEDURE (
          FIRST_INDEX           => MONTH_TYPE,
          SECOND_INDEX          => DAY_TYPE,
          COMPONENT_TYPE        => SHORT_STRING,
          UNCONSTRAINED_ARRAY   => SECOND_TEMPLATE,
          COMPONENT_VALUE       => DEFAULT_STRING) ;

     PROCEDURE THIRD_TEST_PROCEDURE IS NEW TEST_PROCEDURE (
          FIRST_INDEX           => CHARACTER,
          SECOND_INDEX          => BOOLEAN,
          COMPONENT_TYPE        => DAY_TYPE,
          UNCONSTRAINED_ARRAY   => THIRD_TEMPLATE,
          COMPONENT_VALUE       => DAY_TYPE'FIRST) ;


BEGIN  -- C36205L

      REPORT.TEST ( "C36205L","FOR GENERIC PROCEDURES, CHECK THAT " &
                    "ATTRIBUTES GIVE THE CORRECT VALUES FOR " &
                    "UNCONSTRAINED FORMAL PARAMETERS.  BASIC " &
                    "CHECKS OF ARRAY OBJECTS AND WHOLE ARRAYS " &
                    "PASSED AS PARAMETERS TO GENERIC PROCEDURES");

     FIRST_TEST_PROCEDURE (FIRST        => FIRST_ARRAY,
                           FFIFS        => -10,
                           FFILS        => 10,
                           FSIFS        => 27,
                           FSILS        => 35,
                           FFLEN        => 21,
                           FSLEN        => 9,
                           FFIRT        => 0,
                           FSIRT        => 29,
                           SECOND       => FOURTH_ARRAY,
                           SFIFS        => 0,
                           SFILS        => 27,
                           SSIFS        => 75,
                           SSILS        => 100,
                           SFLEN        => 28,
                           SSLEN        => 26,
                           SFIRT        => 5,
                           SSIRT        => 100,
                           REMARKS      => "FIRST_TEST_PROCEDURE") ;

     SECOND_TEST_PROCEDURE (FIRST        => SECOND_ARRAY,
                            FFIFS        => JAN,
                            FFILS        => JUN,
                            FSIFS        => 1,
                            FSILS        => 25,
                            FFLEN        => 6,
                            FSLEN        => 25,
                            FFIRT        => MAR,
                            FSIRT        => 17,
                            SECOND       => FIFTH_ARRAY,
                            SFIFS        => JUL,
                            SFILS        => OCT,
                            SSIFS        => 6,
                            SSILS        => 10,
                            SFLEN        => 4,
                            SSLEN        => 5,
                            SFIRT        => JUL,
                            SSIRT        => 6,
                            REMARKS      => "SECOND_TEST_PROCEDURE") ;

     THIRD_TEST_PROCEDURE (FIRST        => THIRD_ARRAY,
                           FFIFS        => 'A',
                           FFILS        => 'Z',
                           FSIFS        => FALSE,
                           FSILS        => TRUE,
                           FFLEN        => 26,
                           FSLEN        => 2,
                           FFIRT        => 'T',
                           FSIRT        => TRUE,
                           SECOND       => SIXTH_ARRAY,
                           SFIFS        => 'X',
                           SFILS        => 'Z',
                           SSIFS        => TRUE,
                           SSILS        => TRUE,
                           SFLEN        => 3,
                           SSLEN        => 1,
                           SFIRT        => 'Z',
                           SSIRT        => TRUE,
                           REMARKS      => "THIRD_TEST_PROCEDURE") ;

      REPORT.RESULT ;

END C36205L ;
