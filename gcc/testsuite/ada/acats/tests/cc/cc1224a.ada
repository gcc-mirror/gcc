-- CC1224A.ADA

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
--     FOR ARRAY TYPES WITH A NONLIMITED COMPONENT TYPE (OF A FORMAL
--     AND NONFORMAL GENERIC TYPE), CHECK THAT THE FOLLOWING OPERATIONS
--     ARE IMPLICITY DECLARED AND ARE, THEREFORE, AVAILABLE WITHIN THE
--     GENERIC UNIT: ASSIGNMENT, THE OPERATION ASSOCIATED WITH
--     AGGREGATE NOTATION, MEMBERSHIP TESTS, THE OPERATION ASSOCIATED
--     WITH INDEXED COMPONENTS, QUALIFICATION, EXPLICIT CONVERSION,
--     'SIZE, 'ADDRESS, 'FIRST, 'FIRST (N), 'LAST, 'LAST (N),
--     'RANGE, 'RANGE (N), 'LENGTH, 'LENGTH (N).

-- HISTORY:
--     R.WILLIAMS  10/6/86
--     EDWARD V. BERARD  8/10/90  ADDED CHECKS FOR MULTI-DIMENSIONAL
--                                ARRAYS
--     LDC  10/10/90  CHANGED DECLARATIONS OF AD1 - AD6 TO PROCEDURE
--                    CALLS OF FA1 - FA6 TO ADDRESS_CHECK AS SUGGESTED
--                    BY THE CRG. 
--     PWN  01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM ;
WITH REPORT ;

PROCEDURE CC1224A IS

     SHORT_START : CONSTANT := -10 ;
     SHORT_END   : CONSTANT := 10 ;

     TYPE SHORT_RANGE IS RANGE SHORT_START .. SHORT_END ;
     SHORT_LENGTH : CONSTANT NATURAL := (SHORT_END - SHORT_START + 1) ;

     MEDIUM_START    : CONSTANT := 1 ;
     MEDIUM_END      : CONSTANT := 15 ;

     TYPE MEDIUM_RANGE IS RANGE MEDIUM_START .. MEDIUM_END ;
     MEDIUM_LENGTH   : CONSTANT NATURAL := 
                                (MEDIUM_END - MEDIUM_START + 1) ;

     TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                         SEP, OCT, NOV, DEC) ;
     TYPE DAY_TYPE IS RANGE 1 .. 31 ;
     TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
     TYPE DATE IS RECORD
          MONTH : MONTH_TYPE ;
          DAY   : DAY_TYPE ;
          YEAR  : YEAR_TYPE ;
     END RECORD ;

     TODAY : DATE := (AUG, 10, 1990) ;

     TYPE FIRST_TEMPLATE IS ARRAY (SHORT_RANGE RANGE <>,
                                   MEDIUM_RANGE RANGE <>) OF DATE ;

     TYPE SECOND_TEMPLATE IS ARRAY (SHORT_RANGE, MEDIUM_RANGE) 
                                   OF DATE ;

     FIRST_ARRAY     : FIRST_TEMPLATE (-10 .. 10, 6 .. 10) ;
     SECOND_ARRAY    : FIRST_TEMPLATE (0 .. 7, 1 .. 15) ;
     THIRD_ARRAY     : SECOND_TEMPLATE ;
     FOURTH_ARRAY    : SECOND_TEMPLATE ;

     SUBTYPE SUBINT IS INTEGER RANGE REPORT.IDENT_INT (1) .. 
                                     REPORT.IDENT_INT (6);

     TYPE ARRA IS ARRAY (SUBINT) OF SUBINT;
     A1 : ARRA := (REPORT.IDENT_INT (1) .. REPORT.IDENT_INT (6) => 1);
     A2 : ARRA := (A1'RANGE => 2);

     TYPE ARRB IS ARRAY (SUBINT RANGE <>) OF DATE ;
     A3 : ARRB (1 .. 6) := 
               (REPORT.IDENT_INT (1) .. REPORT.IDENT_INT (6) => TODAY);

     TYPE ARRC IS ARRAY (SUBINT RANGE <>, SUBINT RANGE <>) OF SUBINT;
     A4 : CONSTANT ARRC := (1 .. 6 => (1 .. 6 => 4));

     TYPE ARRD IS ARRAY (SUBINT, SUBINT) OF SUBINT;
     A5 : ARRD := (A4'RANGE (1) => (A4'RANGE (2) => 5));

     TYPE ARRE IS ARRAY (SUBINT) OF DATE ;
     A6 : ARRE := (A1'RANGE => TODAY);

     FUNCTION "=" (LEFT  : IN SYSTEM.ADDRESS ;
                   RIGHT : IN SYSTEM.ADDRESS ) RETURN BOOLEAN
              RENAMES SYSTEM."=" ;

     GENERIC

          TYPE T1 IS (<>);
          TYPE T2 IS PRIVATE;
          X2 : T2;

          TYPE FARR1 IS ARRAY (SUBINT) OF T1;
          FA1 : FARR1;

          TYPE FARR2 IS ARRAY (SUBINT) OF SUBINT;
          FA2 : FARR2;

          TYPE FARR3 IS ARRAY (SUBINT RANGE <>) OF T2;
          FA3 : FARR3;

          TYPE FARR4 IS ARRAY (SUBINT RANGE <>, SUBINT RANGE <>) OF T1;
          FA4 : FARR4;

          TYPE FARR5 IS ARRAY (SUBINT, SUBINT) OF SUBINT;
          FA5 : FARR5;

          TYPE FARR6 IS ARRAY (T1) OF T2;
          FA6 : FARR6;

          TYPE FARR7 IS ARRAY (T1) OF T2;
          FA7 : FARR7;

     PROCEDURE P ;

     GENERIC

          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE UNCONSTRAINED_ARRAY IS ARRAY 
               (FIRST_INDEX RANGE <>, SECOND_INDEX RANGE <>) OF DATE ;

     PROCEDURE TEST_PROCEDURE (FIRST        : IN UNCONSTRAINED_ARRAY ;
                               FFIFS        : IN FIRST_INDEX ;
                               FFILS        : IN FIRST_INDEX ;
                               FSIFS        : IN SECOND_INDEX ;
                               FSILS        : IN SECOND_INDEX ;
                               FFLEN        : IN NATURAL ;
                               FSLEN        : IN NATURAL ;
                               FFIRT        : IN FIRST_INDEX ;
                               FSIRT        : IN SECOND_INDEX ;
                               SECOND       : IN UNCONSTRAINED_ARRAY ;
                               SFIFS        : IN FIRST_INDEX ;
                               SFILS        : IN FIRST_INDEX ;
                               SSIFS        : IN SECOND_INDEX ;
                               SSILS        : IN SECOND_INDEX ;
                               SFLEN        : IN NATURAL ;
                               SSLEN        : IN NATURAL ;
                               SFIRT        : IN FIRST_INDEX ;
                               SSIRT        : IN SECOND_INDEX ;
                               REMARKS      : IN STRING) ;
     GENERIC

          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          TYPE CONSTRAINED_ARRAY IS ARRAY 
               (FIRST_INDEX,SECOND_INDEX) OF COMPONENT_TYPE ;

     PROCEDURE CTEST_PROCEDURE (FIRST        : IN CONSTRAINED_ARRAY ;
                                FFIRT        : IN FIRST_INDEX ;
                                FSIRT        : IN SECOND_INDEX ;
                                SECOND       : IN CONSTRAINED_ARRAY ;
                                SFIRT        : IN FIRST_INDEX ;
                                SSIRT        : IN SECOND_INDEX ;
                                REMARKS      : IN STRING) ;


     PROCEDURE P IS

          IN1 : INTEGER := FA1'SIZE;
          IN2 : INTEGER := FA2'SIZE;
          IN3 : INTEGER := FA3'SIZE;
          IN4 : INTEGER := FA4'SIZE;
          IN5 : INTEGER := FA5'SIZE;
          IN6 : INTEGER := FA6'SIZE;

          B1 : FARR1;

          B2 : FARR2;

          SUBTYPE SARR3 IS FARR3 (FA3'RANGE);
          B3 : SARR3;

          SUBTYPE SARR4 IS FARR4 (FA4'RANGE (1), FA4'RANGE (2));
          B4 : SARR4;

          B5 : FARR5;

          B6 : FARR6 ;

          PROCEDURE ADDRESS_CHECK(ADDRESS : SYSTEM.ADDRESS) IS

          BEGIN
               IF REPORT.EQUAL(1, REPORT.IDENT_INT(2)) THEN
                    REPORT.COMMENT("DON'T OPTIMIZE OUT ADDRESS_CHECK");
               END IF;
          END ADDRESS_CHECK;

     BEGIN  -- P

          ADDRESS_CHECK(FA1'ADDRESS);
          ADDRESS_CHECK(FA2'ADDRESS);
          ADDRESS_CHECK(FA3'ADDRESS);
          ADDRESS_CHECK(FA4'ADDRESS);
          ADDRESS_CHECK(FA5'ADDRESS);
          ADDRESS_CHECK(FA6'ADDRESS);

          B1 := FA1;

          IF B1 /= FARR1 (FA1) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 1" );
          END IF;

          B2 := FA2;

          IF B2 /= FARR2 (A2) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 2" );
          END IF;

          B3 := FA3;

          IF B3 /= FARR3 (FA3) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 3" );
          END IF;

          B4 := FA4;

          IF B4 /= FARR4 (FA4) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 4" );
          END IF;

          B5 := FA5;

          IF B5 /= FARR5 (A5) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 5" );
          END IF;

          B6 := FA6;

          IF B6 /= FARR6 (FA6) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 6" );
          END IF;

          IF FA7 /= FARR7 (FA6) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 7" );
          END IF;

          B1 := FARR1'(FA1'RANGE => T1'VAL (1));

          IF B1 (1) /= FA1 (1) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 8" );
          END IF;

          B1 := FARR1'(1 => T1'VAL (1), 2 => T1'VAL (1), 
                       3 .. 6 => T1'VAL (2));

          IF B1 (1) /= FA1 (1) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 9" );
          END IF;

          B2 := FARR2'(FA2'RANGE => 2);

          IF B2 (2) /= FA2 (2) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 10" );
          END IF;

          B3 := FARR3'(1|2|3 => X2, 4|5|6 => X2);

          IF B3 (3) /= FA3 (3) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 11" );
          END IF;

          B4 := FARR4'(FA5'RANGE (1) => (FA5'RANGE (2) => T1'VAL (4)));

          IF B4 (4, 4) /= FA4 (4, 4) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 12" );
          END IF;

          B5 := FARR5'(REPORT.IDENT_INT (1) .. 
                       REPORT.IDENT_INT (6) =>  (1 .. 6 => 5));

          IF B5 (5, 5) /= FA5 (5, 5) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 13" );
          END IF;

          B6 := FARR6'(FA6'RANGE => X2);

          IF B6 (T1'FIRST) /= FA6 (T1'FIRST) THEN
               REPORT.FAILED ("INCORRECT RESULTS - 14" );
          END IF;

          IF B1 NOT IN FARR1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 15" );
          END IF;

          IF FA2 NOT IN FARR2 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 16" );
          END IF;

          IF FA3 NOT IN FARR3 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 17" );
          END IF;

          IF B4 NOT IN FARR4 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 18" );
          END IF;

          IF B5 NOT IN FARR5 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 19" );
          END IF;

          IF FA6 NOT IN FARR6 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 20" );
          END IF;

          IF FA1'LENGTH /= FA1'LAST - FA1'FIRST + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 27" );
          END IF;

          IF FA2'LENGTH /= FA2'LAST - FA2'FIRST + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 28" );
          END IF;

          IF FA3'LENGTH /= FA3'LAST - FA3'FIRST + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 29" );
          END IF;

          IF FA4'LENGTH /= FA4'LAST - FA4'FIRST + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 30" );
          END IF;

          IF FA4'LENGTH (2) /= FA4'LAST (2) - FA4'FIRST (2) + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 31" );
          END IF;

          IF FA5'LENGTH /= FA5'LAST - FA5'FIRST + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 32" );
          END IF;

          IF FA5'LENGTH (2) /= FA5'LAST (2) - FA5'FIRST (2) + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 33" );
          END IF;

          IF FA6'LENGTH /= T1'POS (FA6'LAST) - 
             T1'POS (FA6'FIRST) + 1 THEN
               REPORT.FAILED ("INCORRECT RESULTS - 34" );
          END IF;

     END P ;

     PROCEDURE TEST_PROCEDURE (FIRST        : IN UNCONSTRAINED_ARRAY ;
                               FFIFS        : IN FIRST_INDEX ;
                               FFILS        : IN FIRST_INDEX ;
                               FSIFS        : IN SECOND_INDEX ;
                               FSILS        : IN SECOND_INDEX ;
                               FFLEN        : IN NATURAL ;
                               FSLEN        : IN NATURAL ;
                               FFIRT        : IN FIRST_INDEX ;
                               FSIRT        : IN SECOND_INDEX ;
                               SECOND       : IN UNCONSTRAINED_ARRAY ;
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
               REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUE. " &
                              REMARKS) ;
          END IF ;

     END TEST_PROCEDURE ;

     PROCEDURE CTEST_PROCEDURE (FIRST        : IN CONSTRAINED_ARRAY ;
                                FFIRT        : IN FIRST_INDEX ;
                                FSIRT        : IN SECOND_INDEX ;
                                SECOND       : IN CONSTRAINED_ARRAY ;
                                SFIRT        : IN FIRST_INDEX ;
                                SSIRT        : IN SECOND_INDEX ;
                                REMARKS      : IN STRING) IS

     BEGIN -- CTEST_PROCEDURE

          IF (FIRST'FIRST /= FIRST_INDEX'FIRST) OR
             (FIRST'FIRST (1) /= FIRST_INDEX'FIRST) OR
             (FIRST'FIRST (2) /= SECOND_INDEX'FIRST) OR
             (SECOND'FIRST /= FIRST_INDEX'FIRST) OR
             (SECOND'FIRST (1) /= FIRST_INDEX'FIRST) OR
             (SECOND'FIRST (2) /= SECOND_INDEX'FIRST) THEN
               REPORT.FAILED ("PROBLEMS WITH 'FIRST. " & REMARKS) ;
          END IF ;

          IF (FIRST'LAST /= FIRST_INDEX'LAST) OR
             (FIRST'LAST (1) /= FIRST_INDEX'LAST) OR
             (FIRST'LAST (2) /= SECOND_INDEX'LAST) OR
             (SECOND'LAST /= FIRST_INDEX'LAST) OR
             (SECOND'LAST (1) /= FIRST_INDEX'LAST) OR
             (SECOND'LAST (2) /= SECOND_INDEX'LAST) THEN
               REPORT.FAILED ("PROBLEMS WITH 'LAST. " & REMARKS) ;
          END IF ;

          IF (FIRST'LENGTH /=
              FIRST_INDEX'POS (FIRST_INDEX'LAST)
              - FIRST_INDEX'POS (FIRST_INDEX'FIRST) + 1) OR
             (FIRST'LENGTH (1) /= 
              FIRST_INDEX'POS (FIRST_INDEX'LAST)
              - FIRST_INDEX'POS (FIRST_INDEX'FIRST) + 1) OR
             (FIRST'LENGTH (2) /=
              SECOND_INDEX'POS (SECOND_INDEX'LAST)
              - SECOND_INDEX'POS (SECOND_INDEX'FIRST) + 1) OR
             (SECOND'LENGTH /= 
              FIRST_INDEX'POS (FIRST_INDEX'LAST)
              - FIRST_INDEX'POS (FIRST_INDEX'FIRST) + 1) OR
             (SECOND'LENGTH (1) /=
              FIRST_INDEX'POS (FIRST_INDEX'LAST)
              - FIRST_INDEX'POS (FIRST_INDEX'FIRST) + 1) OR
             (SECOND'LENGTH (2) /=
              SECOND_INDEX'POS (SECOND_INDEX'LAST)
              - SECOND_INDEX'POS (SECOND_INDEX'FIRST) + 1) THEN
               REPORT.FAILED ("PROBLEMS WITH 'LENGTH. " & REMARKS) ;
          END IF ;

          IF (FFIRT NOT IN FIRST'RANGE (1)) OR
             (FFIRT NOT IN FIRST'RANGE) OR
             (SFIRT NOT IN SECOND'RANGE (1)) OR
             (SFIRT NOT IN SECOND'RANGE) OR
             (FSIRT NOT IN FIRST'RANGE (2)) OR
             (SSIRT NOT IN SECOND'RANGE (2)) THEN
                REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUE. " &
                               REMARKS) ;
          END IF ;

          IF CONSTRAINED_ARRAY'SIZE <= 0 THEN
               REPORT.FAILED ("PROBLEMS WITH THE 'SIZE ATTRIBUTE. " &
                              REMARKS) ;
          END IF ;

          IF FIRST'ADDRESS = SECOND'ADDRESS THEN
               REPORT.FAILED ("PROBLEMS WITH THE 'ADDRESS ATTRIBUTE. " &
                              REMARKS) ;
          END IF ;

     END CTEST_PROCEDURE ;

     PROCEDURE FIRST_TEST_PROCEDURE IS NEW TEST_PROCEDURE 
               (FIRST_INDEX            => SHORT_RANGE,
                SECOND_INDEX           => MEDIUM_RANGE,
                UNCONSTRAINED_ARRAY    => FIRST_TEMPLATE) ;

     PROCEDURE NEW_CTEST_PROCEDURE IS NEW CTEST_PROCEDURE 
               (FIRST_INDEX           => SHORT_RANGE,
                SECOND_INDEX          => MEDIUM_RANGE,
                COMPONENT_TYPE        => DATE,
                CONSTRAINED_ARRAY     => SECOND_TEMPLATE) ;

     PROCEDURE NP IS NEW P (SUBINT, DATE, TODAY, ARRA, A1, 
                            ARRA, A2, ARRB, A3, ARRC, A4, ARRD,
                            A5, ARRE, A6, ARRE, A6);

BEGIN  -- CC1224A

     REPORT.TEST ("CC1224A", "FOR ARRAY TYPES WITH A NONLIMITED " &
                  "COMPONENT TYPE (OF A FORMAL AND NONFORMAL GENERIC " &
                  "TYPE), CHECK THAT THE FOLLOWING OPERATIONS " &
                  "ARE IMPLICITY DECLARED AND ARE, THEREFORE, " &
                  "AVAILABLE WITHIN THE GENERIC -- UNIT: " &
                  "ASSIGNMENT, THE OPERATION ASSOCIATED WITH " &
                  "AGGREGATE NOTATION, MEMBERSHIP TESTS, THE " &
                  "OPERATION ASSOCIATED WITH INDEXED " &
                  "COMPONENTS, QUALIFICATION, EXPLICIT " &
                  "CONVERSION, 'SIZE, 'ADDRESS, 'FIRST, " &
                  "'FIRST (N), 'LAST, 'LAST (N), 'RANGE, " &
                  "'RANGE (N), 'LENGTH, 'LENGTH (N)" ) ;

     NP ;

     FIRST_TEST_PROCEDURE (FIRST        => FIRST_ARRAY,
                           FFIFS        => -10,
                           FFILS        => 10,
                           FSIFS        => 6,
                           FSILS        => 10,
                           FFLEN        => 21,
                           FSLEN        => 5,
                           FFIRT        => 0,
                           FSIRT        => 8,
                           SECOND       => SECOND_ARRAY,
                           SFIFS        => 0,
                           SFILS        => 7,
                           SSIFS        => 1,
                           SSILS        => 15,
                           SFLEN        => 8,
                           SSLEN        => 15,
                           SFIRT        => 5,
                           SSIRT        => 13,
                           REMARKS      => "FIRST_TEST_PROCEDURE") ;

     NEW_CTEST_PROCEDURE (FIRST         => THIRD_ARRAY,
                          FFIRT         => -5,
                          FSIRT         => 11,
                          SECOND        => FOURTH_ARRAY,
                          SFIRT         => 0,
                          SSIRT         => 14,
                          REMARKS       => "NEW_CTEST_PROCEDURE") ;

     REPORT.RESULT ;

END CC1224A;
