-- C36204D.ADA

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
-- CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES.
-- BOTH ARRAY OBJECTS AND TYPES ARE CHECKED. THIS TEST CHECKS 
-- THE ABOVE FOR ARRAYS WITHIN GENERIC PROGRAM UNITS.

-- HISTROY
--  EDWARD V. BERARD, 9 AUGUST 1990

WITH REPORT ;
WITH SYSTEM ;

PROCEDURE C36204D IS

    SHORT_START : CONSTANT := -10 ;
    SHORT_END    : CONSTANT := 10 ;
    TYPE SHORT_RANGE IS RANGE SHORT_START .. SHORT_END ;
    SHORT_LENGTH : CONSTANT NATURAL := (SHORT_END - SHORT_START + 1) ;
    
    TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                        SEP, OCT, NOV, DEC) ;
    SUBTYPE MID_YEAR IS MONTH_TYPE RANGE MAY .. AUG ;
    TYPE DAY_TYPE IS RANGE 1 .. 31 ;
    TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
    TYPE DATE IS RECORD
      MONTH : MONTH_TYPE ;
      DAY   : DAY_TYPE ;
      YEAR  : YEAR_TYPE ;
    END RECORD ;
    
    TODAY         : DATE := (MONTH => AUG,
                             DAY   => 10,
                             YEAR  => 1990) ;
                            
    FIRST_DATE     : DATE := (DAY   => 6,
                              MONTH => JUN,
                              YEAR  => 1967) ;
                            
    FUNCTION "=" (LEFT  : IN SYSTEM.ADDRESS ;
                  RIGHT : IN SYSTEM.ADDRESS ) RETURN BOOLEAN
            RENAMES SYSTEM."=" ;

    GENERIC
    
        TYPE FIRST_INDEX IS (<>) ;
        FIRST_INDEX_LENGTH : IN NATURAL ;
        FIRST_TEST_VALUE : IN FIRST_INDEX ;
        TYPE SECOND_INDEX IS (<>) ;
        SECOND_INDEX_LENGTH : IN NATURAL ;
        SECOND_TEST_VALUE : IN SECOND_INDEX ;
        TYPE THIRD_INDEX IS (<>) ;
        THIRD_INDEX_LENGTH : IN NATURAL ;
        THIRD_TEST_VALUE : IN THIRD_INDEX ;
        TYPE FIRST_COMPONENT_TYPE IS PRIVATE ;
        FIRST_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        SECOND_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        TYPE SECOND_COMPONENT_TYPE IS PRIVATE ;
        THIRD_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
        FOURTH_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
    
    PACKAGE ARRAY_ATTRIBUTE_TEST IS
    
        TYPE MATRIX IS ARRAY (FIRST_INDEX, SECOND_INDEX)
            OF FIRST_COMPONENT_TYPE ;
            
        TYPE CUBE IS ARRAY (FIRST_INDEX, SECOND_INDEX, THIRD_INDEX)
            OF SECOND_COMPONENT_TYPE ;
            
    END ARRAY_ATTRIBUTE_TEST ;
    
    PACKAGE BODY ARRAY_ATTRIBUTE_TEST IS
    
        FIRST_ARRAY : MATRIX := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    FIRST_DEFAULT_VALUE)) ;
        
        SECOND_ARRAY : CUBE := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       THIRD_DEFAULT_VALUE))) ;
                                    
        THIRD_ARRAY : CONSTANT MATRIX 
                             := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    SECOND_DEFAULT_VALUE)) ;
                                        
        FOURTH_ARRAY : CONSTANT CUBE 
                            := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       FOURTH_DEFAULT_VALUE))) ;
                                    
        FA1 : FIRST_INDEX := FIRST_ARRAY'FIRST (1) ;
        FA2 : FIRST_INDEX := FIRST_ARRAY'LAST (1) ;
        FA3 : SECOND_INDEX := FIRST_ARRAY'FIRST (2) ;
        FA4 : SECOND_INDEX := FIRST_ARRAY'LAST (2) ;
        
        SA1 : FIRST_INDEX := SECOND_ARRAY'FIRST (1) ;
        SA2 : FIRST_INDEX := SECOND_ARRAY'LAST (1) ;
        SA3 : SECOND_INDEX := SECOND_ARRAY'FIRST (2) ;
        SA4 : SECOND_INDEX := SECOND_ARRAY'LAST (2) ;
        SA5 : THIRD_INDEX := SECOND_ARRAY'FIRST (3) ;
        SA6 : THIRD_INDEX := SECOND_ARRAY'LAST (3) ;
        
        FAL1 : NATURAL := FIRST_ARRAY'LENGTH (1) ;
        FAL2 : NATURAL := FIRST_ARRAY'LENGTH (2) ;
        
        SAL1 : NATURAL := SECOND_ARRAY'LENGTH (1) ;
        SAL2 : NATURAL := SECOND_ARRAY'LENGTH (2) ;
        SAL3 : NATURAL := SECOND_ARRAY'LENGTH (3) ;
        
        MATRIX_SIZE : NATURAL := MATRIX'SIZE ;
        CUBE_SIZE    : NATURAL := CUBE'SIZE ;
        
        FAA  : SYSTEM.ADDRESS := FIRST_ARRAY'ADDRESS ;
        SAA  : SYSTEM.ADDRESS := SECOND_ARRAY'ADDRESS ;
        TAA  : SYSTEM.ADDRESS := THIRD_ARRAY'ADDRESS ;
        FRAA : SYSTEM.ADDRESS := FOURTH_ARRAY'ADDRESS ;
             
     BEGIN  -- ARRAY_ATTRIBUTE_TEST
     
        IF (FA1 /= FIRST_INDEX'FIRST) OR
           (FA3 /= SECOND_INDEX'FIRST) OR
           (SA1 /= FIRST_INDEX'FIRST) OR
           (SA3 /= SECOND_INDEX'FIRST) OR
           (SA5 /= THIRD_INDEX'FIRST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'FIRST - PACKAGE") ;
        END IF ;
        
        IF (FA2 /= FIRST_INDEX'LAST) OR
           (FA4 /= SECOND_INDEX'LAST) OR
           (SA2 /= FIRST_INDEX'LAST) OR
           (SA4 /= SECOND_INDEX'LAST) OR
           (SA6 /= THIRD_INDEX'LAST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LAST - PACKAGE") ;
        END IF ;
        
        IF (FAL1 /= FIRST_INDEX_LENGTH) OR
           (FAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL1 /= FIRST_INDEX_LENGTH) OR
           (SAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL3 /= THIRD_INDEX_LENGTH) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LENGTH - PACKAGE") ;
        END IF ;
        
        FOR OUTER_INDEX IN FIRST_ARRAY'RANGE (1) LOOP
            FOR INNER_INDEX IN FIRST_ARRAY'RANGE (2) LOOP
                FIRST_ARRAY (OUTER_INDEX, INNER_INDEX) :=
                    SECOND_DEFAULT_VALUE ;
            END LOOP ;
        END LOOP ;
        
        IF FIRST_ARRAY /= THIRD_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 2-DIMENSIONAL ARRAY. - PACKAGE") ;
        END IF ;
        
        FOR OUTER_INDEX IN SECOND_ARRAY'RANGE (1) LOOP
            FOR MIDDLE_INDEX IN SECOND_ARRAY'RANGE (2) LOOP
                FOR INNER_INDEX IN SECOND_ARRAY'RANGE (3) LOOP
                    SECOND_ARRAY (OUTER_INDEX, MIDDLE_INDEX, INNER_INDEX)
                        := FOURTH_DEFAULT_VALUE ;
                END LOOP ;
            END LOOP ;
        END LOOP ;
        
        IF SECOND_ARRAY /= FOURTH_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 3-DIMENSIONAL ARRAY. - PACKAGE") ;
        END IF ;
        
        IF (FIRST_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (1)) OR
           (FIRST_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (1)) OR
           (SECOND_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (2)) OR
           (SECOND_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (2)) OR
           (THIRD_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (3)) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "- PACKAGE") ;
        END IF ;
        
        IF (MATRIX_SIZE = 0) OR (CUBE_SIZE = 0) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " &
                           "- PACKAGE") ;
        END IF ;
        
        IF (FAA = TAA) OR (SAA = FRAA) OR (FAA = SAA) OR (FAA = FRAA)
           OR (SAA = TAA) OR (TAA = FRAA) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " &
                           "- PACKAGE") ;
        END IF ;
        
    END ARRAY_ATTRIBUTE_TEST ;

    GENERIC
    
        TYPE FIRST_INDEX IS (<>) ;
        FIRST_INDEX_LENGTH : IN NATURAL ;
        FIRST_TEST_VALUE : IN FIRST_INDEX ;
        TYPE SECOND_INDEX IS (<>) ;
        SECOND_INDEX_LENGTH : IN NATURAL ;
        SECOND_TEST_VALUE : IN SECOND_INDEX ;
        TYPE THIRD_INDEX IS (<>) ;
        THIRD_INDEX_LENGTH : IN NATURAL ;
        THIRD_TEST_VALUE : IN THIRD_INDEX ;
        TYPE FIRST_COMPONENT_TYPE IS PRIVATE ;
        FIRST_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        SECOND_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        TYPE SECOND_COMPONENT_TYPE IS PRIVATE ;
        THIRD_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
        FOURTH_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
        
    PROCEDURE PROC_ARRAY_ATT_TEST ;
    
    PROCEDURE PROC_ARRAY_ATT_TEST IS
    
        TYPE MATRIX IS ARRAY (FIRST_INDEX, SECOND_INDEX)
            OF FIRST_COMPONENT_TYPE ;
            
        TYPE CUBE IS ARRAY (FIRST_INDEX, SECOND_INDEX, THIRD_INDEX)
            OF SECOND_COMPONENT_TYPE ;
            
        FIRST_ARRAY : MATRIX := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    FIRST_DEFAULT_VALUE)) ;
        
        SECOND_ARRAY : CUBE := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       THIRD_DEFAULT_VALUE))) ;
                                    
        THIRD_ARRAY : CONSTANT MATRIX 
                             := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    SECOND_DEFAULT_VALUE)) ;
                                        
        FOURTH_ARRAY : CONSTANT CUBE 
                            := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       FOURTH_DEFAULT_VALUE))) ;
                                    
        FA1 : FIRST_INDEX := FIRST_ARRAY'FIRST (1) ;
        FA2 : FIRST_INDEX := FIRST_ARRAY'LAST (1) ;
        FA3 : SECOND_INDEX := FIRST_ARRAY'FIRST (2) ;
        FA4 : SECOND_INDEX := FIRST_ARRAY'LAST (2) ;
        
        SA1 : FIRST_INDEX := SECOND_ARRAY'FIRST (1) ;
        SA2 : FIRST_INDEX := SECOND_ARRAY'LAST (1) ;
        SA3 : SECOND_INDEX := SECOND_ARRAY'FIRST (2) ;
        SA4 : SECOND_INDEX := SECOND_ARRAY'LAST (2) ;
        SA5 : THIRD_INDEX := SECOND_ARRAY'FIRST (3) ;
        SA6 : THIRD_INDEX := SECOND_ARRAY'LAST (3) ;
        
        FAL1 : NATURAL := FIRST_ARRAY'LENGTH (1) ;
        FAL2 : NATURAL := FIRST_ARRAY'LENGTH (2) ;
        
        SAL1 : NATURAL := SECOND_ARRAY'LENGTH (1) ;
        SAL2 : NATURAL := SECOND_ARRAY'LENGTH (2) ;
        SAL3 : NATURAL := SECOND_ARRAY'LENGTH (3) ;
        
        MATRIX_SIZE : NATURAL := MATRIX'SIZE ;
        CUBE_SIZE    : NATURAL := CUBE'SIZE ;
        
        FAA : SYSTEM.ADDRESS := FIRST_ARRAY'ADDRESS ;
        SAA : SYSTEM.ADDRESS := SECOND_ARRAY'ADDRESS ;
        TAA : SYSTEM.ADDRESS := THIRD_ARRAY'ADDRESS ;
        FRAA : SYSTEM.ADDRESS := FOURTH_ARRAY'ADDRESS ;
             
     BEGIN  -- PROC_ARRAY_ATT_TEST

        IF (FA1 /= FIRST_INDEX'FIRST) OR
           (FA3 /= SECOND_INDEX'FIRST) OR
           (SA1 /= FIRST_INDEX'FIRST) OR
           (SA3 /= SECOND_INDEX'FIRST) OR
           (SA5 /= THIRD_INDEX'FIRST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'FIRST " &
                           "- PROCEDURE") ;
        END IF ;
        
        IF (FA2 /= FIRST_INDEX'LAST) OR
           (FA4 /= SECOND_INDEX'LAST) OR
           (SA2 /= FIRST_INDEX'LAST) OR
           (SA4 /= SECOND_INDEX'LAST) OR
           (SA6 /= THIRD_INDEX'LAST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LAST " &
                           "- PROCEDURE") ;
        END IF ;
        
        IF (FAL1 /= FIRST_INDEX_LENGTH) OR
           (FAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL1 /= FIRST_INDEX_LENGTH) OR
           (SAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL3 /= THIRD_INDEX_LENGTH) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LENGTH " &
                           "- PROCEDURE") ;
        END IF ;
        
        FOR OUTER_INDEX IN FIRST_ARRAY'RANGE (1) LOOP
            FOR INNER_INDEX IN FIRST_ARRAY'RANGE (2) LOOP
                FIRST_ARRAY (OUTER_INDEX, INNER_INDEX) :=
                    SECOND_DEFAULT_VALUE ;
            END LOOP ;
        END LOOP ;
        
        IF FIRST_ARRAY /= THIRD_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 2-DIMENSIONAL ARRAY. - PROCEDURE") ;
        END IF ;
        
        FOR OUTER_INDEX IN SECOND_ARRAY'RANGE (1) LOOP
            FOR MIDDLE_INDEX IN SECOND_ARRAY'RANGE (2) LOOP
                FOR INNER_INDEX IN SECOND_ARRAY'RANGE (3) LOOP
                    SECOND_ARRAY (OUTER_INDEX, MIDDLE_INDEX, INNER_INDEX)
                        := FOURTH_DEFAULT_VALUE ;
                END LOOP ;
            END LOOP ;
        END LOOP ;
        
        IF SECOND_ARRAY /= FOURTH_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 3-DIMENSIONAL ARRAY. - PROCEDURE") ;
        END IF ;
        
        IF (FIRST_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (1)) OR
           (FIRST_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (1)) OR
           (SECOND_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (2)) OR
           (SECOND_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (2)) OR
           (THIRD_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (3)) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "- PROCEDURE") ;
        END IF ;
        
        IF (MATRIX_SIZE = 0) OR (CUBE_SIZE = 0) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " &
                           "- PROCEDURE") ;
        END IF ;
        
        IF (FAA = TAA) OR (SAA = FRAA) OR (FAA = SAA) OR (FAA = FRAA)
           OR (SAA = TAA) OR (TAA = FRAA) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " &
                           "- PROCEDURE") ;
        END IF ;
        
    END PROC_ARRAY_ATT_TEST ;

    GENERIC
    
        TYPE FIRST_INDEX IS (<>) ;
        FIRST_INDEX_LENGTH : IN NATURAL ;
        FIRST_TEST_VALUE : IN FIRST_INDEX ;
        TYPE SECOND_INDEX IS (<>) ;
        SECOND_INDEX_LENGTH : IN NATURAL ;
        SECOND_TEST_VALUE : IN SECOND_INDEX ;
        TYPE THIRD_INDEX IS (<>) ;
        THIRD_INDEX_LENGTH : IN NATURAL ;
        THIRD_TEST_VALUE : IN THIRD_INDEX ;
        TYPE FIRST_COMPONENT_TYPE IS PRIVATE ;
        FIRST_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        SECOND_DEFAULT_VALUE : IN FIRST_COMPONENT_TYPE ;
        TYPE SECOND_COMPONENT_TYPE IS PRIVATE ;
        THIRD_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
        FOURTH_DEFAULT_VALUE : IN SECOND_COMPONENT_TYPE ;
        
    FUNCTION FUNC_ARRAY_ATT_TEST RETURN BOOLEAN ;
    
    FUNCTION FUNC_ARRAY_ATT_TEST RETURN BOOLEAN IS
    
        TYPE MATRIX IS ARRAY (FIRST_INDEX, SECOND_INDEX)
            OF FIRST_COMPONENT_TYPE ;
            
        TYPE CUBE IS ARRAY (FIRST_INDEX, SECOND_INDEX, THIRD_INDEX)
            OF SECOND_COMPONENT_TYPE ;
            
        FIRST_ARRAY : MATRIX := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    FIRST_DEFAULT_VALUE)) ;
        
        SECOND_ARRAY : CUBE := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       THIRD_DEFAULT_VALUE))) ;
                                    
        THIRD_ARRAY : CONSTANT MATRIX 
                             := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                                (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                                    SECOND_DEFAULT_VALUE)) ;
                                        
        FOURTH_ARRAY : CONSTANT CUBE 
                            := (FIRST_INDEX'FIRST .. FIRST_INDEX'LAST =>
                               (SECOND_INDEX'FIRST .. SECOND_INDEX'LAST =>
                               (THIRD_INDEX'FIRST .. THIRD_INDEX'LAST =>
                                       FOURTH_DEFAULT_VALUE))) ;
                                    
        FA1 : FIRST_INDEX := FIRST_ARRAY'FIRST (1) ;
        FA2 : FIRST_INDEX := FIRST_ARRAY'LAST (1) ;
        FA3 : SECOND_INDEX := FIRST_ARRAY'FIRST (2) ;
        FA4 : SECOND_INDEX := FIRST_ARRAY'LAST (2) ;
        
        SA1 : FIRST_INDEX := SECOND_ARRAY'FIRST (1) ;
        SA2 : FIRST_INDEX := SECOND_ARRAY'LAST (1) ;
        SA3 : SECOND_INDEX := SECOND_ARRAY'FIRST (2) ;
        SA4 : SECOND_INDEX := SECOND_ARRAY'LAST (2) ;
        SA5 : THIRD_INDEX := SECOND_ARRAY'FIRST (3) ;
        SA6 : THIRD_INDEX := SECOND_ARRAY'LAST (3) ;
        
        FAL1 : NATURAL := FIRST_ARRAY'LENGTH (1) ;
        FAL2 : NATURAL := FIRST_ARRAY'LENGTH (2) ;
        
        SAL1 : NATURAL := SECOND_ARRAY'LENGTH (1) ;
        SAL2 : NATURAL := SECOND_ARRAY'LENGTH (2) ;
        SAL3 : NATURAL := SECOND_ARRAY'LENGTH (3) ;
        
        MATRIX_SIZE : NATURAL := MATRIX'SIZE ;
        CUBE_SIZE    : NATURAL := CUBE'SIZE ;
        
        FAA : SYSTEM.ADDRESS := FIRST_ARRAY'ADDRESS ;
        SAA : SYSTEM.ADDRESS := SECOND_ARRAY'ADDRESS ;
        TAA : SYSTEM.ADDRESS := THIRD_ARRAY'ADDRESS ;
        FRAA : SYSTEM.ADDRESS := FOURTH_ARRAY'ADDRESS ;
             
     BEGIN  -- FUNC_ARRAY_ATT_TEST

        IF (FA1 /= FIRST_INDEX'FIRST) OR
           (FA3 /= SECOND_INDEX'FIRST) OR
           (SA1 /= FIRST_INDEX'FIRST) OR
           (SA3 /= SECOND_INDEX'FIRST) OR
           (SA5 /= THIRD_INDEX'FIRST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'FIRST " &
                           "- FUNCTION") ;
        END IF ;
        
        IF (FA2 /= FIRST_INDEX'LAST) OR
           (FA4 /= SECOND_INDEX'LAST) OR
           (SA2 /= FIRST_INDEX'LAST) OR
           (SA4 /= SECOND_INDEX'LAST) OR
           (SA6 /= THIRD_INDEX'LAST) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LAST " &
                           "- FUNCTION") ;
        END IF ;
        
        IF (FAL1 /= FIRST_INDEX_LENGTH) OR
           (FAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL1 /= FIRST_INDEX_LENGTH) OR
           (SAL2 /= SECOND_INDEX_LENGTH) OR
           (SAL3 /= THIRD_INDEX_LENGTH) THEN
            REPORT.FAILED ("INCORRECT VALUE RETURNED FOR 'LENGTH " &
                           "- FUNCTION") ;
        END IF ;
        
        FOR OUTER_INDEX IN FIRST_ARRAY'RANGE (1) LOOP
            FOR INNER_INDEX IN FIRST_ARRAY'RANGE (2) LOOP
                FIRST_ARRAY (OUTER_INDEX, INNER_INDEX) :=
                    SECOND_DEFAULT_VALUE ;
            END LOOP ;
        END LOOP ;
        
        IF FIRST_ARRAY /= THIRD_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 2-DIMENSIONAL ARRAY. - FUNCTION") ;
        END IF ;
        
        FOR OUTER_INDEX IN SECOND_ARRAY'RANGE (1) LOOP
            FOR MIDDLE_INDEX IN SECOND_ARRAY'RANGE (2) LOOP
                FOR INNER_INDEX IN SECOND_ARRAY'RANGE (3) LOOP
                    SECOND_ARRAY (OUTER_INDEX, MIDDLE_INDEX, INNER_INDEX)
                        := FOURTH_DEFAULT_VALUE ;
                END LOOP ;
            END LOOP ;
        END LOOP ;
        
        IF SECOND_ARRAY /= FOURTH_ARRAY THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "FOR 3-DIMENSIONAL ARRAY. - FUNCTION") ;
        END IF ;
        
        IF (FIRST_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (1)) OR
           (FIRST_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (1)) OR
           (SECOND_TEST_VALUE NOT IN FIRST_ARRAY'RANGE (2)) OR
           (SECOND_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (2)) OR
           (THIRD_TEST_VALUE NOT IN SECOND_ARRAY'RANGE (3)) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF 'RANGE ATTRIBUTE " &
                           "- FUNCTION") ;
        END IF ;
        
        IF (MATRIX_SIZE = 0) OR (CUBE_SIZE = 0) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'SIZE ATTRIBUTE. " &
                           "- FUNCTION") ;
        END IF ;
        
        IF (FAA = TAA) OR (SAA = FRAA) OR (FAA = SAA) OR (FAA = FRAA)
           OR (SAA = TAA) OR (TAA = FRAA) THEN
            REPORT.FAILED ("INCORRECT HANDLING OF THE 'ADDRESS ATTRIBUTE. " &
                           "- FUNCTION") ;
        END IF ;
        
        RETURN TRUE ;
        
    END FUNC_ARRAY_ATT_TEST ;


BEGIN -- C36204D

    REPORT.TEST ("C36204D", "ARRAY ATTRIBUTES RETURN CORRECT " &
                  "VALUES WITHIN GENERIC PROGRAM UNITS.") ;
     
    LOCAL_BLOCK:
    
    DECLARE
    
        DUMMY : BOOLEAN := FALSE ;
    
        PACKAGE NEW_ARRAY_ATTRIBUTE_TEST IS NEW ARRAY_ATTRIBUTE_TEST (
            FIRST_INDEX             => SHORT_RANGE,
            FIRST_INDEX_LENGTH      => SHORT_LENGTH,
            FIRST_TEST_VALUE        => -7,
            SECOND_INDEX            => MONTH_TYPE,
            SECOND_INDEX_LENGTH     => 12,
            SECOND_TEST_VALUE       => AUG,
            THIRD_INDEX             => BOOLEAN,
            THIRD_INDEX_LENGTH      => 2,
            THIRD_TEST_VALUE        => FALSE,
            FIRST_COMPONENT_TYPE    => MONTH_TYPE,
            FIRST_DEFAULT_VALUE     => JAN,
            SECOND_DEFAULT_VALUE    => DEC,
            SECOND_COMPONENT_TYPE   => DATE,
            THIRD_DEFAULT_VALUE     => TODAY,
            FOURTH_DEFAULT_VALUE    => FIRST_DATE) ;
        
        PROCEDURE NEW_PROC_ARRAY_ATT_TEST IS NEW PROC_ARRAY_ATT_TEST (
            FIRST_INDEX             => MONTH_TYPE,
            FIRST_INDEX_LENGTH      => 12,
            FIRST_TEST_VALUE        => AUG,
            SECOND_INDEX            => SHORT_RANGE,
            SECOND_INDEX_LENGTH     => SHORT_LENGTH,
            SECOND_TEST_VALUE       => -7,
            THIRD_INDEX             => BOOLEAN,
            THIRD_INDEX_LENGTH      => 2,
            THIRD_TEST_VALUE        => FALSE,
            FIRST_COMPONENT_TYPE    => DATE,
            FIRST_DEFAULT_VALUE     => TODAY,
            SECOND_DEFAULT_VALUE    => FIRST_DATE,
            SECOND_COMPONENT_TYPE   => MONTH_TYPE,
            THIRD_DEFAULT_VALUE     => JAN,
            FOURTH_DEFAULT_VALUE    => DEC) ;

        FUNCTION NEW_FUNC_ARRAY_ATT_TEST IS NEW FUNC_ARRAY_ATT_TEST (
            FIRST_INDEX             => DAY_TYPE,
            FIRST_INDEX_LENGTH      => 31,
            FIRST_TEST_VALUE        => 25,
            SECOND_INDEX            => SHORT_RANGE,
            SECOND_INDEX_LENGTH     => SHORT_LENGTH,
            SECOND_TEST_VALUE       => -7,
            THIRD_INDEX             => MID_YEAR,
            THIRD_INDEX_LENGTH      => 4,
            THIRD_TEST_VALUE        => JUL,
            FIRST_COMPONENT_TYPE    => DATE,
            FIRST_DEFAULT_VALUE     => TODAY,
            SECOND_DEFAULT_VALUE    => FIRST_DATE,
            SECOND_COMPONENT_TYPE   => MONTH_TYPE,
            THIRD_DEFAULT_VALUE     => JAN,
            FOURTH_DEFAULT_VALUE    => DEC) ;

    BEGIN  -- LOCAL_BLOCK
    
        NEW_PROC_ARRAY_ATT_TEST ;
        
        DUMMY := NEW_FUNC_ARRAY_ATT_TEST ;
        IF NOT DUMMY THEN
            REPORT.FAILED ("WRONG VALUE RETURNED BY FUNCTION.") ;
        END IF ;
        
    END LOCAL_BLOCK ;
     
    REPORT.RESULT ;
     
END C36204D ;
