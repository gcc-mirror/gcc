-- CC3019B2M.ADA

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
--  CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC UNITS, E.G.,
--  TO SUPPORT ITERATORS. THIS TEST SPECIFICALLY CHECKS THAT A
--  NESTING LEVEL OF 2 IS SUPPORTED FOR GENERICS.
--
--  *** THIS IS THE MAIN PROGRAM. IT MUST BE COMPILED AFTER THE
--  *** SOURCE CODE IN FILES CC3019B0.ADA AND CC3019B1.ADA HAVE
--  *** BEEN COMPILED.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

WITH REPORT ;
WITH CC3019B1_STACK_CLASS ;

PROCEDURE CC3019B2M IS

     TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                         SEP, OCT, NOV, DEC) ;
     TYPE DAY_TYPE IS RANGE 1 .. 31 ;
     TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
     TYPE DATE IS RECORD
          MONTH : MONTH_TYPE ;
          DAY   : DAY_TYPE ;
          YEAR  : YEAR_TYPE ;
     END RECORD ;
     
     STORE_DATE     : DATE ;

     TODAY        : DATE := (MONTH => AUG,
                             DAY   => 31,
                             YEAR  => 1990) ;
                                    
     FIRST_DATE   : DATE := (MONTH => JUN,
                             DAY   => 4,
                             YEAR  => 1967) ;
                                    
     BIRTH_DATE   : DATE := (MONTH => OCT,
                             DAY   => 3,
                             YEAR  => 1949) ;
                                    
     WALL_DATE    : DATE := (MONTH => NOV,
                             DAY   => 9,
                             YEAR  => 1989) ;
                                    
     PROCEDURE ASSIGN (THE_VALUE_OF_THIS_DATE    : IN OUT DATE ;
                       TO_THIS_DATE              : IN OUT DATE) ;
                              
     FUNCTION IS_EQUAL (LEFT  : IN DATE ;
                        RIGHT : IN DATE) RETURN BOOLEAN ;

     PACKAGE DATE_STACK IS
          NEW CC3019B1_STACK_CLASS (ELEMENT => DATE,
                                    ASSIGN  => ASSIGN,
                                    "="     => IS_EQUAL) ;
                                                   
     FIRST_DATE_STACK    : DATE_STACK.STACK ;
     SECOND_DATE_STACK   : DATE_STACK.STACK ;
     THIRD_DATE_STACK    : DATE_STACK.STACK ;
     
     FUNCTION "=" (LEFT  : IN DATE_STACK.STACK ;
                   RIGHT : IN DATE_STACK.STACK) RETURN BOOLEAN
                   RENAMES DATE_STACK."=" ;
                              
     PROCEDURE ASSIGN (THE_VALUE_OF_THIS_DATE    : IN OUT DATE ;
                       TO_THIS_DATE              : IN OUT DATE) IS
                              
     BEGIN -- ASSIGN
          
          TO_THIS_DATE := THE_VALUE_OF_THIS_DATE ;
                
     END ASSIGN ;
                                            
     FUNCTION IS_EQUAL (LEFT  : IN DATE ;
                        RIGHT : IN DATE) RETURN BOOLEAN IS
                         
     BEGIN -- IS_EQUAL
     
          RETURN (LEFT.MONTH = RIGHT.MONTH) AND
                 (LEFT.DAY = RIGHT.DAY) AND
                 (LEFT.YEAR = RIGHT.YEAR) ;
          
     END IS_EQUAL ;

BEGIN  -- CC3019B2M

     REPORT.TEST ("CC3019B2M",
                  "CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC " &
                  "UNITS, E.G., TO SUPPORT ITERATORS. THIS TEST " &
                  "SPECIFICALLY CHECKS THAT A NESTING LEVEL OF " &
                  "2 IS SUPPORTED FOR GENERICS.") ;
                        
     DATE_STACK.CLEAR (THIS_STACK => FIRST_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => FIRST_DATE_STACK) /= 0 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 1") ;
     END IF ;
     
     DATE_STACK.PUSH (THIS_ELEMENT     => TODAY,
                      ON_TO_THIS_STACK => FIRST_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => FIRST_DATE_STACK) /= 1 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 2") ;
     END IF ;
     
     DATE_STACK.PUSH (THIS_ELEMENT     => FIRST_DATE,
                      ON_TO_THIS_STACK => FIRST_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => FIRST_DATE_STACK) /= 2 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 3") ;
     END IF ;
     
     DATE_STACK.PUSH (THIS_ELEMENT     => BIRTH_DATE,
                      ON_TO_THIS_STACK => FIRST_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => FIRST_DATE_STACK) /= 3 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 4") ;
     END IF ;
     
     DATE_STACK.POP (THIS_ELEMENT   => STORE_DATE,
                           OFF_THIS_STACK => FIRST_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => FIRST_DATE_STACK) /= 2 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 5") ;
     END IF ;
     
     IF STORE_DATE /= BIRTH_DATE THEN
          REPORT.FAILED (
               "IMPROPER VALUE REMOVED FROM STACK - 1") ;
     END IF ;
     
     DATE_STACK.CLEAR (THIS_STACK => SECOND_DATE_STACK) ;
     IF DATE_STACK.NUMBER_OF_ELEMENTS 
        (ON_THIS_STACK => SECOND_DATE_STACK) /= 0 THEN
          REPORT.FAILED (
               "IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 6") ;
     END IF ;
          
     DATE_STACK.COPY (THIS_STACK    => FIRST_DATE_STACK,
                      TO_THIS_STACK => SECOND_DATE_STACK) ;
                             
     IF FIRST_DATE_STACK /= SECOND_DATE_STACK THEN
          REPORT.FAILED (
               "PROBLEMS WITH COPY OR TEST FOR EQUALITY") ;
     END IF ;
     
     DATE_STACK.POP (THIS_ELEMENT   => STORE_DATE,
                     OFF_THIS_STACK => SECOND_DATE_STACK) ;
     DATE_STACK.PUSH (THIS_ELEMENT     => WALL_DATE,
                      ON_TO_THIS_STACK => SECOND_DATE_STACK) ;
     IF FIRST_DATE_STACK = SECOND_DATE_STACK THEN
          REPORT.FAILED (
               "PROBLEMS WITH POP OR TEST FOR EQUALITY") ;
     END IF ;
     
     UNDERFLOW_EXCEPTION_TEST:
     
     BEGIN  -- UNDERFLOW_EXCEPTION_TEST
     
          DATE_STACK.CLEAR (THIS_STACK => THIRD_DATE_STACK) ;
          DATE_STACK.POP (THIS_ELEMENT      => STORE_DATE,
                          OFF_THIS_STACK    => THIRD_DATE_STACK) ;
          REPORT.FAILED ("UNDERFLOW EXCEPTION NOT RAISED") ;
     
     EXCEPTION
     
          WHEN DATE_STACK.UNDERFLOW => NULL ;  -- CORRECT EXCEPTION
                                               -- RAISED
          WHEN OTHERS =>
               REPORT.FAILED ("INCORRECT EXCEPTION RAISED IN " &
                              "UNDERFLOW EXCEPTION TEST") ;
                                              
     END UNDERFLOW_EXCEPTION_TEST ;
     
     OVERFLOW_EXCEPTION_TEST:
     
     BEGIN  -- OVERFLOW_EXCEPTION_TEST
     
          DATE_STACK.CLEAR (THIS_STACK => THIRD_DATE_STACK) ;
          FOR INDEX IN 1 .. 10 LOOP
               DATE_STACK.PUSH ( THIS_ELEMENT     => TODAY,
                                 ON_TO_THIS_STACK => THIRD_DATE_STACK) ;
          END LOOP ;
          
          DATE_STACK.PUSH (THIS_ELEMENT     => TODAY,
                           ON_TO_THIS_STACK => THIRD_DATE_STACK) ;
          REPORT.FAILED ("OVERFLOW EXCEPTION NOT RAISED") ;
     
     EXCEPTION
     
          WHEN DATE_STACK.OVERFLOW => NULL ;  -- CORRECT EXCEPTION
                                              -- RAISED
          WHEN OTHERS =>
               REPORT.FAILED ("INCORRECT EXCEPTION RAISED IN " &
                              "OVERFLOW EXCEPTION TEST") ;
                                              
     END OVERFLOW_EXCEPTION_TEST ;

     LOCAL_BLOCK:
     
     DECLARE
     
          TYPE DATE_TABLE IS ARRAY (POSITIVE RANGE 1 .. 10) OF DATE ;
     
          FIRST_DATE_TABLE : DATE_TABLE ;
     
          TABLE_INDEX : POSITIVE := 1 ;
     
          PROCEDURE SHOW_DATES (THIS_DATE : IN  DATE ;
                                CONTINUE  : OUT BOOLEAN) ;
                                      
          PROCEDURE STORE_DATES (THIS_DATE : IN DATE ;
                                 CONTINUE  : OUT BOOLEAN) ;
                                      
          PROCEDURE SHOW_DATE_ITERATE IS NEW 
               DATE_STACK.ITERATE (PROCESS => SHOW_DATES) ;
                                
          PROCEDURE STORE_DATE_ITERATE IS NEW 
               DATE_STACK.ITERATE (PROCESS => STORE_DATES) ;
                                
          PROCEDURE SHOW_DATES (THIS_DATE : IN  DATE ;
                                CONTINUE  : OUT BOOLEAN) IS
          BEGIN  -- SHOW_DATES
          
                REPORT.COMMENT ("THE MONTH IS " &
                           MONTH_TYPE'IMAGE (THIS_DATE.MONTH)) ;
                REPORT.COMMENT ("THE DAY IS " &
                           DAY_TYPE'IMAGE (THIS_DATE.DAY)) ;
                REPORT.COMMENT ("THE YEAR IS " &
                           YEAR_TYPE'IMAGE (THIS_DATE.YEAR)) ;
                
                CONTINUE := TRUE ;
                           
          END SHOW_DATES ;
          
          PROCEDURE STORE_DATES (THIS_DATE : IN  DATE ;
                                       CONTINUE  : OUT BOOLEAN) IS
          BEGIN  -- STORE_DATES
          
                FIRST_DATE_TABLE (TABLE_INDEX) := THIS_DATE ;
                TABLE_INDEX := TABLE_INDEX + 1 ;
                
                CONTINUE := TRUE ;
                
          END STORE_DATES ;
          
     BEGIN  -- LOCAL_BLOCK
     
          REPORT.COMMENT ("CONTENTS OF THE FIRST STACK") ;
          SHOW_DATE_ITERATE (OVER_THIS_STACK => FIRST_DATE_STACK) ;

          REPORT.COMMENT ("CONTENTS OF THE SECOND STACK") ;
          SHOW_DATE_ITERATE (OVER_THIS_STACK => SECOND_DATE_STACK) ;

          STORE_DATE_ITERATE (OVER_THIS_STACK => FIRST_DATE_STACK) ;
          IF (FIRST_DATE_TABLE (1) /= TODAY) OR
               (FIRST_DATE_TABLE (2) /= FIRST_DATE) THEN
                     REPORT.FAILED ("PROBLEMS WITH ITERATION - 1") ;
          END IF ;
          
          TABLE_INDEX := 1 ;
          STORE_DATE_ITERATE (OVER_THIS_STACK => SECOND_DATE_STACK) ;
          IF (FIRST_DATE_TABLE (1) /= TODAY) OR
               (FIRST_DATE_TABLE (2) /= WALL_DATE) THEN
                     REPORT.FAILED ("PROBLEMS WITH ITERATION - 2") ;
          END IF ;
          
     END LOCAL_BLOCK ;
     
     REPORT.RESULT ;
     
END CC3019B2M ;
