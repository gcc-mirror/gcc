-- CC3016C.ADA

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
--  CHECK THAT AN INSTANCE OF A GENERIC PACKAGE MUST DECLARE A
--  PACKAGE. CHECK THAT THE STATEMENTS IN AN INSTANTIATED GENERIC
--  PACKAGE BODY ARE EXECUTED AFTER THE ELABORATION OF THE
--  DECLARATIONS (IN SPEC AND IN BODY).

-- HISTORY:
--         EDWARD V. BERARD, 8 AUGUST 1990

WITH REPORT;

PROCEDURE  CC3016C  IS

    GENERIC
    
        TYPE SOME_TYPE IS PRIVATE ;
        FIRST_INITIAL_VALUE  : IN SOME_TYPE ;
        SECOND_INITIAL_VALUE : IN SOME_TYPE ;
        WITH PROCEDURE CHANGE (FIRST  : IN SOME_TYPE ;
                               RESULT : OUT SOME_TYPE) ;
        WITH PROCEDURE SECOND_CHANGE (FIRST  : IN SOME_TYPE ;
                                      RESULT : OUT SOME_TYPE) ;
        WITH PROCEDURE THIRD_CHANGE (FIRST  : IN SOME_TYPE ;
                                     RESULT : OUT SOME_TYPE) ;
        FIRST_EXPECTED_RESULT     : IN SOME_TYPE ;
        SECOND_EXPECTED_RESULT    : IN SOME_TYPE ;
        THIRD_EXPECTED_RESULT     : IN SOME_TYPE ;
        FOURTH_EXPECTED_RESULT    : IN SOME_TYPE ;
        FIFTH_EXPECTED_RESULT     : IN SOME_TYPE ;
        SIXTH_EXPECTED_RESULT     : IN SOME_TYPE ;
    
    PACKAGE OUTER IS

        VARIABLE : SOME_TYPE := FIRST_INITIAL_VALUE ;
        
        FUNCTION INNER_VARIABLE RETURN SOME_TYPE ;

        GENERIC
        
            INITIAL_VALUE : IN SOME_TYPE ;
            WITH PROCEDURE CHANGE (FIRST  : IN SOME_TYPE ;
                                   RESULT : OUT SOME_TYPE) ;
            WITH PROCEDURE SECOND_CHANGE (FIRST  : IN SOME_TYPE ;
                                          RESULT : OUT SOME_TYPE) ;
            FIRST_EXPECTED_RESULT     : IN SOME_TYPE ;
            SECOND_EXPECTED_RESULT    : IN SOME_TYPE ;
            THIRD_EXPECTED_RESULT     : IN SOME_TYPE ;
            FOURTH_EXPECTED_RESULT    : IN SOME_TYPE ;
                    
        PACKAGE INNER  IS
            VARIABLE : SOME_TYPE := INITIAL_VALUE ;
        END INNER ;
        
    END OUTER ;


    PACKAGE BODY OUTER IS

        ANOTHER_VARIABLE : SOME_TYPE := FIRST_INITIAL_VALUE ;
        
        PACKAGE BODY  INNER  IS
            ANOTHER_VARIABLE : SOME_TYPE := INITIAL_VALUE ;
        BEGIN  -- INNER

            CHANGE (FIRST  => VARIABLE,
                    RESULT => VARIABLE) ;
            CHANGE (FIRST  => ANOTHER_VARIABLE,
                    RESULT => ANOTHER_VARIABLE) ;
            OUTER.SECOND_CHANGE (FIRST  => OUTER.VARIABLE,
                                 RESULT => OUTER.VARIABLE) ;
            OUTER.CHANGE (FIRST  => OUTER.ANOTHER_VARIABLE,
                          RESULT => OUTER.ANOTHER_VARIABLE) ;

            IF (VARIABLE /= FIRST_EXPECTED_RESULT) OR
               (ANOTHER_VARIABLE /= SECOND_EXPECTED_RESULT) OR
               (OUTER.VARIABLE 
                       /= THIRD_EXPECTED_RESULT) OR
               (OUTER.ANOTHER_VARIABLE 
                       /= FOURTH_EXPECTED_RESULT) THEN
                    REPORT.FAILED("ASSIGNED VALUES INCORRECT - BODY OF INNER") ;
            END IF;

        END INNER ;

        PACKAGE NEW_INNER IS NEW INNER 
            (INITIAL_VALUE          => SECOND_INITIAL_VALUE,
             CHANGE                 => CHANGE,
             SECOND_CHANGE          => THIRD_CHANGE,
             FIRST_EXPECTED_RESULT  => FIRST_EXPECTED_RESULT,
             SECOND_EXPECTED_RESULT => SECOND_EXPECTED_RESULT,
             THIRD_EXPECTED_RESULT  => THIRD_EXPECTED_RESULT,
             FOURTH_EXPECTED_RESULT => FOURTH_EXPECTED_RESULT) ;
             
        FUNCTION INNER_VARIABLE RETURN SOME_TYPE IS
        BEGIN
            RETURN NEW_INNER.VARIABLE ;            
        END INNER_VARIABLE ;

    BEGIN  -- OUTER
    
        SECOND_CHANGE (FIRST  => VARIABLE,
                       RESULT => VARIABLE) ;
        SECOND_CHANGE (FIRST  => ANOTHER_VARIABLE,
                       RESULT => ANOTHER_VARIABLE) ;
                       
        IF (VARIABLE /= FIFTH_EXPECTED_RESULT) OR
           (ANOTHER_VARIABLE /= SIXTH_EXPECTED_RESULT) OR
           (NEW_INNER.VARIABLE /= FIRST_EXPECTED_RESULT) THEN
            REPORT.FAILED("ASSIGNED VALUES INCORRECT - BODY OF OUTER") ;
        END IF;

    END OUTER ;
    
    PROCEDURE DOUBLE (THIS_VALUE          : IN  INTEGER;
                      GIVING_THIS_RESULT  : OUT INTEGER) IS
    BEGIN -- DOUBLE
        GIVING_THIS_RESULT := 2 * THIS_VALUE ;
    END DOUBLE ;
    
    PROCEDURE ADD_20 (TO_THIS_VALUE      : IN  INTEGER;
                      GIVING_THIS_RESULT : OUT INTEGER) IS
    BEGIN -- ADD_20
        GIVING_THIS_RESULT := TO_THIS_VALUE + 20 ;
    END ADD_20 ;
    
    PROCEDURE TIMES_FIVE (THIS_VALUE          : IN  INTEGER;
                          GIVING_THIS_RESULT  : OUT INTEGER) IS
    BEGIN -- TIMES_FIVE
        GIVING_THIS_RESULT := 5 * THIS_VALUE ;
    END TIMES_FIVE ;    
    
BEGIN -- CC3016C

    REPORT.TEST ("CC3016C" , "CHECK THAT AN INSTANCE OF A GENERIC PACKAGE " &
                 "MUST DECLARE A PACKAGE. CHECK THAT THE STATEMENTS IN AN " &
                 "INSTANTIATED GENERIC PACKAGE BODY ARE EXECUTED AFTER THE " &
                 "ELABORATION OF THE DECLARATIONS (IN SPEC AND IN BODY).") ;
                   
    LOCAL_BLOCK:
    
    DECLARE
    
        PACKAGE NEW_OUTER IS NEW OUTER
            (SOME_TYPE                 => INTEGER,
            FIRST_INITIAL_VALUE        => 7,
            SECOND_INITIAL_VALUE       => 11,
            CHANGE                     => DOUBLE,
            SECOND_CHANGE              => ADD_20,
            THIRD_CHANGE               => TIMES_FIVE,
            FIRST_EXPECTED_RESULT      => 22, 
            SECOND_EXPECTED_RESULT     => 22,
            THIRD_EXPECTED_RESULT      => 27,
            FOURTH_EXPECTED_RESULT     => 14,
            FIFTH_EXPECTED_RESULT      => 47,
            SIXTH_EXPECTED_RESULT      => 34) ;

    BEGIN  -- LOCAL_BLOCK    
    
        IF (NEW_OUTER.VARIABLE /= 47) OR
           (NEW_OUTER.INNER_VARIABLE /= 22) THEN
            REPORT.FAILED("ASSIGNED VALUES INCORRECT - " &
                          "BODY OF MAIN PROGRAM") ;
        END IF;
        
    END LOCAL_BLOCK ;

    REPORT.RESULT;

END CC3016C;
