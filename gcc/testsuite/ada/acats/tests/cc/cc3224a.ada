-- CC3224A.ADA

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
--     CHECK THAT A FORMAL ARRAY TYPE DENOTES ITS ACTUAL
--     PARAMETER, AND THAT OPERATIONS OF THE FORMAL TYPE ARE THOSE
--     IDENTIFIED WITH THE CORRESPONDING OPERATIONS OF THE ACTUAL TYPE.

-- HISTORY:
--     DHH 09/19/88  CREATED ORIGINAL TEST.
--     EDWARD V. BERARD, 14 AUGUST 1990  ADDED CHECKS FOR MULTI-
--                                       DIMENSIONAL ARRAYS
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH REPORT ;

PROCEDURE CC3224A IS

    SUBTYPE INT IS INTEGER RANGE 1 .. 3;
    TYPE ARR IS ARRAY(1 .. 3) OF INTEGER;
    TYPE B_ARR IS ARRAY(1 .. 3) OF BOOLEAN;

    Q : ARR;
    R : B_ARR;
     
    GENERIC
        TYPE T IS ARRAY(INT) OF INTEGER;
    PACKAGE P IS
        SUBTYPE SUB_T IS T;
        X : SUB_T := (1, 2, 3);
    END P;

    GENERIC
        TYPE T IS ARRAY(INT) OF BOOLEAN;
    PACKAGE BOOL IS
        SUBTYPE SUB_T IS T;
    END BOOL;
     
    SHORT_START : CONSTANT := -100 ;
    SHORT_END   : CONSTANT := 100 ;
    TYPE SHORT_RANGE IS RANGE SHORT_START .. SHORT_END ;
    
    SUBTYPE REALLY_SHORT IS SHORT_RANGE RANGE -9 .. 0 ;
    
    TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                        SEP, OCT, NOV, DEC) ;
                        
    SUBTYPE FIRST_HALF IS MONTH_TYPE RANGE JAN .. JUN ;
    
    TYPE DAY_TYPE IS RANGE 1 .. 31 ;
    TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
    TYPE DATE IS RECORD
      MONTH : MONTH_TYPE ;
      DAY   : DAY_TYPE ;
      YEAR  : YEAR_TYPE ;
    END RECORD ;
    
    TODAY         : DATE := (MONTH => AUG,
                             DAY   => 8,
                             YEAR  => 1990) ;
                            
    FIRST_DATE    : DATE := (DAY   => 6,
                             MONTH => JUN,
                             YEAR  => 1967) ;
                            
    WALL_DATE     : DATE := (MONTH => NOV,
                             DAY   => 9,
                             YEAR  => 1989) ;
                            
    SUBTYPE FIRST_FIVE IS CHARACTER RANGE 'A' .. 'E' ;
                            
    TYPE THREE_DIMENSIONAL IS ARRAY (REALLY_SHORT,
                                     FIRST_HALF,
                                     FIRST_FIVE) OF DATE ;
                                     
    TD_ARRAY        : THREE_DIMENSIONAL ;
    SECOND_TD_ARRAY : THREE_DIMENSIONAL ;
                                     
    GENERIC
    
        TYPE CUBE IS ARRAY (REALLY_SHORT,
                            FIRST_HALF,
                            FIRST_FIVE) OF DATE ;
                            
    PACKAGE TD_ARRAY_PACKAGE IS
    
        SUBTYPE SUB_CUBE IS CUBE ;
        TEST_3D_ARRAY : SUB_CUBE := (THREE_DIMENSIONAL'RANGE =>
                                    (THREE_DIMENSIONAL'RANGE (2) =>
                                    (THREE_DIMENSIONAL'RANGE (3) =>
                                     TODAY))) ;
                                     
    END TD_ARRAY_PACKAGE ;


BEGIN  -- CC3224A

    REPORT.TEST ("CC3224A", "CHECK THAT A FORMAL ARRAY TYPE DENOTES " &
                 "ITS ACTUAL PARAMETER, AND THAT OPERATIONS OF " &
                 "THE FORMAL TYPE ARE THOSE IDENTIFIED WITH THE " &
                 "CORRESPONDING OPERATIONS OF THE ACTUAL TYPE");

    ONE_DIMENSIONAL:
     
    DECLARE

        PACKAGE P1 IS NEW P (ARR);

        TYPE NEW_T IS NEW P1.SUB_T;
        OBJ_NEWT : NEW_T;
          
    BEGIN  -- ONE_DIMENSIONAL
     
        IF NEW_T'FIRST /= ARR'FIRST THEN
            REPORT.FAILED("'FIRST ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF NEW_T'LAST /= ARR'LAST THEN
            REPORT.FAILED("'LAST ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF NEW_T'FIRST(1) /= ARR'FIRST(1) THEN
            REPORT.FAILED("'FIRST(N) ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF NOT (NEW_T'LAST(1) = ARR'LAST(1)) THEN
            REPORT.FAILED("'LAST(N) ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF 2 NOT IN NEW_T'RANGE THEN
            REPORT.FAILED("'RANGE ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF 3 NOT IN NEW_T'RANGE(1) THEN
            REPORT.FAILED("'RANGE(N) ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF NEW_T'LENGTH /= ARR'LENGTH THEN
            REPORT.FAILED("'LENGTH ATTRIBUTE REPORT.FAILED");
        END IF;
          
        IF NEW_T'LENGTH(1) /= ARR'LENGTH(1) THEN
            REPORT.FAILED("'LENGTH(N) ATTRIBUTE REPORT.FAILED");
         END IF;
           
        OBJ_NEWT := (1, 2, 3);
        IF REPORT.IDENT_INT(3) /= OBJ_NEWT(3) THEN
            REPORT.FAILED("ASSIGNMENT REPORT.FAILED");
        END IF;

        IF NEW_T'(1, 2, 3) NOT IN NEW_T THEN
            REPORT.FAILED("QUALIFIED EXPRESSION REPORT.FAILED");
        END IF;

        Q := (1, 2, 3);
        IF NEW_T(Q) /= OBJ_NEWT THEN
            REPORT.FAILED("EXPLICIT CONVERSION REPORT.FAILED");
        END IF;
        
        IF Q(1) /= OBJ_NEWT(1) THEN
            REPORT.FAILED("INDEXING REPORT.FAILED");
        END IF;
        
        IF (1, 2) /= OBJ_NEWT(1 .. 2) THEN
            REPORT.FAILED("SLICE REPORT.FAILED");
        END IF;
        
        IF (1, 2) & OBJ_NEWT(3) /= NEW_T(Q)THEN
            REPORT.FAILED("CATENATION REPORT.FAILED");
        END IF;
        
        IF NOT (P1.X IN ARR) THEN
            REPORT.FAILED ("FORMAL DOES NOT DENOTE ACTUAL");
        END IF;
          
    END ONE_DIMENSIONAL ;

    BOOLEAN_ONE_DIMENSIONAL:
     
    DECLARE

        PACKAGE B1 IS NEW BOOL (B_ARR);

        TYPE NEW_T IS NEW B1.SUB_T;
        OBJ_NEWT : NEW_T;
          
    BEGIN  -- BOOLEAN_ONE_DIMENSIONAL

        OBJ_NEWT := (TRUE, TRUE, TRUE);
        R := (TRUE, TRUE, TRUE);

        IF (NEW_T'((TRUE, TRUE, TRUE)) XOR OBJ_NEWT) /=
           NEW_T'((FALSE, FALSE, FALSE)) THEN
            REPORT.FAILED("XOR REPORT.FAILED - BOOLEAN") ;
        END IF;
        
        IF (NEW_T'((FALSE, FALSE, TRUE)) AND OBJ_NEWT) /=
           NEW_T'((FALSE, FALSE, TRUE)) THEN
            REPORT.FAILED("AND REPORT.FAILED - BOOLEAN") ;
        END IF;
        
        IF (NEW_T'((FALSE, FALSE, FALSE)) OR OBJ_NEWT) /=
           NEW_T'((TRUE, TRUE, TRUE)) THEN
            REPORT.FAILED("OR REPORT.FAILED - BOOLEAN") ;
        END IF ;
          
    END BOOLEAN_ONE_DIMENSIONAL ;
     
    THREE_DIMENSIONAL_TEST:
     
    DECLARE
     
         PACKAGE TD IS NEW TD_ARRAY_PACKAGE (CUBE => THREE_DIMENSIONAL) ;
        
        TYPE NEW_CUBE IS NEW TD.SUB_CUBE ;
        NEW_CUBE_OBJECT : NEW_CUBE ;
        
    BEGIN  -- THREE_DIMENSIONAL_TEST
    
        IF (NEW_CUBE'FIRST /= THREE_DIMENSIONAL'FIRST) OR
           (NEW_CUBE'FIRST (1) /= THREE_DIMENSIONAL'FIRST) OR
           (NEW_CUBE'FIRST (2) /= THREE_DIMENSIONAL'FIRST (2)) OR
           (NEW_CUBE'FIRST (3) /= THREE_DIMENSIONAL'FIRST (3)) THEN
            REPORT.FAILED ("PROBLEMS WITH 'FIRST FOR MULTI-" &
                           "DIMENSIONAL ARRAYS.") ;
        END IF ;
        
        IF (NEW_CUBE'LAST /= THREE_DIMENSIONAL'LAST) OR
           (NEW_CUBE'LAST (1) /= THREE_DIMENSIONAL'LAST) OR
           (NEW_CUBE'LAST (2) /= THREE_DIMENSIONAL'LAST (2)) OR
           (NEW_CUBE'LAST (3) /= THREE_DIMENSIONAL'LAST (3)) THEN
            REPORT.FAILED ("PROBLEMS WITH 'LAST FOR MULTI-" &
                           "DIMENSIONAL ARRAYS.") ;
        END IF ;
        
        IF (-5 NOT IN NEW_CUBE'RANGE) OR
           (-3 NOT IN NEW_CUBE'RANGE (1)) OR
           (FEB NOT IN NEW_CUBE'RANGE (2)) OR
           ('C' NOT IN NEW_CUBE'RANGE (3)) THEN
            REPORT.FAILED ("PROBLEMS WITH 'RANGE FOR MULTI-" &
                           "DIMENSIONAL ARRAYS.") ;
        END IF ;
        
        IF (NEW_CUBE'LENGTH /= THREE_DIMENSIONAL'LENGTH) OR
           (NEW_CUBE'LENGTH (1) /= THREE_DIMENSIONAL'LENGTH) OR
           (NEW_CUBE'LENGTH (2) /= THREE_DIMENSIONAL'LENGTH (2)) OR
           (NEW_CUBE'LENGTH (3) /= THREE_DIMENSIONAL'LENGTH (3)) THEN
            REPORT.FAILED ("PROBLEMS WITH 'LENGTH FOR MULTI-" &
                           "DIMENSIONAL ARRAYS.") ;
        END IF ;
        
        NEW_CUBE_OBJECT := (NEW_CUBE'RANGE =>
                           (NEW_CUBE'RANGE (2) =>
                           (NEW_CUBE'RANGE (3) =>
                            FIRST_DATE))) ;
        IF FIRST_DATE /= NEW_CUBE_OBJECT (-3, MAR, 'D') THEN
            REPORT.FAILED ("ASSIGNMENT FOR MULTI-DIMENSIONAL " &
                           "ARRAYS FAILED.") ;
        END IF ;
        
        IF NEW_CUBE'(NEW_CUBE'RANGE =>
                    (NEW_CUBE'RANGE (2) =>
                    (NEW_CUBE'RANGE (3) =>
                     WALL_DATE))) NOT IN NEW_CUBE THEN
            REPORT.FAILED ("QUALIFIED EXPRESSION FOR MULTI-" &
                           "DIMENSIONAL ARRAYS FAILED.") ;
        END IF ;
        
        SECOND_TD_ARRAY := (NEW_CUBE'RANGE =>
                           (NEW_CUBE'RANGE (2) =>
                           (NEW_CUBE'RANGE (3) =>
                            FIRST_DATE))) ;
        IF NEW_CUBE (SECOND_TD_ARRAY) /= NEW_CUBE_OBJECT THEN
            REPORT.FAILED ("EXPLICIT CONVERSION FOR MULTI-" &
                           "DIMENSIONAL ARRAYS FAILED.") ;
        END IF ;
        
        IF SECOND_TD_ARRAY (-2, FEB, 'B')
            /= NEW_CUBE_OBJECT (-2, FEB, 'B') THEN
            REPORT.FAILED ("INDEXING FOR MULTI-" &
                           "DIMENSIONAL ARRAYS FAILED.") ;
        END IF ;
        
        IF NOT (TD.TEST_3D_ARRAY IN THREE_DIMENSIONAL) THEN
            REPORT.FAILED ("FORMAL MULTI-DIMENSIONAL ARRAY " &
                           "DOES NOT DENOTE ACTUAL.") ;
        END IF ;
        
    END THREE_DIMENSIONAL_TEST ;
    
    REPORT.RESULT ;
     
END CC3224A ;
