-- CC1311A.ADA

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
--     CHECK THAT THE DEFAULT EXPRESSIONS OF THE PARAMETERS OF A FORMAL
--     SUBPROGRAM ARE USED INSTEAD OF THE DEFAULTS (IF ANY) OF THE
--     ACTUAL SUBPROGRAM PARAMETER.

-- HISTORY:
--     RJW 06/05/86  CREATED ORIGINAL TEST.
--     VCL 08/18/87  CHANGED A COUPLE OF STATIC DEFAULT EXPRESSIONS FOR
--                   FORMAL SUBPROGRAM PARAMETERS TO DYNAMIC 
--                   EXPRESSIONS VIA THE USE OF THE IDENTITY FUNCTION.
--     EDWARD V. BERARD 08/13/90  
--                   ADDED CHECKS FOR MULTI-DIMENSIONAL ARRAYS.

WITH REPORT ;

PROCEDURE CC1311A IS

     TYPE NUMBERS IS (ZERO, ONE ,TWO);

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
                            
     FIRST_DATE     : DATE := (DAY   => 6,
                               MONTH => JUN,
                               YEAR  => 1967) ;
     
     SUBTYPE FIRST_FIVE IS CHARACTER RANGE 'A' .. 'E' ;
                            
     TYPE THREE_DIMENSIONAL IS ARRAY (REALLY_SHORT,
                                      FIRST_HALF,
                                      FIRST_FIVE) OF DATE ;
                            
     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH FUNCTION FUN (FIRST : IN CUBE := (CUBE'RANGE =>
                                                (CUBE'RANGE (2) =>
                                                (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))))
                        RETURN CUBE ;
                        
     PROCEDURE PROC_WITH_3D_FUNC ;
    
     PROCEDURE PROC_WITH_3D_FUNC IS
    
     BEGIN  -- PROC_WITH_3D_FUNC
    
          IF FUN /= CUBE'(CUBE'RANGE =>
                         (CUBE'RANGE (2) =>
                         (CUBE'RANGE (3) => DEFAULT_VALUE))) THEN
               REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL " &
                              "ARRAY, FUNCTION, AND PROCEDURE.") ;
          END IF ;
        
     END PROC_WITH_3D_FUNC ;
    
     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH FUNCTION FUN (FIRST : IN CUBE := (CUBE'RANGE =>
                                                (CUBE'RANGE (2) =>
                                                (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))))
                        RETURN CUBE ;
                        
     PACKAGE PKG_WITH_3D_FUNC IS
     END PKG_WITH_3D_FUNC ;
    
     PACKAGE BODY PKG_WITH_3D_FUNC IS
     BEGIN  -- PKG_WITH_3D_FUNC

          REPORT.TEST("CC1311A","CHECK THAT THE DEFAULT EXPRESSIONS " &
                      "OF THE PARAMETERS OF A FORMAL SUBPROGRAM ARE " &
                      "USED INSTEAD OF THE DEFAULTS (IF ANY) OF THE " &
                      "ACTUAL SUBPROGRAM PARAMETER" ) ;
    
          IF FUN /= CUBE'(CUBE'RANGE =>
                         (CUBE'RANGE (2) =>
                         (CUBE'RANGE (3) => DEFAULT_VALUE))) THEN
               REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL " &
                              "ARRAY, FUNCTION, AND PACKAGE.") ;
          END IF ;
        
     END PKG_WITH_3D_FUNC ;
    
     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH FUNCTION FUN (FIRST : IN CUBE := (CUBE'RANGE =>
                                                (CUBE'RANGE (2) =>
                                                (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))))
                        RETURN CUBE ;
                        
     FUNCTION FUNC_WITH_3D_FUNC RETURN BOOLEAN ;
    
     FUNCTION FUNC_WITH_3D_FUNC RETURN BOOLEAN IS
     BEGIN  -- FUNC_WITH_3D_FUNC
     
          RETURN FUN = CUBE'(CUBE'RANGE =>
                            (CUBE'RANGE (2) =>
                            (CUBE'RANGE (3) => DEFAULT_VALUE))) ;
                       
     END FUNC_WITH_3D_FUNC ;
    
     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH PROCEDURE PROC (INPUT  : IN  CUBE := (CUBE'RANGE =>
                                                    (CUBE'RANGE (2) =>
                                                    (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))) ;
                               OUTPUT : OUT CUBE) ;
                        
     PROCEDURE PROC_WITH_3D_PROC ;
    
     PROCEDURE PROC_WITH_3D_PROC IS
    
          RESULTS : CUBE ;
        
     BEGIN  -- PROC_WITH_3D_PROC
    
          PROC (OUTPUT => RESULTS) ;
        
          IF RESULTS /= CUBE'(CUBE'RANGE =>
                             (CUBE'RANGE (2) =>
                             (CUBE'RANGE (3) => DEFAULT_VALUE))) THEN
               REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL " &
                              "ARRAY, PROCEDURE, AND PROCEDURE.") ;
          END IF ;
        
     END PROC_WITH_3D_PROC ;

     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH PROCEDURE PROC (INPUT  : IN  CUBE := (CUBE'RANGE =>
                                                    (CUBE'RANGE (2) =>
                                                    (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))) ;
                               OUTPUT : OUT CUBE) ;
                        
     PACKAGE PKG_WITH_3D_PROC IS
     END PKG_WITH_3D_PROC ;
    
     PACKAGE BODY PKG_WITH_3D_PROC IS
    
          RESULTS : CUBE ;
        
     BEGIN  -- PKG_WITH_3D_PROC
    
          PROC (OUTPUT => RESULTS) ;
        
          IF RESULTS /= CUBE'(CUBE'RANGE =>
                             (CUBE'RANGE (2) =>
                             (CUBE'RANGE (3) => DEFAULT_VALUE))) THEN
               REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL " &
                              "ARRAY, PROCEDURE, AND PACKAGE.") ;
          END IF ;
        
     END PKG_WITH_3D_PROC ;
    
     GENERIC
    
          TYPE FIRST_INDEX IS (<>) ;
          TYPE SECOND_INDEX IS (<>) ;
          TYPE THIRD_INDEX IS (<>) ;
          TYPE COMPONENT_TYPE IS PRIVATE ;
          DEFAULT_VALUE : IN COMPONENT_TYPE ;
          TYPE CUBE IS ARRAY (FIRST_INDEX, 
                              SECOND_INDEX,
                              THIRD_INDEX) OF COMPONENT_TYPE ;
          WITH PROCEDURE PROC (INPUT  : IN  CUBE := (CUBE'RANGE =>
                                                    (CUBE'RANGE (2) =>
                                                    (CUBE'RANGE (3) =>
                                                     DEFAULT_VALUE))) ;
                               OUTPUT : OUT CUBE) ;
                        
     FUNCTION FUNC_WITH_3D_PROC RETURN BOOLEAN ;
    
     FUNCTION FUNC_WITH_3D_PROC RETURN BOOLEAN IS
    
          RESULTS : CUBE ;
        
     BEGIN  -- FUNC_WITH_3D_PROC
    
          PROC (OUTPUT => RESULTS) ;
          RETURN RESULTS = CUBE'(CUBE'RANGE =>
                                (CUBE'RANGE (2) =>
                                (CUBE'RANGE (3) => DEFAULT_VALUE))) ;
                         
     END FUNC_WITH_3D_PROC ;
    
     GENERIC
          TYPE T IS (<>);
          WITH FUNCTION F (X : T := T'VAL (0)) RETURN T;
     FUNCTION FUNC1 RETURN BOOLEAN;

     FUNCTION FUNC1 RETURN BOOLEAN IS
     BEGIN  -- FUNC1
         RETURN F = T'VAL (0);
     END FUNC1;

     GENERIC
          TYPE T IS (<>);
          WITH FUNCTION F (X : T := T'VAL (REPORT.IDENT_INT(0)))
                        RETURN T;
     PACKAGE PKG1 IS END PKG1;

     PACKAGE BODY PKG1 IS
     BEGIN  -- PKG1
          IF F /= T'VAL (0) THEN
               REPORT.FAILED ("INCORRECT DEFAULT VALUE WITH " &
                              "FUNCTION 'F' AND PACKAGE 'PKG1'" );
          END IF;
     END PKG1;
     GENERIC
          TYPE T IS (<>);
          WITH FUNCTION F (X : T := T'VAL (0)) RETURN T;
     PROCEDURE PROC1;

     PROCEDURE PROC1 IS
     BEGIN  -- PROC1
          IF F /= T'VAL (0) THEN
               REPORT.FAILED ("INCORRECT DEFAULT VALUE WITH " &
                              "FUNCTION 'F' AND PROCEDURE 'PROC1'" );
          END IF;
     END PROC1;

     GENERIC
          TYPE T IS (<>);
          WITH PROCEDURE P (RESULTS : OUT T ; 
                            X       : T := T'VAL (0)) ;
     FUNCTION FUNC2 RETURN BOOLEAN;

     FUNCTION FUNC2 RETURN BOOLEAN IS
          RESULTS : T;
     BEGIN  -- FUNC2
          P (RESULTS);
          RETURN RESULTS = T'VAL (0);
     END FUNC2;

     GENERIC
          TYPE T IS (<>);
          WITH PROCEDURE P (RESULTS : OUT T;
                            X       : T := T'VAL(REPORT.IDENT_INT(0)));
     PACKAGE PKG2 IS END PKG2 ;

     PACKAGE BODY PKG2 IS
          RESULTS : T;
     BEGIN  -- PKG2
          P (RESULTS);
          IF RESULTS /= T'VAL (0) THEN
                REPORT.FAILED ("INCORRECT DEFAULT VALUE WITH " &
                               "PROCEDURE 'P' AND PACKAGE 'PKG2'" );
          END IF;
     END PKG2;

     GENERIC
          TYPE T IS (<>);
          WITH PROCEDURE P (RESULTS :OUT T; X : T := T'VAL (0));
     PROCEDURE PROC2;

     PROCEDURE PROC2 IS
          RESULTS : T;
     BEGIN  -- PROC2
          P (RESULTS);
          IF RESULTS /= T'VAL (0) THEN
               REPORT.FAILED ("INCORRECT DEFAULT VALUE WITH " &
                             "PROCEDURE 'P' AND PROCEDURE 'PROC2'" );
          END IF;
     END PROC2;

     FUNCTION F1 (A : NUMBERS := ONE) RETURN NUMBERS IS
     BEGIN  -- F1
          RETURN A;
     END;

     PROCEDURE P2 (OUTVAR : OUT NUMBERS; INVAR : NUMBERS := TWO) IS
     BEGIN  -- P2
          OUTVAR := INVAR;
     END;

     FUNCTION TD_FUNC (FIRST : IN THREE_DIMENSIONAL :=
                                       (THREE_DIMENSIONAL'RANGE =>
                                       (THREE_DIMENSIONAL'RANGE (2) =>
                                       (THREE_DIMENSIONAL'RANGE (3) =>
                                            FIRST_DATE))))
              RETURN THREE_DIMENSIONAL IS 
                
     BEGIN  -- TD_FUNC
    
          RETURN FIRST ;
        
     END TD_FUNC ;
    
     PROCEDURE TD_PROC (INPUT  : IN  THREE_DIMENSIONAL := 
                                        (THREE_DIMENSIONAL'RANGE =>
                                        (THREE_DIMENSIONAL'RANGE (2) =>
                                        (THREE_DIMENSIONAL'RANGE (3) =>
                                             FIRST_DATE))) ;
                        OUTPUT : OUT THREE_DIMENSIONAL) IS
     BEGIN  -- TD_PROC
    
          OUTPUT := INPUT ;
        
     END TD_PROC ;
    
     PROCEDURE NEW_PROC_WITH_3D_FUNC IS NEW 
          PROC_WITH_3D_FUNC (FIRST_INDEX    => REALLY_SHORT,
                             SECOND_INDEX   => FIRST_HALF,
                             THIRD_INDEX    => FIRST_FIVE,
                             COMPONENT_TYPE => DATE,
                             DEFAULT_VALUE  => TODAY,
                             CUBE           => THREE_DIMENSIONAL,
                             FUN            => TD_FUNC) ;

     PACKAGE NEW_PKG_WITH_3D_FUNC IS NEW 
          PKG_WITH_3D_FUNC (FIRST_INDEX     => REALLY_SHORT,
                            SECOND_INDEX    => FIRST_HALF,
                            THIRD_INDEX     => FIRST_FIVE,
                            COMPONENT_TYPE  => DATE,
                            DEFAULT_VALUE   => TODAY,
                            CUBE            => THREE_DIMENSIONAL,
                            FUN             => TD_FUNC) ;

      FUNCTION NEW_FUNC_WITH_3D_FUNC IS NEW 
          FUNC_WITH_3D_FUNC (FIRST_INDEX    => REALLY_SHORT,
                             SECOND_INDEX   => FIRST_HALF,
                             THIRD_INDEX    => FIRST_FIVE,
                             COMPONENT_TYPE => DATE,
                             DEFAULT_VALUE  => TODAY,
                             CUBE           => THREE_DIMENSIONAL,
                             FUN            => TD_FUNC) ;

     PROCEDURE NEW_PROC_WITH_3D_PROC IS NEW 
          PROC_WITH_3D_PROC (FIRST_INDEX    => REALLY_SHORT,
                             SECOND_INDEX   => FIRST_HALF,
                             THIRD_INDEX    => FIRST_FIVE,
                             COMPONENT_TYPE => DATE,
                             DEFAULT_VALUE  => TODAY,
                             CUBE           => THREE_DIMENSIONAL,
                             PROC           => TD_PROC) ;

     PACKAGE NEW_PKG_WITH_3D_PROC IS NEW 
          PKG_WITH_3D_PROC (FIRST_INDEX     => REALLY_SHORT,
                            SECOND_INDEX   => FIRST_HALF,
                            THIRD_INDEX    => FIRST_FIVE,
                            COMPONENT_TYPE => DATE,
                            DEFAULT_VALUE  => TODAY,
                            CUBE           => THREE_DIMENSIONAL,
                            PROC           => TD_PROC) ;

     FUNCTION NEW_FUNC_WITH_3D_PROC IS NEW 
          FUNC_WITH_3D_PROC (FIRST_INDEX    => REALLY_SHORT,
                             SECOND_INDEX   => FIRST_HALF,
                             THIRD_INDEX    => FIRST_FIVE,
                             COMPONENT_TYPE => DATE,
                             DEFAULT_VALUE  => TODAY,
                             CUBE           => THREE_DIMENSIONAL,
                             PROC           => TD_PROC) ;

     FUNCTION  NFUNC1 IS NEW FUNC1 (NUMBERS, F1);
     PACKAGE   NPKG1  IS NEW PKG1  (NUMBERS, F1);
     PROCEDURE NPROC1 IS NEW PROC1 (NUMBERS, F1);

     FUNCTION  NFUNC2 IS NEW FUNC2 (NUMBERS, P2);
     PACKAGE   NPKG2  IS NEW PKG2  (NUMBERS, P2);
     PROCEDURE NPROC2 IS NEW PROC2 (NUMBERS, P2);

BEGIN  -- CC1311A

     IF NOT NFUNC1 THEN
          REPORT.FAILED ("INCORRECT DEFAULT VALUE " &
                         "WITH FUNCTION 'NFUNC1'" ) ;
     END IF ;

     IF NOT NFUNC2 THEN
          REPORT.FAILED ("INCORRECT DEFAULT VALUE " &
                         "WITH FUNCTION 'NFUNC2'" ) ;
     END IF ;

     NPROC1 ;
     NPROC2 ;
    
     NEW_PROC_WITH_3D_FUNC ;
    
     IF NOT NEW_FUNC_WITH_3D_FUNC THEN
          REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL ARRAY, " &
                         "FUNCTION, AND FUNCTION.") ;
     END IF ;
    
     NEW_PROC_WITH_3D_PROC ;
    
     IF NOT NEW_FUNC_WITH_3D_PROC THEN
          REPORT.FAILED ("PROBLEMS WITH THREE DIMENSIONAL ARRAY, " &
                         "FUNCTION, AND PROCEDURE.") ;
     END IF ;
        
     REPORT.RESULT ;

END CC1311A ;
