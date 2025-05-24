-- CC3106B.ADA

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
--     CHECK THAT THE FORMAL PARAMETER DENOTES THE ACTUAL
--     IN AN INSTANTIATION.

-- HISTORY:
--     LDC 06/20/88  CREATED ORIGINAL TEST
--     EDWARD V. BERARD, 10 AUGUST 1990  ADDED CHECKS FOR MULTI-
--                                       DIMENSIONAL ARRAYS

WITH REPORT ;

PROCEDURE CC3106B IS

BEGIN  -- CC3106B

    REPORT.TEST("CC3106B","CHECK THAT THE FORMAL PARAMETER DENOTES " &
                "THE ACTUAL IN AN INSTANTIATION");

    LOCAL_BLOCK:
    
    DECLARE
    
        SUBTYPE SM_INT IS INTEGER RANGE 0..15 ;
        TYPE PCK_BOL IS ARRAY (5..18) OF BOOLEAN ;
        PRAGMA PACK(PCK_BOL) ;
          
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
                                     
        TD_ARRAY : THREE_DIMENSIONAL := (THREE_DIMENSIONAL'RANGE =>
                                        (THREE_DIMENSIONAL'RANGE (2) =>
                                        (THREE_DIMENSIONAL'RANGE (3) =>
                                          TODAY))) ;

        TASK TYPE TSK IS
            ENTRY ENT_1;
            ENTRY ENT_2;
            ENTRY ENT_3;
        END TSK;

        GENERIC
          
            TYPE GEN_TYPE IS (<>);
            GEN_BOLARR         : IN OUT PCK_BOL;
            GEN_TYP            : IN OUT GEN_TYPE;
            GEN_TSK            : IN OUT TSK;
            TEST_VALUE         : IN DATE ;
            TEST_CUBE          : IN OUT THREE_DIMENSIONAL ;

        PACKAGE P IS
               PROCEDURE GEN_PROC1 ;
               PROCEDURE GEN_PROC2 ;
               PROCEDURE GEN_PROC3 ;
               PROCEDURE ARRAY_TEST ;
        END P;

        ACT_BOLARR : PCK_BOL := (OTHERS => FALSE);
        SI         : SM_INT := 0 ;
        T          : TSK;

        PACKAGE BODY P IS
        
            PROCEDURE GEN_PROC1 IS
            BEGIN  -- GEN_PROC1
                GEN_BOLARR(14) := REPORT.IDENT_BOOL(TRUE);
                GEN_TYP := GEN_TYPE'VAL(4);
                IF ACT_BOLARR(14) /= TRUE OR SI /= REPORT.IDENT_INT(4)
                   THEN
                    REPORT.FAILED("VALUES ARE DIFFERENT THAN " &
                                  "INSTANTIATED VALUES");
                END IF;
            END GEN_PROC1;

            PROCEDURE GEN_PROC2 IS
            BEGIN  -- GEN_PROC2
                IF GEN_BOLARR(9) /= REPORT.IDENT_BOOL(TRUE) OR
                      GEN_TYPE'POS(GEN_TYP) /= REPORT.IDENT_INT(2) THEN
                    REPORT.FAILED("VALUES ARE DIFFERENT THAN " &
                                  "VALUES ASSIGNED IN THE MAIN " &
                                  "PROCEDURE");
                END IF;
                GEN_BOLARR(18) := TRUE;
                GEN_TYP := GEN_TYPE'VAL(9);
            END GEN_PROC2;

            PROCEDURE GEN_PROC3 IS
            BEGIN  -- GEN_PROC3
                GEN_TSK.ENT_2;
            END GEN_PROC3 ;
               
            PROCEDURE ARRAY_TEST IS            
            BEGIN  -- ARRAY_TEST
               
                TEST_CUBE (0, JUN, 'C') := TEST_VALUE ;
                           
                IF (TD_ARRAY (0, JUN, 'C')  /= TEST_VALUE) OR
                      (TEST_CUBE (-5, MAR, 'A') /= WALL_DATE) THEN
                    REPORT.FAILED ("MULTI-DIMENSIONAL ARRAY VALUES ARE " &
                                   "DIFFERENT THAN THE VALUES ASSIGNED " &
                                   "IN THE MAIN AND ARRAY_TEST PROCEDURES.") ;
                END IF ;
                
            END ARRAY_TEST ;
            
        END P ;

        TASK BODY TSK IS
        BEGIN  -- TSK
            ACCEPT ENT_1 DO
                REPORT.COMMENT("TASK ENTRY 1 WAS CALLED");
            END;
            ACCEPT ENT_2 DO
                REPORT.COMMENT("TASK ENTRY 2 WAS CALLED");
            END;
            ACCEPT ENT_3 DO
                REPORT.COMMENT("TASK ENTRY 3 WAS CALLED");
            END;
        END TSK;

        PACKAGE INSTA1 IS NEW P (GEN_TYPE       => SM_INT,
                                 GEN_BOLARR     => ACT_BOLARR,
                                 GEN_TYP        => SI,
                                 GEN_TSK        => T,
                                 TEST_VALUE     => FIRST_DATE,
                                 TEST_CUBE      => TD_ARRAY) ;
                             
    BEGIN  -- LOCAL_BLOCK
    
        INSTA1.GEN_PROC1;
        ACT_BOLARR(9) := TRUE;
        SI := 2;
        INSTA1.GEN_PROC2;
        IF ACT_BOLARR(18) /= REPORT.IDENT_BOOL(TRUE) OR
              SI /= REPORT.IDENT_INT(9) THEN
            REPORT.FAILED("VALUES ARE DIFFERENT THAN VALUES " &
                          "ASSIGNED IN THE GENERIC PROCEDURE");
        END IF;

        T.ENT_1;
        INSTA1.GEN_PROC3;
        T.ENT_3;
          
        TD_ARRAY (-5, MAR, 'A') := WALL_DATE ;
        INSTA1.ARRAY_TEST ;

     END LOCAL_BLOCK;

     REPORT.RESULT;

END CC3106B ;
