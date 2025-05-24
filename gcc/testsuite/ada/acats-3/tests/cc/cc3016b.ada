-- CC3016B.ADA

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
--  PACKAGE. CHECK THAT THE DECLARATIVE ITEMS IN AN INSTANTIATION
--  OF A GENERIC PACKAGE SPECIFICATION ARE ELABORATED IN THE ORDER
--  DECLARED.    

-- HISTORY:
--         EDWARD V. BERARD, 8 AUGUST 1990

WITH REPORT ;

PROCEDURE CC3016B IS
     
    WHEN_ELABORATED : NATURAL := 0 ;
    
    TYPE REAL IS DIGITS 6 ;
    REAL_VALUE : REAL := 3.14159 ;
    
    TRUE_VALUE : BOOLEAN := TRUE ;
    
    CHARACTER_VALUE : CHARACTER := 'Z' ;
          
    TYPE MONTH_TYPE IS (JAN, FEB, MAR, APR, MAY, JUN, JUL, AUG,
                        SEP, OCT, NOV, DEC) ;
    TYPE DAY_TYPE IS RANGE 1 .. 31 ;
    TYPE YEAR_TYPE IS RANGE 1904 .. 2050 ;
    TYPE DATE IS RECORD
      MONTH : MONTH_TYPE ;
      DAY   : DAY_TYPE ;
      YEAR  : YEAR_TYPE ;
    END RECORD ;
          
    TYPE DATE_ACCESS IS ACCESS DATE ;

    THIS_MONTH   : MONTH_TYPE := AUG ;
    THIS_YEAR     : YEAR_TYPE := 1990 ;

    TODAY         : DATE := (MONTH => AUG,
                               DAY   => 8,
                              YEAR  => 1990) ;

    FIRST_DATE   : DATE_ACCESS  := NEW DATE'(DAY   => 6,
                                             MONTH => JUN,
                                             YEAR  => 1967) ;
    
    TYPE DUE_DATES IS ARRAY (MONTH_TYPE RANGE JAN .. DEC) OF DATE ;
    REPORT_DATES : DUE_DATES := ((JAN, 23, 1990), (FEB, 23, 1990),
                                 (MAR, 23, 1990), (APR, 23, 1990),
                                 (MAY, 23, 1990), (JUN, 22, 1990),
                                 (JUL, 23, 1990), (AUG, 23, 1990),
                                 (SEP, 24, 1990), (OCT, 23, 1990),
                                 (NOV, 23, 1990), (DEC, 20, 1990)) ;
                                 
    TYPE LIST_INDEX IS RANGE 1 .. 16 ;
    TYPE LIST IS ARRAY (LIST_INDEX) OF NATURAL ;
    ORDER_LIST : LIST := (OTHERS => 0) ;

    GENERIC
    
        TYPE RETURN_TYPE IS PRIVATE ;
        RETURN_VALUE : IN OUT RETURN_TYPE ;
        POSITION      : IN       NATURAL ;
        OFFSET        : IN       NATURAL ;
        WHEN_ELAB     : IN OUT NATURAL ;
        TYPE INDEX IS RANGE <> ;
        TYPE LIST IS ARRAY (INDEX) OF NATURAL ;
        ORDER_LIST      : IN OUT LIST ;
    
    FUNCTION NAME (VALUE : IN NATURAL) RETURN RETURN_TYPE ;
    
    FUNCTION NAME (VALUE : IN NATURAL) RETURN RETURN_TYPE IS
    
    BEGIN -- NAME
        
        IF (VALUE = POSITION) THEN
          WHEN_ELAB := NATURAL'SUCC (WHEN_ELAB) ;
          ORDER_LIST (INDEX (POSITION)) := WHEN_ELAB ;
          RETURN RETURN_VALUE ;
        ELSIF (VALUE = (POSITION + OFFSET)) THEN
          WHEN_ELAB := NATURAL'SUCC (WHEN_ELAB) ;
          ORDER_LIST (INDEX (POSITION + OFFSET)) := WHEN_ELAB ;
          RETURN RETURN_VALUE ;
        END IF ;
    
    END NAME ;
    
    GENERIC
    
        TYPE FIRST_TYPE IS PRIVATE ;
        WITH FUNCTION FIRST (POSITION : IN NATURAL) 
                            RETURN FIRST_TYPE ;
        FIRST_VALUE : IN NATURAL ;
        TYPE SECOND_TYPE IS PRIVATE ;
        WITH FUNCTION SECOND (POSITION : IN NATURAL) 
                            RETURN SECOND_TYPE ;
        SECOND_VALUE : IN NATURAL ;
        TYPE THIRD_TYPE IS PRIVATE ;
        WITH FUNCTION THIRD (POSITION : IN NATURAL) 
                            RETURN THIRD_TYPE ;
        THIRD_VALUE : IN NATURAL ;
        TYPE FOURTH_TYPE IS PRIVATE ;
        WITH FUNCTION FOURTH (POSITION : IN NATURAL) 
                            RETURN FOURTH_TYPE ;
        FOURTH_VALUE : IN NATURAL ;
        TYPE FIFTH_TYPE IS PRIVATE ;
        WITH FUNCTION FIFTH (POSITION : IN NATURAL) 
                            RETURN FIFTH_TYPE ;
        FIFTH_VALUE : IN NATURAL ;
        TYPE SIXTH_TYPE IS PRIVATE ;
        WITH FUNCTION SIXTH (POSITION : IN NATURAL) 
                            RETURN SIXTH_TYPE ;
        SIXTH_VALUE : IN NATURAL ;
        TYPE SEVENTH_TYPE IS PRIVATE ;
        WITH FUNCTION SEVENTH (POSITION : IN NATURAL) 
                            RETURN SEVENTH_TYPE ;
        SEVENTH_VALUE : IN NATURAL ;
        TYPE EIGHTH_TYPE IS PRIVATE ;
        WITH FUNCTION EIGHTH (POSITION : IN NATURAL) 
                            RETURN EIGHTH_TYPE ;
        EIGHTH_VALUE : IN NATURAL ;
        TYPE NINTH_TYPE IS PRIVATE ;
        WITH FUNCTION NINTH (POSITION : IN NATURAL) 
                            RETURN NINTH_TYPE ;
        NINTH_VALUE : IN NATURAL ;
        TYPE TENTH_TYPE IS PRIVATE ;
        WITH FUNCTION TENTH (POSITION : IN NATURAL) 
                            RETURN TENTH_TYPE ;
        TENTH_VALUE : IN NATURAL ;
        TYPE ELEVENTH_TYPE IS PRIVATE ;
        WITH FUNCTION ELEVENTH (POSITION : IN NATURAL) 
                            RETURN ELEVENTH_TYPE ;
        ELEVENTH_VALUE : IN NATURAL ;
        TYPE TWELFTH_TYPE IS PRIVATE ;
        WITH FUNCTION TWELFTH (POSITION : IN NATURAL) 
                            RETURN TWELFTH_TYPE ;
        TWELFTH_VALUE : IN NATURAL ;
        TYPE THIRTEENTH_TYPE IS PRIVATE ;
        WITH FUNCTION THIRTEENTH (POSITION : IN NATURAL) 
                            RETURN THIRTEENTH_TYPE ;
        THIRTEENTH_VALUE : IN NATURAL ;
        TYPE FOURTEENTH_TYPE IS PRIVATE ;
        WITH FUNCTION FOURTEENTH (POSITION : IN NATURAL) 
                            RETURN FOURTEENTH_TYPE ;
        FOURTEENTH_VALUE : IN NATURAL ;
        TYPE FIFTEENTH_TYPE IS PRIVATE ;
        WITH FUNCTION FIFTEENTH (POSITION : IN NATURAL) 
                            RETURN FIFTEENTH_TYPE ;
        FIFTEENTH_VALUE : IN NATURAL ;
        TYPE SIXTEENTH_TYPE IS PRIVATE ;
        WITH FUNCTION SIXTEENTH (POSITION : IN NATURAL) 
                            RETURN SIXTEENTH_TYPE ;
        SIXTEENTH_VALUE : IN NATURAL ;
        
    PACKAGE ORDER_PACKAGE IS
    
        A : FIRST_TYPE      := FIRST (FIRST_VALUE) ;
        B : SECOND_TYPE     := SECOND (SECOND_VALUE) ;
        C : THIRD_TYPE      := THIRD (THIRD_VALUE) ;
        D : FOURTH_TYPE     := FOURTH (FOURTH_VALUE) ;
        E : FIFTH_TYPE      := FIFTH (FIFTH_VALUE) ;
        F : SIXTH_TYPE      := SIXTH (SIXTH_VALUE) ;
        G : SEVENTH_TYPE    := SEVENTH (SEVENTH_VALUE) ;
        H : EIGHTH_TYPE     := EIGHTH (EIGHTH_VALUE) ;
        I : NINTH_TYPE      := NINTH (NINTH_VALUE) ;
        J : TENTH_TYPE      := TENTH (TENTH_VALUE) ;
        K : ELEVENTH_TYPE   := ELEVENTH (ELEVENTH_VALUE) ;
        L : TWELFTH_TYPE    := TWELFTH (TWELFTH_VALUE) ;
        M : THIRTEENTH_TYPE := THIRTEENTH (THIRTEENTH_VALUE) ;
        N : FOURTEENTH_TYPE := FOURTEENTH (FOURTEENTH_VALUE) ;
        O : FIFTEENTH_TYPE  := FIFTEENTH (FIFTEENTH_VALUE) ;
        P : SIXTEENTH_TYPE  := SIXTEENTH (SIXTEENTH_VALUE) ;
        
    END ORDER_PACKAGE ;
    

    FUNCTION BOOL IS NEW NAME (RETURN_TYPE  => BOOLEAN,
                               RETURN_VALUE => TRUE_VALUE,
                               POSITION     => 1,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               
    FUNCTION INT IS NEW NAME (RETURN_TYPE   => YEAR_TYPE,
                              RETURN_VALUE  => THIS_YEAR,
                              POSITION      => 2,
                              OFFSET        => 8,
                              WHEN_ELAB     => WHEN_ELABORATED,
                              INDEX         => LIST_INDEX,
                              LIST          => LIST,
                              ORDER_LIST    => ORDER_LIST) ;
                               
    FUNCTION FLOAT IS NEW NAME (RETURN_TYPE  => REAL,
                                RETURN_VALUE => REAL_VALUE,
                                POSITION     => 3,
                                OFFSET       => 8,
                                WHEN_ELAB    => WHEN_ELABORATED,
                                INDEX        => LIST_INDEX,
                                LIST         => LIST,
                                ORDER_LIST   => ORDER_LIST) ;
                               
    FUNCTION CHAR IS NEW NAME (RETURN_TYPE  => CHARACTER,
                               RETURN_VALUE => CHARACTER_VALUE,
                               POSITION     => 4,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               
    FUNCTION ENUM IS NEW NAME (RETURN_TYPE  => MONTH_TYPE,
                               RETURN_VALUE => THIS_MONTH,
                               POSITION     => 5,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               
    FUNCTION ARRY IS NEW NAME (RETURN_TYPE  => DUE_DATES,
                               RETURN_VALUE => REPORT_DATES,
                               POSITION     => 6,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               

    FUNCTION RCRD IS NEW NAME (RETURN_TYPE  => DATE,
                               RETURN_VALUE => TODAY,
                               POSITION     => 7,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               

    FUNCTION ACSS IS NEW NAME (RETURN_TYPE  => DATE_ACCESS,
                               RETURN_VALUE => FIRST_DATE,
                               POSITION     => 8,
                               OFFSET       => 8,
                               WHEN_ELAB    => WHEN_ELABORATED,
                               INDEX        => LIST_INDEX,
                               LIST         => LIST,
                               ORDER_LIST   => ORDER_LIST) ;
                               
    PACKAGE ELABORATION_ORDER IS NEW ORDER_PACKAGE
        (FIRST_TYPE            => BOOLEAN,
         FIRST                 => BOOL,
         FIRST_VALUE           => 1,
         THIRD_TYPE            => REAL,
         THIRD                 => FLOAT,
         THIRD_VALUE           => 3,
         SECOND_TYPE           => YEAR_TYPE,    -- ORDERING OF PARAMETERS
         SECOND                => INT,          -- IS DELIBERATE. 
         SECOND_VALUE          => 2,
         FOURTH_TYPE           => CHARACTER,
         FOURTH                => CHAR,
         FOURTH_VALUE          => 4,
         FIFTH_TYPE            => MONTH_TYPE,
         FIFTH                 => ENUM,
         FIFTH_VALUE           => 5,
         SIXTH_TYPE            => DUE_DATES,
         SIXTH                 => ARRY,
         SIXTH_VALUE           => 6,
         SEVENTH_TYPE          => DATE,
         SEVENTH               => RCRD,
         SEVENTH_VALUE         => 7,
         EIGHTH_TYPE           => DATE_ACCESS,
         EIGHTH                => ACSS,
         EIGHTH_VALUE          => 8,
         NINTH_TYPE            => BOOLEAN,
         NINTH                 => BOOL,
         NINTH_VALUE           => 9,
         TENTH_TYPE            => YEAR_TYPE,
         TENTH                 => INT,
         TENTH_VALUE           => 10,
         ELEVENTH_TYPE         => REAL,
         ELEVENTH              => FLOAT,
         ELEVENTH_VALUE        => 11,
         TWELFTH_TYPE          => CHARACTER,
         TWELFTH               => CHAR,
         TWELFTH_VALUE         => 12,
         THIRTEENTH_TYPE       => MONTH_TYPE,
         THIRTEENTH            => ENUM,
         THIRTEENTH_VALUE      => 13,
         FOURTEENTH_TYPE       => DUE_DATES,
         FOURTEENTH            => ARRY,
         FOURTEENTH_VALUE      => 14,
         FIFTEENTH_TYPE        => DATE,
         FIFTEENTH             => RCRD,
         FIFTEENTH_VALUE       => 15,
         SIXTEENTH_TYPE        => DATE_ACCESS,
         SIXTEENTH             => ACSS,
         SIXTEENTH_VALUE       => 16) ;

BEGIN
     REPORT.TEST("CC3016B", "CHECK THAT AN INSTANCE OF A GENERIC " &
                 "PACKAGE MUST DECLARE A PACKAGE. CHECK THAT THE " &
                 "DECLARATIVE ITEMS IN AN INSTANTIATION OF A GENERIC " &
                 "PACKAGE SPECIFICATION ARE ELABORATED IN THE ORDER " &
                 "DECLARED.");

     IF ORDER_LIST(1) /= REPORT.IDENT_INT(1) THEN
          REPORT.FAILED("BOOLEAN 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(2) /= REPORT.IDENT_INT(2) THEN
          REPORT.FAILED("INTEGER TYPE 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(3) /= REPORT.IDENT_INT(3) THEN
          REPORT.FAILED("REAL 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(4) /= REPORT.IDENT_INT(4) THEN
          REPORT.FAILED("CHARACTER 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(5) /= REPORT.IDENT_INT(5) THEN
          REPORT.FAILED("ENUMERATION 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(6) /= REPORT.IDENT_INT(6) THEN
          REPORT.FAILED("ARRAY 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(7) /= REPORT.IDENT_INT(7) THEN
          REPORT.FAILED("RECORD 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(8) /= REPORT.IDENT_INT(8) THEN
          REPORT.FAILED("ACCESS 1 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(9) /= REPORT.IDENT_INT(9) THEN
          REPORT.FAILED("BOOLEAN 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(10) /= REPORT.IDENT_INT(10) THEN
          REPORT.FAILED("INTEGER TYPE 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(11) /= REPORT.IDENT_INT(11) THEN
          REPORT.FAILED("REAL 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(12) /= REPORT.IDENT_INT(12) THEN
          REPORT.FAILED("CHARACTER 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(13) /= REPORT.IDENT_INT(13) THEN
          REPORT.FAILED("ENUMERATION 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(14) /= REPORT.IDENT_INT(14) THEN
          REPORT.FAILED("ARRAY 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(15) /= REPORT.IDENT_INT(15) THEN
          REPORT.FAILED("RECORD 2 ELABORATED OUT OF ORDER");
     END IF;

     IF ORDER_LIST(16) /= REPORT.IDENT_INT(16) THEN
          REPORT.FAILED("ACCESS 2 ELABORATED OUT OF ORDER");
     END IF;

     REPORT.RESULT ;
     
END CC3016B;
