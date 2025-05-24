-- CC3017C.ADA

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
--     CHECK THAT AN INSTANCE OF A GENERIC PROCEDURE MUST DECLARE A
--     PROCEDURE AND THAT AN INSTANCE OF A GENERIC FUNCTION MUST
--     DECLARE A FUNCTION. CHECK THAT SCALAR AND ACCESS PARAMETERS
--     ARE COPIED.
--
--     SUBTESTS ARE:
--         (A) SCALAR PARAMETERS TO PROCEDURES.
--         (B) SCALAR PARAMETERS TO FUNCTIONS.
--         (C) ACCESS PARAMETERS TO PROCEDURES.
--         (D) ACCESS PARAMETERS TO FUNCTIONS.

-- HISTORY:
--     EDWARD V. BERARD, 7 AUGUST 1990
--     CJJ 10/16/90  ADJUSTED LINES THAT WERE TOO LONG; REFORMATTED
--                   HEADER TO CONFORM TO ACVC STANDARDS.
--

WITH REPORT;
PROCEDURE CC3017C IS

BEGIN
     REPORT.TEST ("CC3017C", "CHECK THAT AN INSTANCE OF A GENERIC " &
                  "PROCEDURE MUST DECLARE A PROCEDURE AND THAT AN " &
                  "INSTANCE OF A GENERIC FUNCTION MUST DECLARE A " &
                  "FUNCTION. CHECK THAT SCALAR AND ACCESS PARAMETERS " &
                  "ARE COPIED");

     --------------------------------------------------

     SCALAR_TO_PROCS:
     
     DECLARE

--        (A) SCALAR PARAMETERS TO PROCEDURES.

          TYPE NUMBER IS RANGE 0 .. 120 ;
          VALUE : NUMBER ;
          E     : EXCEPTION ;

          GENERIC
          
            TYPE SCALAR_ITEM IS RANGE <> ;
            
          PROCEDURE P (P_IN     : IN SCALAR_ITEM ;
                       P_OUT     : OUT SCALAR_ITEM ;
                       P_IN_OUT : IN OUT SCALAR_ITEM) ;
                       
          PROCEDURE P (P_IN     : IN SCALAR_ITEM ;
                       P_OUT     : OUT SCALAR_ITEM ;
                       P_IN_OUT : IN OUT SCALAR_ITEM) IS

               STORE  : SCALAR_ITEM ;

          BEGIN  -- P

               STORE := P_IN;     -- SAVE VALUE OF P_IN AT PROC ENTRY.

               P_OUT := 10;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO SCALAR OUT " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
                    STORE := P_IN;     -- RESET STORE FOR NEXT CASE.
               END IF;

               P_IN_OUT := P_IN_OUT + 100;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO SCALAR IN OUT " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
                    STORE := P_IN;     -- RESET STORE FOR NEXT CASE.
               END IF;

               VALUE := VALUE + 1;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO SCALAR GLOBAL " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
               END IF;

               RAISE E;  -- CHECK EXCEPTION HANDLING.
          END P;
          
          PROCEDURE NEW_P IS NEW P (SCALAR_ITEM => NUMBER) ;

     BEGIN  -- SCALAR_TO_PROCS
          VALUE := 0;   -- INITIALIZE VALUE SO VARIOUS CASES CAN BE DETECTED.
          
          NEW_P (P_IN     => VALUE,
                 P_OUT    => VALUE,
                 P_IN_OUT => VALUE);
          
          REPORT.FAILED ("EXCEPTION NOT RAISED - SCALARS TO PROCEDURES");
     EXCEPTION
          WHEN E =>
               IF (VALUE /= 1) THEN
                    CASE VALUE IS
                         WHEN 11  =>
                              REPORT.FAILED ("OUT ACTUAL SCALAR " &
                                             "PARAMETER CHANGED GLOBAL VALUE");
                         WHEN 101 =>
                              REPORT.FAILED ("IN OUT ACTUAL SCALAR " &
                                             "PARAMETER CHANGED GLOBAL VALUE");
                         WHEN 111 =>
                              REPORT.FAILED ("OUT AND IN OUT ACTUAL " &
                                             "SCALAR PARAMETERS CHANGED " &
                                             "GLOBAL VALUE");
                         WHEN OTHERS =>
                              REPORT.FAILED ("UNDETERMINED CHANGE TO " &
                                             "GLOBAL VALUE");
                    END CASE;
               END IF;
          WHEN OTHERS =>
              REPORT.FAILED ("WRONG EXCEPTION RAISED - SCALARS TO PROCEDURES");
     END SCALAR_TO_PROCS ;

     --------------------------------------------------

     SCALAR_TO_FUNCS:
     
     DECLARE

--        (B) SCALAR PARAMETERS TO FUNCTIONS.

          TYPE NUMBER IS RANGE 0 .. 101 ;
          FIRST  : NUMBER ;
          SECOND : NUMBER ;

          GENERIC
          
              TYPE ITEM IS RANGE <> ;
            
          FUNCTION F (F_IN : IN ITEM) RETURN ITEM ;
          
          FUNCTION F (F_IN : IN ITEM) RETURN ITEM IS

               STORE  : ITEM := F_IN;

          BEGIN  -- F

               FIRST := FIRST + 1;
               IF (F_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO SCALAR GLOBAL FUNCTION " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
               END IF;

               RETURN (100);
          END F;
          
          FUNCTION NEW_F IS NEW F (ITEM => NUMBER) ;

     BEGIN  -- SCALAR_TO_FUNCS
          FIRST  := 100 ;
          SECOND := NEW_F (FIRST) ;
     END SCALAR_TO_FUNCS ;

     --------------------------------------------------

     ACCESS_TO_PROCS:
     
     DECLARE

--        (C) ACCESS PARAMETERS TO PROCEDURES.

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
          DATE_POINTER : DATE_ACCESS ;
          
          E    : EXCEPTION;
          
          GENERIC
          
            TYPE ITEM IS PRIVATE ;
            TYPE ACCESS_ITEM IS ACCESS ITEM ;

          PROCEDURE P (P_IN     : IN     ACCESS_ITEM ;  
                       P_OUT    : OUT    ACCESS_ITEM ;
                       P_IN_OUT : IN OUT ACCESS_ITEM) ;

          PROCEDURE P (P_IN     : IN     ACCESS_ITEM ;  
                       P_OUT    : OUT    ACCESS_ITEM ;
                       P_IN_OUT : IN OUT ACCESS_ITEM) IS

               STORE  : ACCESS_ITEM ;

          BEGIN  -- P

               STORE := P_IN ;     -- SAVE VALUE OF P_IN AT PROC ENTRY.

               DATE_POINTER := NEW DATE'(YEAR  => 1990,
                                         DAY   => 7,
                                         MONTH => AUG) ;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO ACCESS GLOBAL " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
                    STORE := P_IN;     -- RESET STORE FOR NEXT CASE.
               END IF;

               P_OUT := NEW ITEM ;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO ACCESS OUT " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
                    STORE := P_IN;     -- RESET STORE FOR NEXT CASE.
               END IF;

               P_IN_OUT :=  NEW ITEM ;
               IF (P_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO ACCESS IN OUT " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
               END IF;

               RAISE E;  -- CHECK EXCEPTION HANDLING.
          END P ;
          
          PROCEDURE NEW_P IS NEW P (ITEM         => DATE,
                                    ACCESS_ITEM  => DATE_ACCESS) ;

     BEGIN  -- ACCESS_TO_PROCS
          DATE_POINTER := NEW DATE'(MONTH => DEC,
                                    DAY   => 25,
                                    YEAR  => 2000) ;
                                    
          NEW_P (P_IN       => DATE_POINTER,
                   P_OUT    => DATE_POINTER,
                   P_IN_OUT => DATE_POINTER) ;
                 
          REPORT.FAILED ("EXCEPTION NOT RAISED - ACCESS TO PROCEDURES");
     EXCEPTION
          WHEN E =>
               IF (DATE_POINTER.ALL /= (AUG, 7, 1990)) THEN
                    REPORT.FAILED ("OUT OR IN OUT ACTUAL PROCEDURE " &
                                   "PARAMETER VALUE CHANGED DESPITE " &
                                   "RAISED EXCEPTION");
               END IF;
          WHEN OTHERS =>
               REPORT.FAILED ("WRONG EXCEPTION RAISED - ACCESS TO PROCEDURES");
     END ACCESS_TO_PROCS ;

     --------------------------------------------------

     ACCESS_TO_FUNCS:
     
     DECLARE
     
--        (D) ACCESS PARAMETERS TO FUNCTIONS.

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
          DATE_POINTER : DATE_ACCESS ;
          NEXT_DATE    : DATE_ACCESS ;

          GENERIC
          
            TYPE ITEM IS PRIVATE ;
            TYPE ACCESS_ITEM IS ACCESS ITEM ;

          FUNCTION F (F_IN : IN ACCESS_ITEM) RETURN ACCESS_ITEM ;

          FUNCTION F (F_IN : IN ACCESS_ITEM) RETURN ACCESS_ITEM IS

               STORE  : ACCESS_ITEM := F_IN ;

          BEGIN  -- F

               DATE_POINTER := NEW DATE'(YEAR  => 1990,
                                            DAY   => 7,
                                         MONTH => AUG) ;
               IF (F_IN /= STORE) THEN
                    REPORT.FAILED ("ASSIGNMENT TO ACCESS GLOBAL FUNCTION " &
                                   "PARAMETER CHANGES THE VALUE OF " &
                                   "INPUT PARAMETER");
               END IF;

               RETURN (NULL);
          END F ;
          
          FUNCTION NEW_F IS NEW F (ITEM        => DATE,
                                   ACCESS_ITEM => DATE_ACCESS) ;

     BEGIN  -- ACCESS_TO_FUNCS
          DATE_POINTER := NULL ;
          NEXT_DATE    := NEW_F(F_IN => DATE_POINTER) ;
     END ACCESS_TO_FUNCS ;

     --------------------------------------------------

     REPORT.RESULT;

END CC3017C;
