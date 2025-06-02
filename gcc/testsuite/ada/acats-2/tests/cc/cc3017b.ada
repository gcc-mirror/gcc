-- CC3017B.ADA

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
-- CHECK THAT AN INSTANCE OF A GENERIC PROCEDURE MUST DECLARE A
-- PROCEDURE AND THAT AN INSTANCE OF A GENERIC FUNCTION MUST
-- DECLARE A FUNCTION. CHECK THAT CONSTRAINT_ERROR IS NOT RAISED
-- IF THE DEFAULT VALUE FOR A FORMAL PARAMETER DOES NOT SATISFY
-- THE CONSTRAINTS OF THE SUBTYPE_INDICATION WHEN THE     
-- DECLARATION IS ELABORATED, ONLY WHEN THE DEFAULT IS USED.   

--   SUBTESTS ARE:
--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.
--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.
--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.
--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

-- EDWARD V. BERARD, 7 AUGUST 1990

WITH REPORT;

PROCEDURE CC3017B IS

BEGIN

     REPORT.TEST ("CC3017B", "CHECK THAT AN INSTANCE OF A GENERIC " &
                  "PROCEDURE MUST DECLARE A PROCEDURE AND THAT AN " &
                  "INSTANCE OF A GENERIC FUNCTION MUST DECLARE A " &
                  "FUNCTION. CHECK THAT CONSTRAINT_ERROR IS NOT " &
                  "RAISED IF AN INITIALIZATION VALUE DOES NOT SATISFY " &
                  "CONSTRAINTS ON A FORMAL PARAMETER");

     --------------------------------------------------

     NONSTAT_ARRAY_PARMS:
     
     DECLARE
     
--        (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--            INITIALIZED WITH A STATIC AGGREGATE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            LOWER : IN INTEGER_TYPE ;
            UPPER : IN INTEGER_TYPE ;
          
          PROCEDURE PA (FIRST  : IN INTEGER_TYPE ;
                        SECOND : IN INTEGER_TYPE) ;

          PROCEDURE PA (FIRST  : IN INTEGER_TYPE ;
                        SECOND : IN INTEGER_TYPE) IS
                        
               TYPE A1 IS ARRAY (INTEGER_TYPE RANGE LOWER .. FIRST,
                                 INTEGER_TYPE RANGE LOWER .. SECOND)
                                         OF INTEGER_TYPE;

               PROCEDURE PA1 (A : A1 := ((LOWER,UPPER),(UPPER,UPPER)))
                    IS
               BEGIN
                    REPORT.FAILED ("BODY OF PA1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN PA1");
               END PA1;

          BEGIN  -- PA
               PA1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - PA1");
          END PA;
          
          PROCEDURE NEW_PA IS NEW PA (INTEGER_TYPE => NUMBER,
                                      LOWER        => 1,
                                      UPPER        => 50) ;

     BEGIN   -- NONSTAT_ARRAY_PARMS
     
          NEW_PA (FIRST  => NUMBER (25),
                  SECOND => NUMBER (75));
          
     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_PA");
               
     END NONSTAT_ARRAY_PARMS ;   
     
     --------------------------------------------------

     SCALAR_NON_STATIC:
     
     DECLARE
     
--        (B) A SCALAR PARAMETER WITH NON-STATIC RANGE CONSTRAINTS
--            INITIALIZED WITH A STATIC VALUE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            STATIC_VALUE : IN INTEGER_TYPE ;
          
          PROCEDURE PB (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) ;

          PROCEDURE PB (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) IS

               SUBTYPE INT IS INTEGER_TYPE RANGE LOWER .. UPPER ;

               PROCEDURE PB1 (I : INT := STATIC_VALUE) IS
               BEGIN  -- PB1
                    REPORT.FAILED ("BODY OF PB1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN PB1");
               END PB1;

          BEGIN  -- PB
               PB1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - PB1");
          END PB;

          PROCEDURE NEW_PB IS NEW PB (INTEGER_TYPE => NUMBER,
                                      STATIC_VALUE => 20) ;

     BEGIN   -- SCALAR_NON_STATIC
          
          NEW_PB (LOWER  => NUMBER (25),
                  UPPER  => NUMBER (75));

     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_PB");
     END SCALAR_NON_STATIC ; 

     --------------------------------------------------

     REC_NON_STAT_COMPS:
     
     DECLARE
     
--        (C) A RECORD PARAMETER WHOSE COMPONENTS HAVE NON-STATIC
--            CONSTRAINTS INITIALIZED WITH A STATIC AGGREGATE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            F_STATIC_VALUE : IN INTEGER_TYPE ;
            S_STATIC_VALUE : IN INTEGER_TYPE ;
            T_STATIC_VALUE : IN INTEGER_TYPE ;
            L_STATIC_VALUE : IN INTEGER_TYPE ;
          
          PROCEDURE PC (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) ;

          PROCEDURE PC (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) IS
                        
               SUBTYPE SUBINTEGER_TYPE IS INTEGER_TYPE
                       RANGE LOWER .. UPPER ;
               TYPE AR1 IS ARRAY (INTEGER RANGE 1..3) OF 
                     SUBINTEGER_TYPE ;
               TYPE REC IS
                    RECORD
                         FIRST  : SUBINTEGER_TYPE ;
                         SECOND : AR1 ;
                    END RECORD;

               PROCEDURE PC1 (R : REC := (F_STATIC_VALUE,
                                         (S_STATIC_VALUE,
                                          T_STATIC_VALUE,
                                          L_STATIC_VALUE))) IS
               BEGIN  -- PC1
                    REPORT.FAILED ("BODY OF PC1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN PC1");
               END PC1;

          BEGIN  -- PC
               PC1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - PC1");
          END PC;
         
          PROCEDURE NEW_PC IS NEW PC (INTEGER_TYPE => NUMBER,
                                      F_STATIC_VALUE => 15,
                                      S_STATIC_VALUE => 19,
                                      T_STATIC_VALUE => 85,
                                      L_STATIC_VALUE => 99) ;

     BEGIN   -- REC_NON_STAT_COMPS
          NEW_PC (LOWER => 20,
                  UPPER => 80);
     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_PC");
     END REC_NON_STAT_COMPS ;

     --------------------------------------------------

     FIRST_STATIC_ARRAY:

     DECLARE
     
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            F_STATIC_VALUE : IN INTEGER_TYPE ;
            S_STATIC_VALUE : IN INTEGER_TYPE ;
            T_STATIC_VALUE : IN INTEGER_TYPE ;
            L_STATIC_VALUE : IN INTEGER_TYPE ;
            A_STATIC_VALUE : IN INTEGER_TYPE ;
            B_STATIC_VALUE : IN INTEGER_TYPE ;
            C_STATIC_VALUE : IN INTEGER_TYPE ;
            D_STATIC_VALUE : IN INTEGER_TYPE ;
          
          PROCEDURE P1D (LOWER  : IN INTEGER_TYPE ;
                         UPPER  : IN INTEGER_TYPE) ;

          PROCEDURE P1D (LOWER  : IN INTEGER_TYPE ;
                         UPPER  : IN INTEGER_TYPE) IS
                         
               SUBTYPE SUBINTEGER_TYPE IS INTEGER_TYPE
                       RANGE LOWER .. UPPER ;

               TYPE A1 IS ARRAY (INTEGER_TYPE RANGE
                                       F_STATIC_VALUE .. S_STATIC_VALUE,
                                 INTEGER_TYPE RANGE
                                       T_STATIC_VALUE .. L_STATIC_VALUE)
                       OF SUBINTEGER_TYPE ;

               PROCEDURE P1D1 (A : A1 :=
                           ((A_STATIC_VALUE, B_STATIC_VALUE),
                           (C_STATIC_VALUE, D_STATIC_VALUE))) IS
               BEGIN  -- P1D1
                    REPORT.FAILED ("BODY OF P1D1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN P1D1");
               END P1D1;

          BEGIN  -- P1D
               P1D1 ;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - P1D1");
          END P1D;
          
          PROCEDURE NEW_P1D IS NEW P1D (INTEGER_TYPE => NUMBER,
                                        F_STATIC_VALUE => 21,
                                        S_STATIC_VALUE => 37,
                                        T_STATIC_VALUE => 67,
                                        L_STATIC_VALUE => 79,
                                        A_STATIC_VALUE => 11,
                                        B_STATIC_VALUE => 88,
                                        C_STATIC_VALUE => 87,
                                        D_STATIC_VALUE => 13) ;

     BEGIN  -- FIRST_STATIC_ARRAY
          NEW_P1D (LOWER => 10,
                     UPPER => 90);
     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_P1D");
     END FIRST_STATIC_ARRAY ;

     --------------------------------------------------

     SECOND_STATIC_ARRAY:
     
     DECLARE
     
--        (D) AN ARRAY PARAMETER CONSTRAINED WITH STATIC BOUNDS ON SUB-
--            SCRIPTS AND NON-STATIC BOUNDS ON COMPONENTS, INITIALIZED
--            WITH A STATIC AGGREGATE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            F_STATIC_VALUE : IN INTEGER_TYPE ;
            S_STATIC_VALUE : IN INTEGER_TYPE ;
            T_STATIC_VALUE : IN INTEGER_TYPE ;
            L_STATIC_VALUE : IN INTEGER_TYPE ;
            A_STATIC_VALUE : IN INTEGER_TYPE ;
            B_STATIC_VALUE : IN INTEGER_TYPE ;
          
          PROCEDURE P2D (LOWER  : IN INTEGER_TYPE ;
                         UPPER  : IN INTEGER_TYPE) ;

          PROCEDURE P2D (LOWER  : IN INTEGER_TYPE ;
                         UPPER  : IN INTEGER_TYPE) IS
                         
               SUBTYPE SUBINTEGER_TYPE IS INTEGER_TYPE
                       RANGE LOWER .. UPPER ;

               TYPE A1 IS ARRAY (INTEGER_TYPE RANGE
                                       F_STATIC_VALUE .. S_STATIC_VALUE,
                                 INTEGER_TYPE RANGE
                                       T_STATIC_VALUE .. L_STATIC_VALUE)
                       OF SUBINTEGER_TYPE ;

               PROCEDURE P2D1 (A : A1 :=
                                   (F_STATIC_VALUE .. S_STATIC_VALUE =>
                                   (A_STATIC_VALUE, B_STATIC_VALUE))) IS
               BEGIN  -- P2D1
                    REPORT.FAILED ("BODY OF P2D1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN P2D1");
               END P2D1;

          BEGIN  -- P2D
               P2D1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - P2D1");
          END P2D;
          
          PROCEDURE NEW_P2D IS NEW P2D (INTEGER_TYPE => NUMBER,
                                        F_STATIC_VALUE => 21,
                                        S_STATIC_VALUE => 37,
                                        T_STATIC_VALUE => 67,
                                        L_STATIC_VALUE => 79,
                                        A_STATIC_VALUE => 7,
                                        B_STATIC_VALUE => 93) ;

     BEGIN  -- SECOND_STATIC_ARRAY
          NEW_P2D (LOWER => 5,
                   UPPER => 95);
     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_P2D");
     END SECOND_STATIC_ARRAY ;

     --------------------------------------------------

     REC_NON_STATIC_CONS:
     
     DECLARE
     
--        (E) A RECORD PARAMETER WITH A NON-STATIC CONSTRAINT
--            INITIALIZED WITH A STATIC AGGREGATE.

          TYPE NUMBER IS RANGE 1 .. 100 ;
          
          GENERIC
          
            TYPE INTEGER_TYPE IS RANGE <> ;
            F_STATIC_VALUE : IN INTEGER_TYPE ;
            S_STATIC_VALUE : IN INTEGER_TYPE ;
            T_STATIC_VALUE : IN INTEGER_TYPE ;
            L_STATIC_VALUE : IN INTEGER_TYPE ;
            D_STATIC_VALUE : IN INTEGER_TYPE ;
          
          PROCEDURE PE (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) ;

          PROCEDURE PE (LOWER  : IN INTEGER_TYPE ;
                        UPPER  : IN INTEGER_TYPE) IS
                        
               SUBTYPE SUBINTEGER_TYPE IS INTEGER_TYPE
                       RANGE LOWER .. UPPER ;
               TYPE AR1 IS ARRAY (INTEGER RANGE 1..3) OF 
                     SUBINTEGER_TYPE ;
                    
               TYPE REC (DISCRIM : SUBINTEGER_TYPE) IS
                    RECORD
                         FIRST  : SUBINTEGER_TYPE ;
                         SECOND : AR1 ;
                    END RECORD ;

               SUBTYPE REC4 IS REC (LOWER) ;

               PROCEDURE PE1 (R : REC4 := (D_STATIC_VALUE,
                                           F_STATIC_VALUE,
                                          (S_STATIC_VALUE,
                                           T_STATIC_VALUE,
                                           L_STATIC_VALUE))) IS
               BEGIN  -- PE1
                    REPORT.FAILED ("BODY OF PE1 EXECUTED");
               EXCEPTION
                    WHEN OTHERS =>
                         REPORT.FAILED ("EXCEPTION RAISED IN PE1");
               END PE1;

          BEGIN  -- PE
               PE1;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    REPORT.FAILED ("WRONG EXCEPTION RAISED - PE1");
          END PE;

          PROCEDURE NEW_PE IS NEW PE (INTEGER_TYPE => NUMBER,
                                      F_STATIC_VALUE => 37,
                                      S_STATIC_VALUE => 21,
                                      T_STATIC_VALUE => 67,
                                      L_STATIC_VALUE => 79,
                                      D_STATIC_VALUE => 44) ;

     BEGIN  -- REC_NON_STATIC_CONS
          NEW_PE  (LOWER => 2,
                   UPPER => 99);
     EXCEPTION
          WHEN OTHERS =>
               REPORT.FAILED ("EXCEPTION RAISED IN CALL TO NEW_PE");
     END REC_NON_STATIC_CONS ;

     --------------------------------------------------

     REPORT.RESULT;

END CC3017B;
