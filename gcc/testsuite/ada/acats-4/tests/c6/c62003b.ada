-- C62003B.ADA

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
-- CHECK THAT PRIVATE TYPES IMPLEMENTED AS SCALAR OR ACCESS TYPES ARE
--   PASSED BY COPY.
--   SUBTESTS ARE:
--        (A) PRIVATE SCALAR PARAMETERS TO PROCEDURES.
--        (B) PRIVATE SCALAR PARAMETERS TO FUNCTIONS.
--        (C) PRIVATE ACCESS PARAMETERS TO PROCEDURES.
--        (D) PRIVATE ACCESS PARAMETERS TO FUNCTIONS.

-- CPP 05/25/84
-- EG  10/29/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

WITH REPORT;  USE REPORT;
PROCEDURE C62003B IS

BEGIN
     TEST("C62003B", "CHECK THAT PRIVATE SCALAR AND ACCESS " &
                     "PARAMETERS ARE COPIED");

     ---------------------------------------------------

A_B: DECLARE

          PACKAGE SCALAR_PKG IS

               TYPE T IS PRIVATE;
               C0 : CONSTANT T;
               C1 : CONSTANT T;
               C10 : CONSTANT T;
               C100 : CONSTANT T;

               FUNCTION "+" (OLD : IN T; INCREMENT : IN T) RETURN T;
               FUNCTION CONVERT (OLD_PRIVATE : IN T) RETURN INTEGER;

          PRIVATE
               TYPE T IS NEW INTEGER;
               C0 : CONSTANT T := 0;
               C1 : CONSTANT T := 1;
               C10 : CONSTANT T := 10;
               C100 : CONSTANT T := 100;

          END SCALAR_PKG;


          PACKAGE BODY SCALAR_PKG IS

               FUNCTION "+" (OLD : IN T; INCREMENT : IN T) RETURN T IS
               BEGIN     -- "+"
                    RETURN T(INTEGER(OLD) + INTEGER(INCREMENT));
               END "+";

               FUNCTION CONVERT (OLD_PRIVATE : IN T) RETURN INTEGER IS
               BEGIN     -- CONVERT
                    RETURN INTEGER(OLD_PRIVATE);
               END CONVERT;

          END SCALAR_PKG;

          USE SCALAR_PKG;

     ---------------------------------------------------

     BEGIN     -- A_B

      A : DECLARE

               I : T;
               E : EXCEPTION;

               PROCEDURE P (PI : IN T; PO : OUT T; PIO : IN OUT T) IS

                    TEMP : T;

               BEGIN  -- P

                    TEMP := PI;    -- SAVE VALUE OF PI AT PROC ENTRY.

                    PO := C10;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (SCALAR) OUT " &
                                 "PARAMETER CHANGES THE VALUE OF " &
                                 "INPUT PARAMETER");
                         TEMP := PI;    -- RESET TEMP FOR NEXT CASE.
                    END IF;

                    PIO := PIO + C100;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (SCALAR) IN " &
                                 "OUT PARAMETER CHANGES THE VALUE OF " &
                                 "INPUT PARAMETER");
                         TEMP := PI;    -- RESET TEMP FOR NEXT CASE.
                    END IF;

                    I := I + C1;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (SCALAR) " &
                                 "ACTUAL PARAMETER CHANGES THE " &
                                 "VALUE OF INPUT PARAMETER");
                    END IF;

                    RAISE E;  -- CHECK EXCEPTION HANDLING.
               END P;

          BEGIN  -- A
               I := C0;  -- INITIALIZE I SO VARIOUS CASES CAN BE
                         -- DETECTED.
               P (I, I, I);
               FAILED ("EXCEPTION NOT RAISED - A");
          EXCEPTION
               WHEN E =>
                    IF (I /= C1) THEN
                         CASE CONVERT(I) IS
                              WHEN 11 =>
                                   FAILED ("OUT ACTUAL PRIVATE " &
                                           "(SCALAR) PARAMETER " &
                                           "CHANGED GLOBAL VALUE");
                              WHEN 101 =>
                                   FAILED ("IN OUT ACTUAL PRIVATE " &
                                           "(SCALAR) PARAMETER " &
                                           "CHANGED GLOBAL VALUE");
                              WHEN 111 =>
                                   FAILED ("OUT AND IN OUT ACTUAL " &
                                           "PRIVATE (SCALAR) " &
                                           "PARAMETER CHANGED " &
                                           "GLOBAL VALUE");
                              WHEN OTHERS =>
                                   FAILED ("UNDETERMINED CHANGE TO " &
                                           "GLOBAL VALUE");
                         END CASE;
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - A");
          END A;

     ---------------------------------------------------

      B : DECLARE

               I, J : T;

               FUNCTION F (FI : IN T) RETURN T IS

                    TEMP : T := FI;  -- SAVE VALUE OF FI AT FN ENTRY.

               BEGIN  -- F

                    I := I + C1;
                    IF (FI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (SCALAR) " &
                                 "ACTUAL FUNCTION PARAMETER CHANGES " &
                                 "THE VALUE OF INPUT PARAMETER ");
                    END IF;

                    RETURN C0;
               END F;

          BEGIN  -- B
               I := C0;
               J := F(I);
          END B;

     END A_B;

     ---------------------------------------------------

C_D: DECLARE

          PACKAGE ACCESS_PKG IS

               TYPE T IS PRIVATE;
               C_NULL : CONSTANT T;
               C1 : CONSTANT T;
               C10 : CONSTANT T;
               C100 : CONSTANT T;
               C101 : CONSTANT T;

          PRIVATE
               TYPE T IS ACCESS INTEGER;
               C_NULL : CONSTANT T := NULL;
               C1 : CONSTANT T := NEW INTEGER'(1);
               C10 : CONSTANT T := NEW INTEGER'(10);
               C100 : CONSTANT T := NEW INTEGER'(100);
               C101 : CONSTANT T := NEW INTEGER'(101);

          END ACCESS_PKG;

          USE ACCESS_PKG;

     ---------------------------------------------------

     BEGIN     -- C_D;

      C : DECLARE

               I : T;
               E : EXCEPTION;
               PROCEDURE P (PI : IN T; PO : OUT T; PIO : IN OUT T) IS

                    TEMP : T;

               BEGIN     -- P

                    TEMP := PI;    -- SAVE VALUE OF PI AT PROC ENTRY.

                    I := C101;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (ACCESS) " &
                                 "ACTUAL VARIABLE CHANGES THE VALUE " &
                                 "OF INPUT PARAMETER");
                         TEMP := PI;    -- RESET TEMP FOR NEXT CASE.
                    END IF;

                    PO := C1;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (ACCESS) OUT " &
                                 "PARAMETER CHANGES THE VALUE OF " &
                                 "INPUT PARAMETER");
                         TEMP := PI;    -- RESET TEMP FOR NEXT CASE.
                    END IF;

                    PIO := C10;
                    IF (PI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE (ACCESS) IN " &
                                 "OUT PARAMETER CHANGES THE VALUE " &
                                 "OF INPUT PARAMETER");
                    END IF;

                    RAISE E;  -- CHECK EXCEPTION HANDLING.
               END P;

          BEGIN     -- C
               I := C100;
               P (I, I, I);
               FAILED ("EXCEPTION NOT RAISED - C");
          EXCEPTION
               WHEN E =>
                    IF (I /= C101) THEN
                         FAILED ("OUT OR IN OUT ACTUAL PROCEDURE " &
                                 "PARAMETER VALUE CHANGED DESPITE " &
                                 "RAISED EXCEPTION");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - C");
          END C;

     ---------------------------------------------------

      D : DECLARE

               I, J : T;

               FUNCTION F (FI : IN T) RETURN T IS

                    TEMP : T := FI;     -- SAVE VALUE OF FI AT FN ENTRY.

               BEGIN     -- F
                    I := C100;
                    IF (FI /= TEMP) THEN
                         FAILED ("ASSIGNMENT TO PRIVATE " &
                                 "(ACCESS) ACTUAL FUNCTION " &
                                 "PARAMETER CHANGES THE VALUE " &
                                 "OF INPUT PARAMETER");
                    END IF;
                    RETURN C_NULL;
               END F;

           BEGIN     -- D
               I := C_NULL;
               J := F(I);
          END D;

     END C_D;

     ---------------------------------------------------

     RESULT;

END C62003B;
