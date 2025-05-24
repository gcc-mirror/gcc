-- C95072B.ADA

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
-- PASSED BY COPY FOR ALL MODES.
-- SUBTESTS ARE:
--   (A)  PRIVATE SCALAR PARAMETERS TO ENTRIES.
--   (B)  PRIVATE ACCESS PARAMETERS TO ENTRIES.

-- JWC 7/22/85

WITH REPORT; USE REPORT;
PROCEDURE C95072B IS

BEGIN
     TEST("C95072B", "CHECK THAT PRIVATE SCALAR AND ACCESS " &
                     "PARAMETERS ARE COPIED");

     ---------------------------------------------------

     DECLARE  -- (A)

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
               BEGIN
                    RETURN T (INTEGER(OLD) + INTEGER(INCREMENT));
               END "+";

               FUNCTION CONVERT (OLD_PRIVATE : IN T) RETURN INTEGER IS
               BEGIN
                    RETURN INTEGER (OLD_PRIVATE);
               END CONVERT;

          END SCALAR_PKG;

          USE SCALAR_PKG;

     BEGIN  -- (A)

          DECLARE  -- (A1)

               I : T;
               E : EXCEPTION;

               TASK TA IS
                    ENTRY EA (EI : IN T; EO : OUT T;
                              EIO : IN OUT T);
               END TA;

               TASK BODY TA IS

                    TEMP : T;

               BEGIN

                    ACCEPT EA (EI : IN T; EO : OUT T;
                               EIO : IN OUT T) DO

                         TEMP := EI;    -- SAVE VALUE OF EI AT ACCEPT.

                         EO := C10;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(SCALAR) OUT PARAMETER " &
                                      "CHANGES THE VALUE OF INPUT " &
                                      "PARAMETER");
                              TEMP := EI;   -- RESET TEMP FOR NEXT CASE.
                         END IF;

                         EIO := EIO + C100;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(SCALAR) IN OUT PARAMETER " &
                                      "CHANGES THE VALUE OF INPUT " &
                                      "PARAMETER");
                              TEMP := EI;   -- RESET TEMP FOR NEXT CASE.
                         END IF;

                         I := I + C1;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(SCALAR) ACTUAL PARAMETER " &
                                      "CHANGES THE VALUE OF " &
                                      "INPUT PARAMETER");
                         END IF;

                         RAISE E;              -- CHECK EXCEPTION
                                               -- HANDLING.
                    END EA;

               EXCEPTION
                    WHEN OTHERS => NULL;
               END TA;

          BEGIN    -- (A1)

               I := C0;  -- INITIALIZE I SO VARIOUS CASES CAN BE
                         -- DETECTED.
               TA.EA (I, I, I);
               FAILED ("EXCEPTION NOT RAISED - A");

          EXCEPTION
               WHEN E =>
                    IF I /= C1 THEN
                         CASE CONVERT (I) IS
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
          END;  -- (A1)

     END;  -- (A)

     ---------------------------------------------------

     DECLARE  -- (B)

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

     BEGIN    -- (B)

          DECLARE  -- (B1)

               I : T;
               E : EXCEPTION;

               TASK TB IS
                    ENTRY EB (EI : IN T; EO : OUT T;
                              EIO : IN OUT T);
               END TB;

               TASK BODY TB IS

                    TEMP : T;

               BEGIN

                    ACCEPT EB (EI : IN T; EO : OUT T;
                               EIO : IN OUT T) DO

                         TEMP := EI;  -- SAVE VALUE OF EI AT ACCEPT.

                         I := C101;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(ACCESS) ACTUAL VARIABLE " &
                                      "CHANGES THE VALUE OF INPUT " &
                                      "PARAMETER");
                              TEMP := EI;   -- RESET TEMP FOR NEXT CASE.
                         END IF;

                         EO := C1;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(ACCESS) OUT PARAMETER " &
                                      "CHANGES THE VALUE OF INPUT " &
                                      "PARAMETER");
                              TEMP := EI;   -- RESET TEMP FOR NEXT CASE.
                         END IF;

                         EIO := C10;
                         IF EI /= TEMP THEN
                              FAILED ("ASSIGNMENT TO PRIVATE " &
                                      "(ACCESS) IN OUT PARAMETER " &
                                      "CHANGES THE VALUE OF INPUT " &
                                      "PARAMETER");
                         END IF;

                         RAISE E;                 -- CHECK EXCEPTION
                                                  -- HANDLING.
                    END EB;

               EXCEPTION
                    WHEN OTHERS => NULL;
               END TB;

          BEGIN     -- (B1)

               I := C100;
               TB.EB (I, I, I);
               FAILED ("EXCEPTION NOT RAISED - B");

          EXCEPTION
               WHEN E =>
                    IF I /= C101 THEN
                         FAILED ("OUT OR IN OUT ACTUAL ENTRY " &
                                 "PARAMETER VALUE CHANGED DESPITE " &
                                 "RAISED EXCEPTION");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - B");
          END;  -- (B1)

     END;  -- (B)

     ---------------------------------------------------

     RESULT;
END C95072B;
