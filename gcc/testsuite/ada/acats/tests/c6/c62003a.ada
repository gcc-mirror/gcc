-- C62003A.ADA

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
-- CHECK THAT SCALAR AND ACCESS PARAMETERS ARE COPIED.
--   SUBTESTS ARE:
--        (A) SCALAR PARAMETERS TO PROCEDURES.
--        (B) SCALAR PARAMETERS TO FUNCTIONS.
--        (C) ACCESS PARAMETERS TO PROCEDURES.
--        (D) ACCESS PARAMETERS TO FUNCTIONS.

-- DAS 01/14/80
-- SPS 10/26/82
-- CPP 05/25/84
-- EG  10/29/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

WITH REPORT;
PROCEDURE C62003A IS

     USE REPORT;

BEGIN
     TEST ("C62003A", "CHECK THAT SCALAR AND ACCESS PARAMETERS ARE " &
                      "COPIED");

     --------------------------------------------------

     DECLARE  -- (A)

          I    : INTEGER;
          E    : EXCEPTION;

          PROCEDURE P (PI : IN INTEGER;  PO : OUT INTEGER;
                       PIO : IN OUT INTEGER) IS

               TMP  : INTEGER;

          BEGIN

               TMP := PI;     -- SAVE VALUE OF PI AT PROC ENTRY.

               PO := 10;
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO SCALAR OUT " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
                    TMP := PI;     -- RESET TMP FOR NEXT CASE.
               END IF;

               PIO := PIO + 100;
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO SCALAR IN OUT " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
                    TMP := PI;     -- RESET TMP FOR NEXT CASE.
               END IF;

               I := I + 1;
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO SCALAR ACTUAL " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
               END IF;

               RAISE E;  -- CHECK EXCEPTION HANDLING.
          END P;

     BEGIN  -- (A)
          I := 0;   -- INITIALIZE I SO VARIOUS CASES CAN BE DETECTED.
          P (I, I, I);
          FAILED ("EXCEPTION NOT RAISED - A");
     EXCEPTION
          WHEN E =>
               IF (I /= 1) THEN
                    CASE I IS
                         WHEN 11  =>
                              FAILED ("OUT ACTUAL SCALAR PARAMETER " &
                                      "CHANGED GLOBAL VALUE");
                         WHEN 101 =>
                              FAILED ("IN OUT ACTUAL SCALAR " &
                                      "PARAMETER CHANGED GLOBAL VALUE");
                         WHEN 111 =>
                              FAILED ("OUT AND IN OUT ACTUAL SCALAR " &
                                      "PARAMETERS CHANGED GLOBAL " &
                                      "VALUE");
                         WHEN OTHERS =>
                              FAILED ("UNDETERMINED CHANGE TO GLOBAL " &
                                      "VALUE");
                    END CASE;
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - A");
     END;  -- (A)

     --------------------------------------------------

     DECLARE  -- (B)

          I,J  : INTEGER;

          FUNCTION F (FI : IN INTEGER) RETURN INTEGER IS

               TMP  : INTEGER := FI;

          BEGIN

               I := I + 1;
               IF (FI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO SCALAR ACTUAL FUNCTION " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
               END IF;

               RETURN (100);
          END F;

     BEGIN  -- (B)
          I := 100;
          J := F(I);
     END;  -- (B)

     --------------------------------------------------

     DECLARE  -- (C)

          TYPE ACCTYPE IS ACCESS INTEGER;

          I    : ACCTYPE;
          E    : EXCEPTION;

          PROCEDURE P (PI : IN ACCTYPE;  PO : OUT ACCTYPE;
                       PIO : IN OUT ACCTYPE) IS

               TMP  : ACCTYPE;

          BEGIN

               TMP := PI;     -- SAVE VALUE OF PI AT PROC ENTRY.

               I := NEW INTEGER'(101);
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO ACCESS ACTUAL " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
                    TMP := PI;     -- RESET TMP FOR NEXT CASE.
               END IF;

               PO := NEW INTEGER'(1);
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO ACCESS OUT " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
                    TMP := PI;     -- RESET TMP FOR NEXT CASE.
               END IF;

               PIO := NEW INTEGER'(10);
               IF (PI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO ACCESS IN OUT " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
               END IF;

               RAISE E;  -- CHECK EXCEPTION HANDLING.
          END P;

     BEGIN  -- (C)
          I := NEW INTEGER'(100);
          P (I, I, I);
          FAILED ("EXCEPTION NOT RAISED - C");
     EXCEPTION
          WHEN E =>
               IF (I.ALL /= 101) THEN
                    FAILED ("OUT OR IN OUT ACTUAL PROCEDURE " &
                            "PARAMETER VALUE CHANGED DESPITE " &
                            "RAISED EXCEPTION");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - C");
     END;  -- (C)

     --------------------------------------------------

     DECLARE  -- (D)

          TYPE ACCTYPE IS ACCESS INTEGER;

          I,J  : ACCTYPE;

          FUNCTION F (FI : IN ACCTYPE) RETURN ACCTYPE IS

               TMP  : ACCTYPE := FI;

          BEGIN

               I := NEW INTEGER;
               IF (FI /= TMP) THEN
                    FAILED ("ASSIGNMENT TO ACCESS ACTUAL FUNCTION " &
                            "PARAMETER CHANGES THE VALUE OF " &
                            "INPUT PARAMETER");
               END IF;

               RETURN (NULL);
          END F;

     BEGIN  -- (D)
          I := NULL;
          J := F(I);
     END;  -- (D)

     --------------------------------------------------

     RESULT;

END C62003A;
