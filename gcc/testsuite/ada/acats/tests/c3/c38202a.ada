-- C38202A.ADA

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
-- CHECK THAT TASKING ATTRIBUTES ARE DECLARED AND RETURN CORRECT 
-- VALUES FOR OBJECTS HAVING AN ACCESS TYPE WHOSE DESIGNATED 
-- TYPE IS A TASK TYPE.
-- CHECK THE ACCESS TYPE RESULTS OF FUNCTION CALLS.

-- AH  9/12/86
-- EDS 7/14/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C38202A IS
BEGIN 
     TEST ("C38202A", "OBJECTS HAVING ACCESS TYPES WITH DESIGNATED " &
           "TASK TYPE CAN BE PREFIX OF TASKING ATTRIBUTES");

-- CHECK TWO CASES:  (1)  TASK IS CALLABLE, NOT TERMINATED.
--                   (2)  TASK IS NOT CALLABLE, TERMINATED.

     DECLARE 
          TASK TYPE TSK IS 
               ENTRY GO_ON;
          END TSK;

          TASK DRIVER IS 
               ENTRY TSK_DONE;
          END DRIVER;

          TYPE P_TYPE IS ACCESS TSK;
          P : P_TYPE;

          TASK BODY TSK IS
               I : INTEGER RANGE 0 .. 2;
          BEGIN 
               ACCEPT GO_ON;
               I := IDENT_INT(5);         -- CONSTRAINT_ERROR RAISED.
               FAILED ("CONSTAINT_ERROR NOT RAISED IN TASK " &
                       " TSK - 1A " & INTEGER'IMAGE(I));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    DRIVER.TSK_DONE;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN TASK " &
                            "TSK - 1A ");
                    DRIVER.TSK_DONE;
          END TSK;

          TASK BODY DRIVER IS
               COUNTER : INTEGER := 1;
          BEGIN
               P := NEW TSK;
               IF NOT P'CALLABLE THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE - 1B");
               END IF;

               IF P'TERMINATED THEN 
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE - 1C");
               END IF;

               P.GO_ON;
               ACCEPT TSK_DONE;
               WHILE (NOT P'TERMINATED AND COUNTER <= 3) LOOP
                    DELAY 10.0;
                    COUNTER := COUNTER + 1;
               END LOOP;

               IF COUNTER > 3 THEN
                    FAILED ("TASK TSK NOT TERMINATED IN SUFFICIENT " &
                            "TIME - 1D");
               END IF;

               IF P'CALLABLE THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE - 1E");
               END IF;

               IF NOT P'TERMINATED THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE - 1F");
               END IF;
          END DRIVER;

     BEGIN
          NULL;
     END;     -- BLOCK

-- CHECK ACCESS TYPE RESULT RETURNED FROM FUNCTION.
-- CHECK TWO CASES:  (1)  TASK IS CALLABLE, NOT TERMINATED.
--                   (2)  TASK IS NOT CALLABLE, TERMINATED.

     DECLARE 
          TASK TYPE TSK IS 
               ENTRY GO_ON;
          END TSK;

          TASK DRIVER IS 
               ENTRY TSK_DONE;
          END DRIVER;

          TYPE P_TYPE IS ACCESS TSK;
          P : P_TYPE;

          TSK_CREATED : BOOLEAN := FALSE;

          FUNCTION F1 RETURN P_TYPE IS
          BEGIN
               RETURN P;
          END F1;

          TASK BODY TSK IS
               I : INTEGER RANGE 0 .. 2;
          BEGIN 
               ACCEPT GO_ON;
               I := IDENT_INT(5);          -- CONSTRAINT_ERROR RAISED.
               FAILED ("CONSTRAINT_ERROR NOT RAISED IN TASK " &
                       "TSK - 2A " & INTEGER'IMAGE(I));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    DRIVER.TSK_DONE;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN TASK " &
                            "TSK - 2A ");
                    DRIVER.TSK_DONE;
          END TSK;

          TASK BODY DRIVER IS
               COUNTER : INTEGER := 1;
          BEGIN
               P := NEW TSK;               -- ACTIVATE P.ALL (F1.ALL).
               IF NOT F1'CALLABLE THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE WHEN PREFIX IS VALUE FROM " &
                            "FUNCTION CALL - 2B");
               END IF;

               IF F1'TERMINATED THEN 
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE WHEN PREFIX IS VALUE FROM " &
                            "FUNCTION CALL - 2C");
               END IF;

               F1.ALL.GO_ON;
               ACCEPT TSK_DONE;
               WHILE (NOT F1'TERMINATED AND COUNTER <= 3) LOOP
                    DELAY 10.0;
                    COUNTER := COUNTER + 1;
               END LOOP;

               IF COUNTER > 3 THEN
                    FAILED ("TASK TSK NOT TERMINATED IN SUFFICIENT " &
                            "TIME - 2D");
               END IF;

               IF F1'CALLABLE THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE WHEN PREFIX IS VALUE FROM " &
                            "FUNCTION CALL - 2E");
               END IF;

               IF NOT F1'TERMINATED THEN
                    FAILED ("TASKING ATTRIBUTE RETURNS INCORRECT " &
                            "VALUE WHEN PREFIX IS VALUE FROM " &
                            "FUNCTION CALL - 2F");
               END IF;
          END DRIVER;

     BEGIN
          NULL;
     END;     -- BLOCK

     RESULT;
END C38202A;
