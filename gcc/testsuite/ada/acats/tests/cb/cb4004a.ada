-- CB4004A.ADA

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
-- CHECK THAT VARIOUS EXCEPTIONS IN THE BODY OF A SUBPROGRAM WITH
-- AN APPLICABLE HANDLER ARE HANDLED LOCALLY.

-- DAT 04/15/81
-- JRK 04/24/81
-- SPS 11/02/82
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

WITH REPORT; USE REPORT;

PROCEDURE CB4004A IS

     E, F : EXCEPTION;
     STORAGE_ERROR: EXCEPTION;

     I1 : INTEGER RANGE 1 .. 1;

     FUNCTION F1 (I : INTEGER) RETURN BOOLEAN IS
     BEGIN
          CASE I IS
               WHEN 1 => RAISE E;
               WHEN 2 => RAISE STORAGE_ERROR;
               WHEN 3 => I1 := 4;
               WHEN 4 => RAISE TASKING_ERROR;
               WHEN OTHERS => NULL;
          END CASE;
          RETURN FALSE;
     EXCEPTION
          WHEN E | F => RETURN I = 1;
          WHEN STORAGE_ERROR => RETURN I = 2;
          WHEN PROGRAM_ERROR | CONSTRAINT_ERROR =>
               RETURN I = 3;
          WHEN OTHERS => RETURN I = 4;
     END F1;

BEGIN
     TEST ("CB4004A", "EXCEPTIONS WITH LOCAL HANDLERS ARE HANDLED"
          & " THERE");

     BEGIN
          FOR L IN 1 .. 4 LOOP
               IF F1(L) /= TRUE THEN
                    FAILED ("LOCAL EXCEPTIONS DON'T WORK");
                    EXIT;
               END IF;
          END LOOP;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("WRONG HANDLER");
     END;

     RESULT;
END CB4004A;
