-- C46051C.ADA

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
--     CHECK THAT RECORD VALUES CAN BE CONVERTED IF THE OPERAND
--     AND TARGET TYPES ARE RELATED BY DERIVATION, EVEN IF THE OPERAND
--     AND TARGET TYPES HAVE DIFFERENT REPRESENTATIONS.

-- HISTORY:
--     JET 07/13/88  CREATED ORIGINAL TEST.
--     RJW 08/28/89  REMOVED APPLICABILITY CRITERIA AND CHANGED
--                   EXTENSION TO 'ADA'.

WITH REPORT; USE REPORT;
WITH SYSTEM;

PROCEDURE C46051C IS

     UNITS_PER_INTEGER : CONSTANT :=
          (INTEGER'SIZE+SYSTEM.STORAGE_UNIT-1) / SYSTEM.STORAGE_UNIT;

     TYPE ARR IS ARRAY (1..2) OF INTEGER;

     TYPE REC IS RECORD
          F1 : INTEGER;
          F2 : INTEGER;
          F3 : INTEGER;
     END RECORD;

     TYPE REC1 IS NEW REC;
     FOR REC1 USE
          RECORD
               F1 AT 0 RANGE 0 .. INTEGER'SIZE - 1;
               F2 AT 1*UNITS_PER_INTEGER RANGE 0..INTEGER'SIZE - 1;
               F3 AT 3*UNITS_PER_INTEGER RANGE 0..INTEGER'SIZE - 1;
          END RECORD;

     TYPE REC2 IS NEW REC;
     FOR REC2 USE
          RECORD
               F1 AT 0 RANGE 0 .. INTEGER'SIZE - 1;
               F2 AT 2*UNITS_PER_INTEGER RANGE 0..INTEGER'SIZE - 1;
               F3 AT 3*UNITS_PER_INTEGER RANGE 0..INTEGER'SIZE - 1;
          END RECORD;

     TYPE REC3 IS NEW REC1;

     R  : REC  := (IDENT_INT (0), 1, 2);
     R1 : REC1 := (IDENT_INT (1), 2, 3);
     R2 : REC2 := (IDENT_INT (2), 3, 4);
     R3 : REC3 := (IDENT_INT (3), 4, 5);

BEGIN
     TEST ( "C46051C", "CHECK THAT RECORD VALUES CAN BE " &
                       "CONVERTED IF THE OPERAND AND TARGET TYPES " &
                       "ARE RELATED BY DERIVATION, EVEN IF THE " &
                       "OPERAND AND TARGET TYPES HAVE DIFFERENT " &
                       "REPRESENTATIONS");

     IF REC1(R) /= (0,1,2) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC1 (R)'" );
     END IF;

     IF REC (R1) /= (1,2,3) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC (R1)'" );
     END IF;

     IF REC1 (R2) /= (2,3,4) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC1 (R2)'" );
     END IF;

     IF REC2 (R3) /= (3,4,5) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC2 (R3)'" );
     END IF;

     IF REC (R) /= (0,1,2) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC (R)'" );
     END IF;

     IF REC2 (R1) /= (1,2,3) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC2 (R1)'" );
     END IF;

     IF REC3 (R2) /= (2,3,4) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC3 (R2)'" );
     END IF;

     IF REC (R3) /= (3,4,5) THEN
          FAILED ( "INCORRECT CONVERSION OF 'REC (R3)'" );
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                   "RECORD TYPES" );
          RESULT;
END C46051C;
