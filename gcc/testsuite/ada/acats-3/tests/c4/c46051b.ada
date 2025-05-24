-- C46051B.ADA

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
--     CHECK THAT ENUMERATION VALUES CAN BE CONVERTED IF THE OPERAND
--     AND TARGET TYPES ARE RELATED BY DERIVATION, EVEN IF THE OPERAND
--     AND TARGET TYPES HAVE DIFFERENT REPRESENTATIONS.

-- HISTORY:
--     JET 07/13/88  CREATED ORIGINAL TEST.
--     RJW 08/28/89  REMOVED APPLICABILITY CRITERIA AND CHANGED
--                   EXTENSION TO 'ADA'.  CHANGED THE CODES IN SECOND
--                   ENUMERATION REPRESENTATION CLAUSE.

WITH REPORT; USE REPORT;
PROCEDURE C46051B IS

     TYPE ENUM IS (WE, LOVE, WRITING, TESTS);

     TYPE ENUM1 IS NEW ENUM;
     FOR ENUM1 USE
          (WE => -1, LOVE => 0, WRITING => 3, TESTS => 9);

     TYPE ENUM2 IS NEW ENUM;
     FOR ENUM2 USE
          (WE => 10, LOVE => 15, WRITING => 16, TESTS => 19);

     TYPE ENUM3 IS NEW ENUM1;

     E : ENUM := ENUM'VAL (IDENT_INT (0));
     E1 : ENUM1 := ENUM1'VAL (IDENT_INT (1));
     E2 : ENUM2 := ENUM2'VAL (IDENT_INT (2));
     E3 : ENUM3 := ENUM3'VAL (IDENT_INT (3));

BEGIN
     TEST ( "C46051B", "CHECK THAT ENUMERATION VALUES CAN BE " &
                       "CONVERTED IF THE OPERAND AND TARGET TYPES " &
                       "ARE RELATED BY DERIVATION, EVEN IF THE " &
                       "OPERAND AND TARGET TYPES HAVE DIFFERENT " &
                       "REPRESENTATIONS");

     IF ENUM1 (E) /= WE THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM1 (E)'" );
     END IF;

     IF ENUM (E1) /= LOVE THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM (E1)'" );
     END IF;

     IF ENUM1 (E2) /= WRITING THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM1 (E2)'" );
     END IF;

     IF ENUM2 (E3) /= TESTS THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM2 (E3)'" );
     END IF;

     IF ENUM (E) /= WE THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM (E)'" );
     END IF;

     IF ENUM2 (E1) /= LOVE THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM2 (E1)'" );
     END IF;

     IF ENUM3 (E2) /= WRITING THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM3 (E2)'" );
     END IF;

     IF ENUM (E3) /= TESTS THEN
          FAILED ( "INCORRECT CONVERSION OF 'ENUM (E3)'" );
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ( "EXCEPTION RAISED DURING CONVERSION OF " &
                   "ENUMERATION TYPES" );
          RESULT;
END C46051B;
