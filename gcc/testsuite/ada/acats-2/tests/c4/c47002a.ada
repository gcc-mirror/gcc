-- C47002A.ADA

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
-- CHECK THAT VALUES BELONGING TO EACH CLASS OF TYPE CAN BE WRITTEN AS 
-- THE OPERANDS OF QUALIFIED EXPRESSIONS.
-- THIS TEST IS FOR DISCRETE TYPES.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47002A IS

BEGIN

     TEST( "C47002A", "CHECK THAT VALUES HAVING DISCRETE TYPES " &
                      "CAN BE WRITTEN AS THE OPERANDS OF " &
                      "QUALIFIED EXPRESSIONS" );

     DECLARE  -- ENUMERATION TYPES.
          
          TYPE WEEK IS (SUN, MON, TUE, WED, THU, FRI, SAT);
          TYPE WEEKEND IS (SAT, SUN);

          TYPE CHAR IS ('B', 'A');

          TYPE MYBOOL IS (TRUE, FALSE);
          
          TYPE NBOOL IS NEW BOOLEAN;

     BEGIN
          IF WEEKEND'(SAT) >= SUN THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE WEEKEND" );
          END IF;

          IF CHAR'('B') >= 'A' THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE CHAR" );
          END IF;

          IF MYBOOL'(TRUE) >= FALSE THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE MYBOOL" );
          END IF;

          IF NBOOL'(TRUE) <= FALSE THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE NBOOL" );
          END IF;
     END;

     DECLARE -- INTEGER TYPES.

          TYPE RESULTS IS (INT1, INT2, INT3);
   
          TYPE NEWINT IS NEW INTEGER;
          
          TYPE INT IS RANGE -10 .. 10;

          FUNCTION F (I : NEWINT) RETURN RESULTS IS
          BEGIN
               RETURN INT1;
          END F;

          FUNCTION F (I : INT) RETURN RESULTS IS
          BEGIN
               RETURN INT2;
          END F;

          FUNCTION F (I : INTEGER) RETURN RESULTS IS
          BEGIN
               RETURN INT3;
          END F;

     BEGIN
          IF F (NEWINT'(5)) /= INT1 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE NEWINT" );
          END IF;

          IF F (INT'(5)) /= INT2 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE INT" );
          END IF;

          IF F (INTEGER'(5)) /= INT3 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE INTEGER" );
          END IF;
     END;

     RESULT;
END C47002A;
