-- C46011A.ADA

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
-- CHECK THAT INTEGER CONVERSIONS ARE PERFORMED CORRECTLY WHEN THE 
-- TARGET AND OPERAND TYPES ARE BOTH INTEGER TYPES.

-- R.WILLIAMS 9/8/86

WITH REPORT; USE REPORT;
PROCEDURE C46011A IS

     TYPE INT1 IS RANGE -100 .. 100;
     I1 : INT1 := INT1'VAL (IDENT_INT (10));
     F1 : INT1 := INT1'VAL (IDENT_INT (-100));
     L1 : INT1 := INT1'VAL (IDENT_INT (100));

     TYPE INT2 IS RANGE -100 .. 100;
     I2 : INT2 := INT2'VAL (IDENT_INT (10));
     F2 : INT2 := INT2'VAL (IDENT_INT (-100));
     L2 : INT2 := INT2'VAL (IDENT_INT (100));

     
     TYPE NEWINTEGER IS NEW INTEGER;
     N1 : NEWINTEGER := 
          NEWINTEGER'VAL (IDENT_INT (10));

     T1 : INTEGER := IDENT_INT (10);

     U1 : CONSTANT := INTEGER'POS (10);
BEGIN
     TEST ( "C46011A", "CHECK THAT INTEGER CONVERSIONS ARE " &
                       "PERFORMED CORRECTLY WHEN THE TARGET AND " &
                       "OPERAND TYPES ARE BOTH INTEGER TYPES" );
     
     IF INT1 (U1) /= U1 THEN 
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (U1)'" );
     END IF;

     IF INT1 (I1) /= I1 THEN 
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (I1)'" );
     END IF;

     IF INT1 (N1) /= I1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (N1)'" );
     END IF;

     IF INT1 (10) /= I1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (10)'" );
     END IF;

     IF INT1 (T1) /= I1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (T1)'" );
     END IF;

     IF INT1 (F2) /= F1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (F2)'" );
     END IF;

     IF INT1 (L2) /= L1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT1 (L2)'" );
     END IF;

     IF INT2 (I1) /= I2 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT2 (I1)'" );
     END IF;

     IF INT2 (T1) /= 10 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT2 (T1)'" );
     END IF;

     IF INT2 (F1) /= -100 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT2 (F1)'" );
     END IF;

     IF INT2 (L1) /= 100 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INT2 (L1)'" );
     END IF;

     IF NEWINTEGER (I1) /= N1 THEN 
          FAILED ( "INCORRECT CONVERSION OF 'NEWINTEGER (I1)'" );
     END IF;

     IF NEWINTEGER (N1) /= N1 THEN 
          FAILED ( "INCORRECT CONVERSION OF 'NEWINTEGER (N1)'" );
     END IF;

     IF NEWINTEGER (T1) /= N1 THEN 
          FAILED ( "INCORRECT CONVERSION OF 'NEWINTEGER (T1)'" );
     END IF;

     IF NEWINTEGER (INTEGER (N1)) /= N1 THEN 
          FAILED ( "INCORRECT CONVERSION OF " &
                   "'NEWINTEGER (INTEGER (N1))'" );
     END IF;

     IF NEWINTEGER (INTEGER (N1 + 1)) /= 11 THEN 
          FAILED ( "INCORRECT CONVERSION OF " &
                   "'NEWINTEGER (INTEGER (N1 + 1))'" );
     END IF;

     IF INTEGER (10) /= T1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INTEGER (10)'" );
     END IF;

     IF INTEGER (N1) /= 10 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INTEGER (N1)'" );
     END IF;
     
     IF INTEGER (I1) /= T1 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INTEGER (I1)'" );
     END IF;     

     IF INTEGER (INT1 (NEWINTEGER (INT1 (I1)))) /= T1 THEN
          FAILED ( "INCORRECT CONVERSION OF " &
                   "'INTEGER (INT1 (NEWINTEGER (INT1 (I1)))'" );
     END IF;     

     
     IF INTEGER (I1 + 1) /= 11 THEN
          FAILED ( "INCORRECT CONVERSION OF 'INTEGER (I1 + 1)'" );
     END IF;     

     RESULT;
END C46011A;
