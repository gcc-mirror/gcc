-- C47002B.ADA

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
-- THIS TEST IS FOR REAL TYPES.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47002B IS

BEGIN

     TEST( "C47002B", "CHECK THAT VALUES HAVING REAL TYPES " &
                      "CAN BE WRITTEN AS THE OPERANDS OF " &
                      "QUALIFIED EXPRESSIONS" );

     DECLARE -- FLOATING POINT TYPES.
     
          TYPE RESULTS IS (FL1, FL2, FL3);

          TYPE FLT IS DIGITS 3 RANGE -5.0 .. 5.0;

          TYPE NFLT IS NEW FLOAT;

          FUNCTION F (FL : FLT) RETURN RESULTS IS 
          BEGIN
               RETURN FL1;
          END F;

          FUNCTION F (FL : NFLT) RETURN RESULTS IS
          BEGIN
               RETURN FL2;
          END F;

          FUNCTION F (FL : FLOAT) RETURN RESULTS IS
          BEGIN
               RETURN FL3;
          END F;

     BEGIN
          IF F (FLT'(0.0)) /= FL1 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE FLT" );
          END IF;

          IF F (NFLT'(0.0)) /= FL2 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE NFLT" );
          END IF;

          IF F (FLOAT'(0.0)) /= FL3 THEN 
               FAILED ( "INCORRECT RESULTS FOR TYPE FLOAT" );
          END IF;
     END;   
     
     DECLARE -- FIXED POINT TYPES.
     
          TYPE RESULTS IS (FI1, FI2, FI3);

          TYPE FIXED IS DELTA 0.5 RANGE -5.0 .. 5.0;

          TYPE NFIX IS NEW FIXED;

          FUNCTION F (FI : FIXED) RETURN RESULTS IS 
          BEGIN
               RETURN FI1;
          END F;

          FUNCTION F (FI : NFIX) RETURN RESULTS IS
          BEGIN
               RETURN FI2;
          END F;

          FUNCTION F (FI : DURATION) RETURN RESULTS IS
          BEGIN
               RETURN FI3;
          END F;

     BEGIN
          IF F (FIXED'(0.0)) /= FI1 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE FIXED" );
          END IF;

          IF F (NFIX'(0.0)) /= FI2 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE NFIX" );
          END IF;

          IF F (DURATION'(0.0)) /= FI3 THEN
               FAILED ( "INCORRECT RESULTS FOR TYPE DURATION" );
          END IF;
     END;   
     
     RESULT;
END C47002B;
