-- C62004A.ADA

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
-- CHECK THAT ALIASING IS PERMITTED FOR PARAMETERS OF COMPOSITE TYPES,
--   E.G., THAT A MATRIX ADDITION PROCEDURE CAN BE CALLED WITH THREE
--   IDENTICAL ARGUMENTS.  (NOTE: ALIASING MAY NOT WORK FOR ARGUMENTS
--   TO ALL SUBROUTINES SINCE PARAMETER PASSING IS IMPLEMENTATION
--   DEPENDENT.  HOWEVER, THIS TEST IS NOT ERRONEOUS.)

-- DAS  1/26/81

WITH REPORT;
PROCEDURE C62004A IS

     USE REPORT;

     TYPE MATRIX IS ARRAY (1..3,1..3) OF INTEGER;

     A    : MATRIX  := ((1,2,3),(4,5,6),(7,8,9));

     PROCEDURE MAT_ADD (X,Y : IN MATRIX; SUM : OUT MATRIX) IS
     BEGIN
          FOR I IN 1..3 LOOP
               FOR J IN 1..3 LOOP
                    SUM(I,J) := X(I,J) + Y(I,J);
               END LOOP;
          END LOOP;
     END MAT_ADD;

BEGIN

     TEST ("C62004A", "CHECK THAT ALIASING IS PERMITTED FOR" &
                      " PARAMETERS OF COMPOSITE TYPES");

     MAT_ADD (A, A, A);

     IF (A /= ((2,4,6),(8,10,12),(14,16,18))) THEN
          FAILED ("THE RESULT OF THE MATRIX ADDITION IS INCORRECT");
     END IF;

     RESULT;

END C62004A;
