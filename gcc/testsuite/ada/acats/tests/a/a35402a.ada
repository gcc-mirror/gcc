-- A35402A.ADA

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
-- CHECK THAT THE BOUNDS OF AN INTEGER TYPE DEFINITION NEED NOT 
-- HAVE THE SAME INTEGER TYPE.

-- RJW 2/20/86

WITH REPORT; USE REPORT;

PROCEDURE A35402A IS

BEGIN

     TEST ( "A35402A", "CHECK THAT THE BOUNDS OF AN INTEGER " &
                       "TYPE DEFINITION NEED NOT HAVE THE SAME " &
                       "INTEGER TYPE" );

     DECLARE     
          TYPE INT1 IS RANGE 1 .. 10;
          TYPE INT2 IS RANGE 2 .. 8;
          TYPE INT3 IS NEW INTEGER;
     
          I  : CONSTANT INTEGER   := 5;
          I1 : CONSTANT INT1      := 5;
          I2 : CONSTANT INT2      := 5;
          I3 : CONSTANT INT3      := 5;
     
          TYPE INTRANGE1 IS RANGE I  .. I1;          -- OK.

          TYPE INTRANGE2 IS RANGE I1 .. I2;          -- OK.

          TYPE INTRANGE3 IS RANGE I2 .. I3;          -- OK.
          
          TYPE INTRANGE4 IS RANGE I3 .. I;           -- OK.
     BEGIN
          NULL;
     END;

     RESULT;

END A35402A;
