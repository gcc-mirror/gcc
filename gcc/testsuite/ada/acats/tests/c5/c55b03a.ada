-- C55B03A.ADA

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
-- CHECK THAT THE LOOP_PARAMETER IS ASSIGNED VALUES IN ASCENDING ORDER 
--   IF REVERSE IS ABSENT, AND DESCENDING ORDER IF REVERSE IS PRESENT.

-- DAS 1/12/81
-- SPS 3/2/83

WITH REPORT;
PROCEDURE C55B03A IS

     USE REPORT;
     I1 : INTEGER;

BEGIN
     TEST( "C55B03A" , "CHECK CORRECT ORDER OF VALUE SEQUENCING" & 
           " FOR A LOOP_PARAMETER" );

     I1 := 0;
     FOR I IN IDENT_INT(1)..IDENT_INT(5) LOOP
          I1 := I1 + 1;
          IF ( I /= I1 ) THEN
               FAILED ( "LOOP_PARAMETER ASCENDING INCORRECTLY" );
          END IF;
     END LOOP;

     I1 := 6;
     FOR I IN REVERSE IDENT_INT(1)..IDENT_INT(5) LOOP
          I1 := I1 - 1;
          IF ( I /= I1 ) THEN
               FAILED ( "LOOP_PARAMETER DESCENDING INCORRECTLY" );
          END IF;
     END LOOP;

     RESULT;

END C55B03A;
