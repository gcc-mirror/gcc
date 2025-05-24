-- C35A02A.ADA

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
-- CHECK THAT T'DELTA YIELDS CORRECT VALUES FOR SUBTYPE T.

-- RJW 2/27/86

WITH REPORT; USE REPORT;

PROCEDURE C35A02A IS

BEGIN

     TEST ( "C35A02A", "CHECK THAT T'DELTA YIELDS CORRECT VALUES " &
                       "FOR SUBTYPE T" );
                              
     DECLARE
          D  : CONSTANT := 0.125;
          SD : CONSTANT := 1.0;
     
          TYPE VOLT IS DELTA D RANGE 0.0 .. 255.0;
          SUBTYPE ROUGH_VOLTAGE IS VOLT DELTA SD;

          GENERIC
               TYPE FIXED IS DELTA <> ;
          FUNCTION F RETURN FIXED;

          FUNCTION F RETURN FIXED IS
          BEGIN
               RETURN FIXED'DELTA;
          END F;

          FUNCTION VF IS NEW F (VOLT);
          FUNCTION RF IS NEW F (ROUGH_VOLTAGE);

     BEGIN 
          IF VOLT'DELTA /= D THEN
               FAILED ( "INCORRECT VALUE FOR VOLT'DELTA" );
          END IF;
          IF ROUGH_VOLTAGE'DELTA /= SD THEN 
               FAILED ( "INCORRECT VALUE FOR ROUGH_VOLTAGE'DELTA" );
          END IF;

          IF VF /= D THEN
               FAILED ( "INCORRECT VALUE FOR VF" );
          END IF;
          IF RF /= SD THEN
               FAILED ( "INCORRECT VALUE FOR RF" );
          END IF;
     END;               

     RESULT;

END C35A02A;
