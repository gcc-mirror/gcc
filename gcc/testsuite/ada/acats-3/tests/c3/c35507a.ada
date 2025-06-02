-- C35507A.ADA

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
-- CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS THE CORRECT RESULTS 
-- WHEN THE PREFIX IS A CHARACTER TYPE.   

-- RJW 5/29/86

WITH REPORT; USE REPORT;

PROCEDURE  C35507A  IS

BEGIN

     TEST( "C35507A" , "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
                       "THE CORRECT RESULTS WHEN THE PREFIX " &
                       "IS A CHARACTER TYPE" );

     DECLARE
          TYPE CHAR1 IS (A, 'A');

          SUBTYPE CHAR2 IS CHARACTER RANGE 'A' .. 'Z';

          SUBTYPE NOCHAR IS CHARACTER RANGE 'Z' .. 'A';
          
          TYPE NEWCHAR IS NEW CHARACTER 
                    RANGE 'A' .. 'Z';

     BEGIN
          IF CHAR1'WIDTH /= 3 THEN
               FAILED( "INCORRECT WIDTH FOR CHAR1" );
          END IF;

          IF CHAR2'WIDTH /= 3 THEN
               FAILED( "INCORRECT WIDTH FOR CHAR2" );
          END IF;

          IF NEWCHAR'WIDTH /= 3 THEN
               FAILED( "INCORRECT WIDTH FOR NEWCHAR" );
          END IF;

          IF NOCHAR'WIDTH /= 0 THEN
               FAILED( "INCORRECT WIDTH FOR NOCHAR" );
          END IF;
     END;

     DECLARE
          SUBTYPE NONGRAPH IS CHARACTER 
                    RANGE CHARACTER'VAL (0) .. CHARACTER'VAL (31);

          MAX : INTEGER := 0;

     BEGIN
          FOR CH IN NONGRAPH
               LOOP
                    IF CHARACTER'IMAGE (CH)'LENGTH > MAX THEN
                         MAX := CHARACTER'IMAGE (CH)'LENGTH;
                    END IF;
          END LOOP;
          
          IF NONGRAPH'WIDTH /= MAX THEN
               FAILED ( "INCORRECT WIDTH FOR NONGRAPH" );
          END IF;
     END;                    

     RESULT;
END C35507A;
