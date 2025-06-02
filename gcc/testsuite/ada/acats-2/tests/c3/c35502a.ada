-- C35502A.ADA

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
-- WHEN THE PREFIX IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR 
-- A CHARACTER TYPE.   

-- RJW 5/05/86

WITH REPORT; USE REPORT;

PROCEDURE  C35502A  IS

BEGIN

     TEST( "C35502A" , "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
                       "THE CORRECT RESULTS WHEN THE PREFIX " &
                       "IS AN ENUMERATION TYPE OTHER THAN " &
                       "A BOOLEAN OR A CHARACTER TYPE" );

     DECLARE
          TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);

          SUBTYPE SUBENUM IS ENUM RANGE A .. ABC;
          SUBTYPE NOENUM IS ENUM RANGE ABC .. A;
          
          TYPE NEWENUM IS NEW ENUM;
         
     BEGIN

          IF ENUM'WIDTH /= IDENT_INT(5) THEN
               FAILED( "INCORRECT WIDTH FOR ENUM" );
          END IF;

          IF NEWENUM'WIDTH /= IDENT_INT(5) THEN
               FAILED( "INCORRECT WIDTH FOR NEWENUM" );
          END IF;

          IF SUBENUM'WIDTH /= IDENT_INT(3) THEN
               FAILED( "INCORRECT WIDTH FOR SUBENUM" );
          END IF;

          IF NOENUM'WIDTH /= IDENT_INT(0) THEN
               FAILED( "INCORRECT WIDTH FOR NOENUM" );
          END IF;

     END;                    

     RESULT;
END C35502A;
