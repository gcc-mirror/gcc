-- C35508A.ADA

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
-- CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS THE CORRECT RESULTS WHEN 
-- THE PREFIX IS A BOOLEAN TYPE.   

-- RJW 3/14/86  COMPLETELY REVISED.

WITH REPORT; USE REPORT;

PROCEDURE  C35508A  IS

BEGIN

     TEST( "C35508A" , "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
                       "THE CORRECT RESULTS WHEN THE PREFIX IS A " &
                       "BOOLEAN TYPE" );

     DECLARE
          TYPE NEWBOOL IS NEW BOOLEAN;
          SUBTYPE FRANGE IS BOOLEAN 
               RANGE IDENT_BOOL(FALSE) .. IDENT_BOOL(FALSE);
          SUBTYPE TRANGE IS BOOLEAN 
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(TRUE);
          SUBTYPE NOBOOL IS BOOLEAN 
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(FALSE);

     BEGIN

          IF BOOLEAN'WIDTH /= 5 THEN
               FAILED( "INCORRECT WIDTH FOR BOOLEAN" );
          END IF;

          IF NEWBOOL'WIDTH /= 5 THEN
               FAILED( "INCORRECT WIDTH FOR NEWBOOL" );
          END IF;

          IF FRANGE'WIDTH /= 5 THEN
               FAILED( "INCORRECT WIDTH FOR FRANGE" );
          END IF;

          IF TRANGE'WIDTH /= 4 THEN
               FAILED( "INCORRECT WIDTH FOR TRANGE" );
          END IF;

          IF NOBOOL'WIDTH /= 0 THEN
               FAILED( "INCORRECT WIDTH FOR NOBOOL" );
          END IF;
     
     END;                    

     RESULT;
END C35508A;
