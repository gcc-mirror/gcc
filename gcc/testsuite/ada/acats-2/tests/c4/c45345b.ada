-- C45345B.ADA

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
-- CHECK THAT  CONSTRAINT_ERROR  IS NOT RAISED IF THE RESULT OF
--     CATENATION HAS PRECISELY THE MAXIMUM LENGTH PERMITTED BY THE
--     INDEX SUBTYPE.


-- RM  2/26/82


WITH REPORT;
USE REPORT;
PROCEDURE C45345B IS


BEGIN

     TEST ( "C45345B" , "CHECK THAT  CONSTRAINT_ERROR  IS NOT RAISED" &
                        " IF THE RESULT OF CATENATION HAS PRECISELY" &
                        " THE MAXIMUM LENGTH PERMITTED BY THE" &
                        " INDEX SUBTYPE" );


     -------------------------------------------------------------------
     -----------------  STRG_VAR := STRG_LIT & STRG_LIT  ---------------

     DECLARE

          X : STRING(1..5) ;

     BEGIN

          X := "ABCD" & "E" ;

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>
               FAILED( "'STRING & STRING' RAISED  CONSTRAINT_ERROR " );

          WHEN  OTHERS =>
               FAILED( "'STRING & STRING' RAISED ANOTHER EXCEPTION" );

     END;


     -------------------------------------------------------------------
     -----------------  STRG_VAR := STRG_LIT & CHARACTER  --------------

     DECLARE

          X : STRING(1..5) ;

     BEGIN

          X := "ABCD" & 'E' ;

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>
               FAILED( "'STRING & STRING' RAISED  CONSTRAINT_ERROR " );

          WHEN  OTHERS =>
               FAILED( "'STRING & STRING' RAISED ANOTHER EXCEPTION" );

     END;

     -------------------------------------------------------------------
     -----------------  STRG_VAR := STRG_VAR & STRG_VAR  ---------------

     DECLARE

          X :          STRING(1..5) ;
          A : CONSTANT STRING       := "A" ;
          B :          STRING(1..4) := IDENT_STR("BCDE") ;

     BEGIN

          X :=  A & B ;

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>
               FAILED( "'STRING & STRING' RAISED  CONSTRAINT_ERROR " );

          WHEN  OTHERS =>
               FAILED( "'STRING & STRING' RAISED ANOTHER EXCEPTION" );

     END;

     -------------------------------------------------------------------


     RESULT;


END C45345B;
