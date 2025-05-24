-- C35508B.ADA

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
-- THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL 
-- PARAMETER IS A BOOLEAN TYPE.   

-- RJW 3/19/86  COMPLETELY REVISED.

WITH REPORT; USE REPORT;

PROCEDURE  C35508B  IS

BEGIN

     TEST( "C35508B" , "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
                       "THE CORRECT RESULTS WHEN THE PREFIX IS A " &
                       "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
                       "PARAMETER IS A BOOLEAN TYPE" );

     DECLARE
          SUBTYPE FRANGE IS BOOLEAN 
               RANGE IDENT_BOOL(FALSE) .. IDENT_BOOL(FALSE);
          SUBTYPE TRANGE IS BOOLEAN 
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(TRUE);
          TYPE NEWBOOL IS NEW BOOLEAN;

          GENERIC
               TYPE B IS (<>);
               W : INTEGER;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE NOBOOL IS B RANGE 
                    B'VAL (IDENT_INT(1)) .. B'VAL (IDENT_INT(0));
          BEGIN
               IF B'WIDTH /= W THEN 
                    FAILED ( "INCORRECT B'WIDTH FOR " & STR );
               END IF;
               IF NOBOOL'WIDTH /= 0 THEN
                    FAILED ( "INCORRECT NOBOOL'WIDTH FOR " & STR );
               END IF;
          END P;

          PROCEDURE PROC1 IS NEW P (BOOLEAN, 5);
          PROCEDURE PROC2 IS NEW P (FRANGE, 5);
          PROCEDURE PROC3 IS NEW P (TRANGE, 4);
          PROCEDURE PROC4 IS NEW P (NEWBOOL, 5);

     BEGIN
          PROC1 ( "BOOLEAN" );
          PROC2 ( "FRANGE" );
          PROC3 ( "TRANGE");
          PROC4 ( "NEWBOOL" );
     END;                    

     RESULT;
END C35508B;
