-- C35502B.ADA

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
-- WHEN THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL 
-- PARAMETER IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR CHARACTER 
-- TYPE.   

-- RJW 5/05/86

WITH REPORT; USE REPORT;

PROCEDURE  C35502B  IS

BEGIN

     TEST( "C35502B" , "CHECK THAT THE ATTRIBUTE 'WIDTH' YIELDS " &
                       "THE CORRECT RESULTS WHEN THE PREFIX " &
                       "IS A GENERIC FORMAL DISCRETE TYPE " &
                       "WHOSE ACTUAL PARAMETER IS AN ENUMERATION " &
                       "TYPE" );

     DECLARE
          TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);
          SUBTYPE SUBENUM IS ENUM RANGE A .. ABC;
          SUBTYPE NOENUM IS ENUM RANGE ABC .. A;
          
          TYPE NEWENUM IS NEW ENUM;

          GENERIC
               TYPE E IS (<>);
               W : INTEGER;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE NOENUM IS E RANGE 
                    E'VAL (IDENT_INT(2)) .. E'VAL (IDENT_INT(1));
          BEGIN
               IF E'WIDTH /= IDENT_INT(W) THEN 
                    FAILED ( "INCORRECT E'WIDTH FOR " & STR );
               END IF;
               IF NOENUM'WIDTH /= IDENT_INT(0) THEN
                    FAILED ( "INCORRECT NOENUM'WIDTH FOR " & STR );
               END IF;
          END P;

          PROCEDURE PROC1 IS NEW P (ENUM, 5);
          PROCEDURE PROC2 IS NEW P (SUBENUM, 3);
          PROCEDURE PROC3 IS NEW P (NEWENUM, 5);
          PROCEDURE PROC4 IS NEW P (NOENUM, 0);

     BEGIN
          PROC1 ( "ENUM" );
          PROC2 ( "SUBENUM" );
          PROC3 ( "NEWENUM" );
          PROC4 ( "NOENUM" );
     END;                    

     RESULT;
END C35502B;
