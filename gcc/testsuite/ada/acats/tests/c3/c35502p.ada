-- C35502P.ADA

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
-- OBJECTIVE:
--     FOR AN ENUMERATION TYPE OTHER THAN BOOLEAN OR CHARACTER TYPE,
--     CHECK THAT THE RESULTS AND TYPE PRODUCED BY THE ATTRIBUTES
--     ARE CORRECT.

--     CHECK THAT 'FIRST AND 'LAST YIELD CORRECT RESULTS WHEN THE
--     PREFIX DENOTES A NULL SUBTYPE.

-- HISTORY:
--     RJW 05/05/86  CREATED ORIGINAL TEST.
--     CJJ 06/09/87  CHANGED "=" COMPARISONS IN GENERIC
--                   PROCEDURE Q TO "/=".


WITH REPORT; USE REPORT;

PROCEDURE  C35502P  IS

BEGIN

     TEST( "C35502P" , "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
                       "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A GENERIC FORMAL DISCRETE TYPE " &
                       "WHOSE ACTUAL PARAMETER IS AN ENUMERATION " &
                       "TYPE OTHER THAN A CHARACTER OR A BOOLEAN " &
                       "TYPE" );

     DECLARE
       -- FOR THESE DECLARATIONS, 'FIRST AND 'LAST REFER TO THE
       -- SUBTYPE VALUES, BUT 'VAL AND 'POS ARE INHERITED FROM THE
       -- BASE TYPE.

          TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);
          SUBTYPE SUBENUM IS ENUM RANGE A .. ABC;

          TYPE NEWENUM IS NEW ENUM RANGE BC .. A_B_C;
          TYPE NONEWENUM IS NEW ENUM RANGE ABCD .. A;
          GENERIC
               TYPE E IS (<>);
               F, L : E;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE NOENUM IS E RANGE
                   E'VAL (IDENT_INT(2)) .. E'VAL (IDENT_INT(1));
          BEGIN
               IF E'FIRST /= F THEN
                    FAILED ( "INCORRECT E'FIRST FOR " & STR );
               END IF;
               IF NOENUM'FIRST /= E'VAL (2) THEN
                    FAILED ( "INCORRECT NOENUM'FIRST FOR " & STR );
               END IF;

               IF E'LAST /= L THEN
                    FAILED ( "INCORRECT E'LAST FOR " & STR );
               END IF;
               IF NOENUM'LAST /= E'VAL (1) THEN
                    FAILED ( "INCORRECT NOENUM'LAST FOR " & STR );
               END IF;
          END P;

          GENERIC
               TYPE E IS (<>);
          PROCEDURE Q;

          PROCEDURE Q IS
               SUBTYPE NOENUM IS E RANGE
                       E'VAL (IDENT_INT(2)) .. E'VAL (IDENT_INT(1));
          BEGIN
               IF E'FIRST /= E'VAL (IDENT_INT(4)) THEN
                    FAILED ( "INCORRECT E'FIRST FOR NONEWENUM" );
               END IF;
               IF NOENUM'FIRST /= E'VAL (2) THEN
                    FAILED ( "INCORRECT NOENUM'FIRST FOR NONEWENUM");
               END IF;

               IF E'LAST /= E'VAL (IDENT_INT(0)) THEN
                    FAILED ( "INCORRECT E'LAST FOR NONEWENUM");
               END IF;
               IF NOENUM'LAST /= E'VAL (1) THEN
                    FAILED ( "INCORRECT NOENUM'LAST FOR NONEWENUM");
               END IF;
          END Q;

          PROCEDURE PROC1 IS NEW P (ENUM, A, ABCD);
          PROCEDURE PROC2 IS NEW P (SUBENUM, A, ABC);
          PROCEDURE PROC3 IS NEW P (NEWENUM, BC, A_B_C);
          PROCEDURE PROC4 IS NEW Q (NONEWENUM);

     BEGIN
          PROC1 ( "ENUM" );
          PROC2 ( "SUBENUM" );
          PROC3 ( "NEWENUM" );
          PROC4;
     END;

     RESULT;
END C35502P;
