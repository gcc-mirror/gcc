-- C35508P.ADA

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
--     CHECK THAT 'FIRST' AND 'LAST' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER
--     IS A BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35508P IS

BEGIN
     TEST ("C35508P", "CHECK THAT 'FIRST' AND 'LAST' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
                      "PARAMETER IS A BOOLEAN TYPE" );
     DECLARE
          SUBTYPE TBOOL IS BOOLEAN
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(TRUE);
          SUBTYPE FBOOL IS BOOLEAN
               RANGE IDENT_BOOL(FALSE) .. IDENT_BOOL(FALSE);
          SUBTYPE NOBOOL IS BOOLEAN
               RANGE IDENT_BOOL(TRUE) .. IDENT_BOOL(FALSE);
          TYPE NEWBOOL IS NEW BOOLEAN;

          GENERIC
               TYPE BOOL IS (<>);
               F, L : BOOL;
          PROCEDURE P ( STR : STRING );

          PROCEDURE P ( STR : STRING ) IS
          BEGIN
               IF BOOL'FIRST /= F THEN
                    FAILED ( "WRONG VALUE FOR " & STR & "'FIRST" );
               END IF;
               IF BOOL'LAST /= L THEN
                    FAILED ( "WRONG VALUE FOR " & STR & "'LAST" );
               END IF;
          END P;

          GENERIC
               TYPE BOOL IS (<>);
          PROCEDURE Q;

          PROCEDURE Q IS
          BEGIN
               IF BOOL'FIRST /= BOOL'VAL (IDENT_INT(1)) THEN
                    FAILED ( "WRONG 'FIRST FOR NOBOOL" );
               END IF;
               IF BOOL'LAST /= BOOL'VAL (IDENT_INT(0)) THEN
                    FAILED ( "WRONG 'LAST FOR NOBOOL" );
               END IF;
          END Q;

          GENERIC
               TYPE BOOL IS (<>);
               F, L : BOOL;
          PROCEDURE R;

          PROCEDURE R IS
               SUBTYPE SBOOL IS BOOL
                              RANGE BOOL'VAL (0) .. BOOL'VAL (1);
          BEGIN
               IF SBOOL'FIRST /= F THEN
                    FAILED ( "WRONG VALUE FOR BOOLEAN'FIRST AS " &
                             "SUBTYPE " );
               END IF;
               IF SBOOL'LAST /= L THEN
                    FAILED ( "WRONG VALUE FOR BOOLEAN'LAST AS " &
                             "SUBTYPE" );
               END IF;
          END R;

          PROCEDURE P1 IS NEW P
          ( BOOL => BOOLEAN, F => IDENT_BOOL(FALSE),
                                              L => IDENT_BOOL(TRUE) );

          PROCEDURE P2 IS NEW P
          ( BOOL => TBOOL, F => IDENT_BOOL(TRUE),
                                              L => IDENT_BOOL(TRUE) );

          PROCEDURE P3 IS NEW P
          ( BOOL => FBOOL, F => IDENT_BOOL(FALSE),
                                             L => IDENT_BOOL(FALSE) );

          PROCEDURE P4 IS NEW P
                         (BOOL => NEWBOOL, F => FALSE, L => TRUE );

          PROCEDURE Q1 IS NEW Q
                         ( BOOL => NOBOOL );

          PROCEDURE R1 IS NEW R
                         ( BOOL => BOOLEAN, F => FALSE, L => TRUE );

     BEGIN
          P1 ( "BOOLEAN" );
          P2 ( "TBOOL" );
          P3 ( "FBOOL" );
          P4 ( "NEWBOOL" );
          Q1;
          R1;
     END;

     RESULT;
END C35508P;
