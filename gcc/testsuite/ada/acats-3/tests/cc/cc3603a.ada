-- CC3603A.ADA

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
--     CHECK THAT ENUMERATION LITERALS (BOTH IDENTIFIERS AND CHARACTER
--     LITERALS) MAY BE PASSED AS ACTUALS CORRESPONDING TO GENERIC
--     FORMAL SUBPROGRAMS.

-- HISTORY:
--     RJW 06/11/86  CREATED ORIGINAL TEST.
--     VCL 08/18/87  CHANGED THE SECOND ACTUAL GENERIC PARAMETER IN THE
--                   INSTANTIATION OF PROCEDURE NP3 TO
--                   'IDENT_CHAR('X')'.

WITH REPORT; USE REPORT;

PROCEDURE CC3603A IS

BEGIN
     TEST ("CC3603A", "CHECK THAT ENUMERATION LITERALS (BOTH " &
                      "IDENTIFIERS AND CHARACTER LITERALS) MAY " &
                      "BE PASSED AS ACTUALS CORRESPONDING TO " &
                      "GENERIC FORMAL SUBPROGRAMS" );

     DECLARE

          TYPE ENUM1 IS ('A', 'B');
          TYPE ENUM2 IS (C, D);

          GENERIC
               TYPE E IS (<>);
               E1 : E;
               WITH FUNCTION F RETURN E;
          PROCEDURE P;

          PROCEDURE P IS
          BEGIN
               IF F /= E1 THEN
                    FAILED ( "WRONG VALUE FOR " & E'IMAGE (E1) &
                             " AS ACTUAL PARAMETER" );
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED INSIDE OF P WITH " &
                              E'IMAGE (E1) &
                             " AS ACTUAL PARAMETER" );
          END P;

          PROCEDURE NP1 IS NEW P (ENUM1, 'A', 'A');
          PROCEDURE NP2 IS NEW P (ENUM2, D, D);
          PROCEDURE NP3 IS NEW P (CHARACTER, IDENT_CHAR('X'), 'X');
     BEGIN
          BEGIN
               NP1;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED WHEN NP1 CALLED" );
          END;

          BEGIN
               NP2;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED WHEN NP2 CALLED" );
          END;

          BEGIN
               NP3;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED WHEN NP3 CALLED" );
          END;
     END;
     RESULT;

END CC3603A;
