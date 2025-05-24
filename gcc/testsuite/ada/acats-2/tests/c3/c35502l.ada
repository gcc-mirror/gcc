-- C35502L.ADA

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
-- CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN 
-- THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT IS
-- AN ENUMERATION TYPE OTHER THAN  A BOOLEAN OR A CHARACTER TYPE.

-- RJW 5/27/86

WITH REPORT; USE REPORT;

PROCEDURE C35502L IS
     
     TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);
     SUBTYPE SUBENUM IS ENUM RANGE A .. BC;

     TYPE NEWENUM IS NEW ENUM;
     SUBTYPE SUBNEW IS NEWENUM RANGE A .. BC;

BEGIN
     TEST ("C35502L", "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT " &
                      "IS AN ENUMERATION TYPE OTHER THAN A " &
                      "CHARACTER OR A BOOLEAN TYPE" );

     DECLARE

          GENERIC
               TYPE E IS (<>);
               STR : STRING;
          PROCEDURE P;
          
          PROCEDURE P IS
               SUBTYPE SE IS E RANGE E'VAL(0) .. E'VAL(1);
               POSITION : INTEGER;
          BEGIN
     
               POSITION := 0;

               FOR E1 IN E
                    LOOP
                         IF SE'POS (E1) /= POSITION THEN
                              FAILED ( "INCORRECT SE'POS (" &
                                        E'IMAGE (E1) & ")" );
                         END IF;
                    
                         IF SE'VAL (POSITION) /= E1 THEN 
                              FAILED ( "INCORRECT " & STR & "'VAL (" &
                                        INTEGER'IMAGE (POSITION) & 
                                       ")" );
                         END IF;
     
                         POSITION := POSITION + 1;
                    END LOOP;
          
               BEGIN
                    IF E'VAL (-1) = E'VAL (1) THEN
                         FAILED ( "NO EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (-1) - 1" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (-1) - 2" );
                    END IF;
               EXCEPTION
                     WHEN CONSTRAINT_ERROR =>
                         NULL;
               WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (-1)" );
               END;

               BEGIN
                    IF E'VAL (5) = E'VAL (4) THEN
                         FAILED ( "NO EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (5) - 1" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (5) - 2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " & 
                                   STR & "'VAL (5)" );
               END;
          END P;
     
          PROCEDURE PE IS NEW P ( ENUM, "ENUM" );
          PROCEDURE PN IS NEW P ( NEWENUM, "NEWENUM" );
     BEGIN          
          PE;          
          PN;
     END;
     
     DECLARE
          GENERIC
               TYPE E IS (<>);
          FUNCTION F (E1 : E) RETURN BOOLEAN;

          FUNCTION F (E1 : E) RETURN BOOLEAN IS
          BEGIN
               RETURN E'VAL (0) = E1;
          END F;               
     
          FUNCTION FE IS NEW F (ENUM);

     BEGIN

          DECLARE
               FUNCTION A_B_C RETURN ENUM IS
               BEGIN
                    RETURN ENUM'VAL (IDENT_INT (0));
               END A_B_C;
          BEGIN
               IF FE (A_B_C) THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT VAL FOR A_B_C WHEN HIDDEN " &
                             "BY A FUNCTION" );
               END IF;

               IF FE (C35502L.A_B_C) THEN
                    FAILED ( "INCORRECT VAL FOR C35502L.A_B_C" );
               END IF;
         END;
     END;

     RESULT;
END C35502L;
