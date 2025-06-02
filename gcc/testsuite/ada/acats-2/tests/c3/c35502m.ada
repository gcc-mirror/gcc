-- C35502M.ADA

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
--     CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN
--     THE PREFIX IS AN ENUMERATION TYPE, OTHER THAN A BOOLEAN OR A
--     CHARACTER TYPE, WITH AN ENUMERATION REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 05/27/86  CREATED ORIGINAL TEST.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;

PROCEDURE C35502M IS

     TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);
     FOR ENUM USE (A => 2, BC => 4, ABC => 6,
                   A_B_C => 8, ABCD => 10);

     SUBTYPE SUBENUM IS ENUM RANGE A .. BC;

     TYPE NEWENUM IS NEW ENUM;
     SUBTYPE SUBNEW IS NEWENUM RANGE A .. BC;

BEGIN
     TEST ("C35502M", "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
                      "ENUMERATION TYPE, OTHER THAN A CHARACTER " &
                      "OR A BOOLEAN TYPE, WITH AN ENUMERATION " &
                      "REPRESENTATION CLAUSE" );

     DECLARE
          POSITION : INTEGER;
     BEGIN
          POSITION := 0;

          FOR E IN ENUM
               LOOP
                    IF SUBENUM'POS (E) /= POSITION THEN
                         FAILED ( "INCORRECT SUBENUM'POS (" &
                                   ENUM'IMAGE (E) & ")" );
                    END IF;

                    IF SUBENUM'VAL (POSITION) /= E THEN
                         FAILED ( "INCORRECT SUBENUM'VAL (" &
                                   INTEGER'IMAGE (POSITION) &
                                  ")" );
                    END IF;

                    POSITION := POSITION + 1;
               END LOOP;

          POSITION := 0;
          FOR E IN NEWENUM
               LOOP
                    IF SUBNEW'POS (E) /= POSITION THEN
                         FAILED ( "INCORRECT SUBNEW'POS (" &
                                   NEWENUM'IMAGE (E) & ")" );
                    END IF;

                    IF SUBNEW'VAL (POSITION) /= E THEN
                         FAILED ( "INCORRECT SUBNEW'VAL (" &
                                   INTEGER'IMAGE (POSITION) &
                                  ")" );
                    END IF;

                    POSITION := POSITION + 1;
               END LOOP;
     END;

     DECLARE
          FUNCTION A_B_C RETURN ENUM IS
          BEGIN
               RETURN A;
          END A_B_C;

     BEGIN
          IF ENUM'VAL (0) /= A_B_C THEN
               FAILED ( "WRONG ENUM'VAL (0) WHEN HIDDEN " &
                        "BY FUNCTION - 1" );
          END IF;

          IF ENUM'VAL (0) = C35502M.A_B_C THEN
               FAILED ( "WRONG ENUM'VAL (0) WHEN HIDDEN " &
                        "BY FUNCTION - 2" );
          END IF;
     END;

     BEGIN
          IF ENUM'VAL (IDENT_INT (-1)) = ENUM'FIRST THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (-1)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (-1)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (-1))" );
     END;

     BEGIN
          IF NEWENUM'VAL (IDENT_INT (-1)) = NEWENUM'LAST THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (-1)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (-1)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (-1))" );
     END;

     BEGIN
          IF ENUM'VAL (IDENT_INT (5)) = ENUM'LAST THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (5)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (5)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "ENUM'VAL (IDENT_INT (5))" );
     END;

     BEGIN
          IF NEWENUM'VAL (IDENT_INT (5)) = NEWENUM'LAST THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (5)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (5)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NEWENUM'VAL (IDENT_INT (5))" );
     END;

     RESULT;
END C35502M;
