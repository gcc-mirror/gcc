-- C84002A.ADA

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
-- CHECK THAT:

--   A) IF A USE CLAUSE NAMES AN ENCLOSING PACKAGE, THE USE CLAUSE
--      HAS NO EFFECT.

--   B) IF A DECLARATION IS DIRECTLY VISIBLE PRIOR TO THE OCCURRENCE
--      OF A USE CLAUSE, AND IS NOT IN THE SET OF POTENTIALLY
--      VISIBLE DECLARATIONS, IT REMAINS DIRECTLY VISIBLE AFTER THE
--      USE CLAUSE.

--   C) IF A HOMOGRAPH FOR A POTENTIALLY VISIBLE SUBPROGRAM OR
--      OBJECT IS DECLARED AFTER A USE CLAUSE, THE POTENTIALLY
--      VISIBLE ENTITY IS NO LONGER VISIBLE.

-- EG  02/16/84

WITH REPORT;

PROCEDURE C84002A IS

     USE REPORT;

BEGIN

     TEST("C84002A","CHECK THAT DECLARATIONS DIRECTLY VISIBLE PRIOR " &
                    "TO THE USE CLAUSE REMAIN VISIBLE AFTERWARDS");

     BEGIN

          COMMENT ("CASE A : CHECK THAT IF A USE CLAUSE NAMES AN " &
                   "ENCLOSING PACKAGE, THE USE CLAUSE HAS NO EFFECT");

CASE_A :  DECLARE

               PACKAGE P1 IS
                    X : FLOAT := 1.5;
               END P1;
               PACKAGE P2 IS
                    X : INTEGER := 15;

                    USE P1;
                    USE P2;

                    A : INTEGER := X;
               END P2;
               PACKAGE BODY P1 IS
               BEGIN
                    NULL;
               END P1;
               PACKAGE BODY P2 IS
               BEGIN
                    IF X /= IDENT_INT(15) OR X /= P2.X OR
                       A /= P2.X THEN
                         FAILED ("CASE A : USE CLAUSE HAS AN EFFECT");
                    END IF;
               END P2;

          BEGIN

               NULL;

          END CASE_A;

          COMMENT ("CASE B : CHECK THAT IF A DECLARATION IS DIRECTLY " &
                   "VISIBLE PRIOR TO THE OCCURRENCE OF A USE CLAUSE, " &
                   "AND IS NOT IN THE SET OF POTENTIALLY VISIBLE "     &
                   "DECLARATIONS, IT REMAINS DIRECTLY VISIBLE");

CASE_B :  BEGIN

     CASE_B1 : DECLARE

                    PACKAGE P1 IS
                         Y : FLOAT := 1.5;
                    END P1;
                    PACKAGE P2 IS
                         X : INTEGER := 15;

                         USE P1;

                         A : INTEGER := X;
                    END P2;

                    PACKAGE BODY P1 IS
                    BEGIN
                         NULL;
                    END P1;
                    PACKAGE BODY P2 IS
                    BEGIN
                         IF X /= IDENT_INT(15) OR X /= P2.X OR
                            A /= P2.X THEN
                              FAILED ("CASE B1 : DECLARATION NO " &
                                      "LONGER DIRECTLY VISIBLE");
                         END IF;
                    END P2;

               BEGIN

                    NULL;

               END CASE_B1;

     CASE_B2 : DECLARE

                    PROCEDURE PROC1 (X : STRING) IS
                    BEGIN
                         NULL;
                    END PROC1;

                    PACKAGE P1 IS
                         PROCEDURE PROC1 (X : STRING);
                    END P1;
                    PACKAGE BODY P1 IS
                         PROCEDURE PROC1 (X : STRING) IS
                         BEGIN
                              FAILED ("CASE B2 : WRONG PROCEDURE " &
                                      "DIRECTLY VISIBLE");
                         END PROC1;
                    END P1;

                    USE P1;

               BEGIN

                    PROC1 ("ABC");

               END CASE_B2;

     CASE_B3 : DECLARE

                    PROCEDURE PROC1 (X : STRING) IS
                    BEGIN
                         NULL;
                    END PROC1;

                    PACKAGE P1 IS
                         PROCEDURE PROC1 (Y : STRING);
                    END P1;
                    PACKAGE BODY P1 IS
                         PROCEDURE PROC1 (Y : STRING) IS
                         BEGIN
                              FAILED ("CASE B3 : WRONG PROCEDURE " &
                                      "DIRECTLY VISIBLE");
                         END PROC1;
                    END P1;

                    USE P1;

               BEGIN

                    PROC1 ("ABC");

               END CASE_B3;

          END CASE_B;

          COMMENT ("CASE C : IF A HOMOGRAPH FOR A POTENTIALLY "      &
                   "VISIBLE SUBPROGRAM OR OBJECT IS DECLARED AFTER " &
                   "A USE CLAUSE, THE POTENTIALLY VISIBLE ENTITY "   &
                   "IS NO LONGER VISIBLE");

CASE_C :  BEGIN

     CASE_C1 : DECLARE

                    PACKAGE P1 IS
                         PROCEDURE PROC1 (X : FLOAT);
                    END P1;

                    USE P1;

                    PACKAGE BODY P1 IS
                         PROCEDURE PROC1 (X : FLOAT) IS
                         BEGIN
                              IF X = -1.5 THEN
                                   FAILED ("CASE C1 : WRONG PROCEDURE" &
                                           " CALLED (A)");
                              ELSIF X /= 1.5 THEN
                                   FAILED ("CASE C1 : WRONG VALUE " &
                                           "PASSED (A)");
                              END IF;
                         END PROC1;
                    BEGIN
                         NULL;
                    END P1;

                    PROCEDURE PROC2 IS
                    BEGIN
                         PROC1 (1.5);
                    END PROC2;

                    PROCEDURE PROC1 (X : FLOAT) IS
                    BEGIN
                         IF X = 1.5 THEN
                              FAILED ("CASE C1 : WRONG PROCEDURE" &
                                      " CALLED (B)");
                         ELSIF X /= -1.5 THEN
                              FAILED ("CASE C1 : WRONG VALUE " &
                                      "PASSED (B)");
                         END IF;
                    END PROC1;

               BEGIN

                    PROC2;
                    PROC1 (-1.5);

               END CASE_C1;

     CASE_C2 : DECLARE

                    PACKAGE P1 IS
                         X : INTEGER := 15;
                    END P1;

                    USE P1;

                    A : INTEGER := X;

                    X : BOOLEAN := TRUE;

                    B : BOOLEAN := X;

               BEGIN

                    IF A /= IDENT_INT(15) THEN
                         FAILED ("CASE C2 : VARIABLE A DOES NOT " &
                                 "CONTAIN THE CORRECT VALUE");
                    END IF;
                    IF B /= IDENT_BOOL(TRUE) THEN
                         FAILED ("CASE C2 : VARIABLE B DOES NOT " &
                                 "CONTAIN THE CORRECT VALUE");
                    END IF;

               END CASE_C2;

          END CASE_C;

     END;

     RESULT;

END C84002A;
