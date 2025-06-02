-- C32113A.ADA

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
--     CHECK THAT WHEN A VARIABLE OR CONSTANT HAVING A CONSTRAINED TYPE
--     WITH DISCRIMINANTS IS DECLARED WITH AN INITIAL VALUE,
--     CONSTRAINT_ERROR IS RAISED IF THE CORRESPONDING DISCRIMINANTS OF
--     THE INITIAL VALUE AND THE SUBTYPE DO NOT HAVE THE SAME VALUE.

-- HISTORY:
--     RJW 07/20/86
--     DWC 06/22/87  ADDED SUBTYPE PRIVAS.  ADDED CODE TO PREVENT DEAD
--                   VARIABLE OPTIMIZATION.

WITH REPORT; USE REPORT;

PROCEDURE C32113A IS

     PACKAGE PKG IS
          TYPE PRIVA (D : INTEGER := 0) IS PRIVATE;
          SUBTYPE PRIVAS IS PRIVA (IDENT_INT (1));
          PRA1 : CONSTANT PRIVAS;

          TYPE PRIVB (D1, D2 : INTEGER) IS PRIVATE;
          PRB12 : CONSTANT PRIVB;

     PRIVATE
          TYPE PRIVA (D : INTEGER := 0) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE PRIVB (D1, D2 : INTEGER) IS
               RECORD
                     NULL;
               END RECORD;

          PRA1  : CONSTANT PRIVAS := (D => (IDENT_INT (1)));
          PRB12 : CONSTANT PRIVB := (IDENT_INT (1), IDENT_INT (2));
     END PKG;

     USE PKG;

     TYPE RECA (D : INTEGER := 0) IS
          RECORD
               NULL;
          END RECORD;

     TYPE RECB (D1, D2 : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     RA1 : CONSTANT RECA (IDENT_INT (1)) := (D => (IDENT_INT (1)));

     RB12 : CONSTANT RECB := (IDENT_INT (1), IDENT_INT (2));

BEGIN
     TEST ("C32113A", "CHECK THAT WHEN A VARIABLE OR CONSTANT " &
                      "HAVING A CONSTRAINED TYPE IS DECLARED WITH " &
                      "AN INITIAL VALUE, CONSTRAINT_ERROR IS " &
                      "RAISED IF THE CORRESPONDING DISCRIMINANTS " &
                      "OF THE INITIAL VALUE AND THE SUBTYPE DO " &
                      "NOT HAVE THE SAME VALUE" );

     BEGIN
          DECLARE
               PR1 : CONSTANT PRIVA (IDENT_INT (0)) := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR1'" );
               IF PR1 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR1'" );
     END;

     BEGIN
          DECLARE
               PR2 : CONSTANT PRIVA (IDENT_INT (2)) := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR2'" );
               IF PR2 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR2'" );
     END;

     BEGIN
          DECLARE
               PR3 : PRIVA (IDENT_INT (0)) := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR3'" );
               IF PR3 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR3'" );
     END;

     BEGIN
          DECLARE
               PR4 : PRIVA (IDENT_INT (2)) := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR4'" );
               IF PR4 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR4'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SPRIVA IS PRIVA (IDENT_INT (-1));
               PR5 : CONSTANT SPRIVA := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR5'" );
               IF PR5 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR5'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SPRIVA IS PRIVA (IDENT_INT (3));
               PR6 : SPRIVA := PRA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR6'" );
               IF PR6 = PRA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR6'" );
     END;

     BEGIN
          DECLARE
               PR7 : CONSTANT PRIVB (IDENT_INT (1), IDENT_INT (1)) :=
                     PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR7'" );
               IF PR7 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR7'" );
     END;

     BEGIN
          DECLARE
               PR8 : CONSTANT PRIVB (IDENT_INT (2), IDENT_INT (2)) :=
                     PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR8'" );
               IF PR8 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR8'" );
     END;

     BEGIN
          DECLARE
               PR9 : PRIVB (IDENT_INT (1), IDENT_INT (1)) := PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR9'" );
               IF PR9 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR9'" );
     END;

     BEGIN
          DECLARE
               PR10 : PRIVB (IDENT_INT (2), IDENT_INT (2)) := PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR10'" );
               IF PR10 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR10'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SPRIVB IS
                       PRIVB (IDENT_INT (-1), IDENT_INT (-2));
               PR11 : CONSTANT SPRIVB := PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR11'" );
               IF PR11 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'PR11'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SPRIVB IS PRIVB (IDENT_INT (2), IDENT_INT (1));
               PR12 : SPRIVB := PRB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR12'" );
               IF PR12 = PRB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'PR12'" );
     END;

     BEGIN
          DECLARE
               R1 : CONSTANT RECA (IDENT_INT (0)) := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R1'" );
               IF R1 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R1'" );
     END;

     BEGIN
          DECLARE
               R2 : CONSTANT RECA (IDENT_INT (2)) := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R2'" );
               IF R2 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R2'" );
     END;

     BEGIN
          DECLARE
               R3 : RECA (IDENT_INT (0)) := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R3'" );
               IF R3 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R3'" );
     END;

     BEGIN
          DECLARE
               R4 : RECA (IDENT_INT (2)) := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R4'" );
               IF R4 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R4'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SRECA IS RECA (IDENT_INT (-1));
               R5 : CONSTANT SRECA := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R5'" );
               IF R5 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R5'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SRECA IS RECA (IDENT_INT (3));
               R6 : SRECA := RA1;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R6'" );
               IF R6 = RA1 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R6'" );
     END;

     BEGIN
          DECLARE
               R7 : CONSTANT RECB (IDENT_INT (1), IDENT_INT (1)) :=
                     RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R7'" );
               IF R7 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R7'" );
     END;

     BEGIN
          DECLARE
               R8 : CONSTANT RECB (IDENT_INT (2), IDENT_INT (2)) :=
                     RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R8'" );
               IF R8 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R8'" );
     END;

     BEGIN
          DECLARE
               R9 : RECB (IDENT_INT (1), IDENT_INT (1)) := RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R9'" );
               IF R9 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R9'" );
     END;

     BEGIN
          DECLARE
               R10 : RECB (IDENT_INT (2), IDENT_INT (2)) := RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R10'" );
               IF R10 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R10'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SRECB IS
                       RECB (IDENT_INT (-1), IDENT_INT (-2));
               R11 : CONSTANT SRECB := RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R11'" );
               IF R11 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF CONSTANT 'R11'" );
     END;

     BEGIN
          DECLARE
               SUBTYPE SRECB IS RECB (IDENT_INT (2), IDENT_INT (1));
               R12 : SRECB := RB12;
          BEGIN
               FAILED ( "NO EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R12'" );
               IF R12 = RB12 THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                        "OF VARIABLE 'R12'" );
     END;

     RESULT;
END C32113A;
