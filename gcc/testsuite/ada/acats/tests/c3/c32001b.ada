-- C32001B.ADA

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
--     CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR ARRAY TYPES, THE
--     SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE
--     EVALUATED ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE
--     SUBTYPE INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE
--     EVALUATIONS YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT
--     DECLARATIONS.

-- HISTORY:
--     RJW 07/16/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   COMMENTS FOR S4 AND CS4 TO READ THAT THE BOUNDS ARE
--                   1 .. 6 AND THE COMPONENT TYPE ARR IS 1 .. 5.

WITH REPORT; USE REPORT;

PROCEDURE C32001B IS

     TYPE ARR IS ARRAY (NATURAL RANGE <>) OF INTEGER;

     BUMP : ARRAY (1 .. 4) OF INTEGER := (0, 0, 0, 0);

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          RETURN BUMP (I);
     END F;

BEGIN
     TEST ("C32001B", "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " &
                      "FOR ARRAY TYPES, THE SUBTYPE INDICATION " &
                      "AND THE INITIALIZATION EXPRESSIONS ARE " &
                      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
                      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
                      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
                      "EVALUATIONS YIELD THE SAME RESULT AS A " &
                      "SEQUENCE OF SINGLE OBJECT DECLARATIONS" );

     DECLARE

          S1, S2   : ARR (1 .. F (1)) := (OTHERS => F (1));
          CS1, CS2 : CONSTANT ARR (1 .. F (2)) := (OTHERS => F (2));

          PROCEDURE CHECK (A, B : ARR; STR1, STR2 : STRING) IS
          BEGIN
               IF A'LAST /= 1 THEN
                    FAILED ( "INCORRECT UPPER BOUND FOR " & STR1 );
               END IF;

               IF A (1) /= 2 THEN
                    FAILED ( "INCORRECT INITIAL VALUE FOR " & STR1 );
               END IF;

               IF B'LAST /= 3 THEN
                    FAILED ( "INCORRECT UPPER BOUND FOR " & STR2 );
               END IF;

               BEGIN
                    IF B (1 .. 3) = (4, 5, 6) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(4, 5, 6)" );
                    ELSIF B (1 .. 3) = (5, 4, 6) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(5, 4, 6)" );
                    ELSIF B (1 .. 3) = (4, 6, 5) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(4, 6, 5)" );
                    ELSIF B (1 .. 3) = (6, 4, 5) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(6, 4, 5)" );
                    ELSIF B (1 .. 3) = (6, 5, 4) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(6, 5, 4)" );
                    ELSIF B (1 .. 3) = (5, 6, 4) THEN
                         COMMENT ( STR2 & " WAS INITIALIZED TO " &
                                   "(5, 6, 4)" );
                    ELSE
                         FAILED ( STR2 & " HAS INCORRECT INITIAL " &
                                  "VALUE" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         FAILED ( "CONSTRAINT_ERROR RAISED - " &
                                   STR2 );
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED - " &
                                   STR2 );
               END;
          END;

     BEGIN
          CHECK (S1, S2, "S1", "S2");
          CHECK (CS1, CS2, "CS1", "CS2");
     END;

     DECLARE

          S3, S4 : ARRAY (1 .. F (3)) OF ARR (1 .. F (3)) :=
                   (OTHERS => (OTHERS => F (3)));

          CS3, CS4 : CONSTANT ARRAY (1.. F (4)) OF
                     ARR (1 .. F (4)) :=
                     (OTHERS => (OTHERS => F (4)));
     BEGIN
          IF S3'LAST = 1 THEN
               IF S3 (1)'LAST = 2 THEN
                    COMMENT ( "S3 HAS BOUNDS 1 .. 1 AND " &
                              "COMPONENT TYPE ARR (1 .. 2)" );
                    IF S3 (1)(1 .. 2) = (3, 4) THEN
                         COMMENT ( "S3 HAS INITIAL VALUES " &
                                   "3 AND 4 - 1" );
                    ELSIF S3 (1)(1 .. 2) = (4, 3) THEN
                         COMMENT ( "S3 HAS INITIAL VALUES " &
                                   "4 AND 3 - 1" );
                    ELSE
                         FAILED ( "S3 HAS WRONG INITIAL VALUES - 1" );
                    END IF;
               ELSE
                    FAILED ( "S3 HAS WRONG COMPONENT TYPE - 1" );
               END IF;
          ELSIF S3'LAST = 2 THEN
               IF S3 (1)'LAST = 1 THEN
                    COMMENT ( "S3 HAS BOUNDS 1 .. 2 AND " &
                              "COMPONENT TYPE ARR (1 .. 1)" );
                    IF S3 (1) (1) = 3 AND S3 (2) (1) = 4 THEN
                         COMMENT ( "S3 HAS INITIAL VALUES " &
                                   "3 AND 4 - 2" );
                    ELSIF S3 (1) (1) = 4 AND S3 (2) (1) = 3 THEN
                         COMMENT ( "S3 HAS INITIAL VALUES " &
                                   "4 AND 3 - 2" );
                    ELSE
                         FAILED ( "S3 HAS WRONG INITIAL VALUES - 2" );
                    END IF;
               ELSE
                    FAILED ( "S3 HAS WRONG COMPONENT TYPE - 2" );
               END IF;
          ELSE
               FAILED ( "S3 HAS INCORRECT BOUNDS" );
          END IF;

          IF S4'LAST = 5 THEN
               IF S4 (1)'LAST = 6 THEN
                    COMMENT ( "S4 HAS BOUNDS 1 .. 5 AND " &
                              "COMPONENT TYPE ARR (1 .. 6)" );
               ELSE
                    FAILED ( "S4 HAS WRONG COMPONENT TYPE - 1" );
               END IF;
          ELSIF S4'LAST = 6 THEN
               IF S4 (1)'FIRST = 1 AND S4 (1)'LAST = 5 THEN
                    COMMENT ( "S4 HAS BOUNDS 1 .. 6 AND " &
                              "COMPONENT TYPE ARR (1 .. 5)" );
               ELSE
                    FAILED ( "S4 HAS WRONG COMPONENT TYPE - 2" );
               END IF;
          ELSE
               FAILED ( "S4 HAS INCORRECT BOUNDS" );
          END IF;

          IF BUMP (3) /= 36 THEN
               FAILED ( "FUNCTION F NOT INVOKED CORRECT NUMBER OF " &
                        "TIMES TO INITIALIZE S4" );
          END IF;

          IF CS3'FIRST = 1 AND CS3'LAST = 1 THEN
               IF CS3 (1)'FIRST = 1 AND CS3 (1)'LAST = 2 THEN
                    COMMENT ( "CS3 HAS BOUNDS 1 .. 1 AND " &
                              "COMPONENT TYPE ARR (1 .. 2)" );
                    IF CS3 (1)(1 .. 2) = (3, 4) THEN
                         COMMENT ( "CS3 HAS INITIAL VALUES " &
                                   "3 AND 4 - 1" );
                    ELSIF CS3 (1)(1 .. 2) = (4, 3) THEN
                         COMMENT ( "CS3 HAS INITIAL VALUES " &
                                   "4 AND 3 - 1" );
                    ELSE
                         FAILED ( "CS3 HAS WRONG INITIAL VALUES - 1" );
                    END IF;
               ELSE
                    FAILED ( "CS3 HAS WRONG COMPONENT TYPE - 1" );
               END IF;
          ELSIF CS3'FIRST = 1 AND CS3'LAST = 2 THEN
               IF CS3 (1)'FIRST = 1 AND CS3 (1)'LAST = 1 THEN
                    COMMENT ( "CS3 HAS BOUNDS 1 .. 2 AND " &
                              "COMPONENT TYPE ARR (1 .. 1)" );
                    IF CS3 (1) (1) = 3 AND CS3 (2) (1) = 4 THEN
                         COMMENT ( "CS3 HAS INITIAL VALUES " &
                                   "3 AND 4 - 2" );
                    ELSIF CS3 (1) (1) = 4 AND CS3 (2) (1) = 3 THEN
                         COMMENT ( "CS3 HAS INITIAL VALUES " &
                                   "4 AND 3 - 2" );
                    ELSE
                         FAILED ( "CS3 HAS WRONG INITIAL VALUES - 2" );
                    END IF;
               ELSE
                    FAILED ( "CS3 HAS WRONG COMPONENT TYPE - 2" );
               END IF;
          ELSE
               FAILED ( "CS3 HAS INCORRECT BOUNDS" );
          END IF;

          IF CS4'FIRST = 1 AND CS4'LAST = 5 THEN
               IF CS4 (1)'FIRST = 1 AND CS4 (1)'LAST = 6 THEN
                    COMMENT ( "CS4 HAS BOUNDS 1 .. 5 AND " &
                              "COMPONENT TYPE ARR (1 .. 6)" );
               ELSE
                    FAILED ( "CS4 HAS WRONG COMPONENT TYPE - 1" );
               END IF;
          ELSIF CS4'FIRST = 1 AND CS4'LAST = 6 THEN
               IF CS4 (1)'FIRST = 1 AND CS4 (1)'LAST = 5 THEN
                    COMMENT ( "CS4 HAS BOUNDS 1 .. 6 AND " &
                              "COMPONENT TYPE ARR (1 .. 5)" );
               ELSE
                    FAILED ( "CS4 HAS WRONG COMPONENT TYPE - 2" );
               END IF;
          ELSE
               FAILED ( "CS4 HAS INCORRECT BOUNDS" );
          END IF;

          IF BUMP (4) /= 36 THEN
               FAILED ( "FUNCTION F NOT INVOKED CORRECT NUMBER OF " &
                        "TIMES TO INITIALIZE CS4" );
          END IF;
     END;

     RESULT;
END C32001B;
