-- C37207A.ADA

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

--     FOR A TYPE WITH OR WITHOUT DEFAULT DISCRIMINANT VALUES, CHECK
--     THAT A DISCRIMINANT CONSTRAINT CAN BE SUPPLIED IN THE FOLLOWING
--     CONTEXTS AND HAS THE PROPER EFFECT:

--     IN A 1) OBJECT_DECLARATION, 2) COMPONENT_DECLARATION OR
--     3) SUBTYPE INDICATION OF AN ARRAY_TYPE_DEFINITION, AND HENCE,
--     ASSIGNMENTS CANNOT ATTEMPT TO CHANGE THE SPECIFIED DISCRIMINANT
--     VALUES WITHOUT RAISING CONSTRAINT_ERROR

--     4) IN AN ACCESS_TYPE_DEFINITION, AND HENCE, ACCESS VALUES
--     OF THIS ACCESS TYPE CANNOT BE ASSIGNED NON-NULL VALUES
--     DESIGNATING OBJECTS WITH DIFFERENT DISCRIMINANT VALUES.

--     5) IN AN ALLOCATOR, AND THE ALLOCATED OBJECT HAS THE SPECIFIED
--     DISCRIMINANT VALUES.

--     6) IN A FORMAL PARAMETER DECLARATION OF A SUBPROGRAM, AND
--     HENCE, ASSIGNMENTS TO THE FORMAL PARAMETER CANNOT ATTEMPT TO
--     CHANGE THE DISCRIMINANT VALUES WITHOUT RAISING CONSTRAINT_ERROR,
--     CONSTRAINED IS TRUE, AND IF ACTUAL PARAMETERS HAVE DISCRIMINANT
--     VALUES DIFFERENT FROM THE SPECIFIED ONES, CONSTRAINT_ERROR IS
--     RAISED.

-- HISTORY:

--     ASL 07/24/81
--     RJW 08/28/86  CORRECTED SYNTAX ERRORS.
--     JLH 08/07/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--     EDS 07/16/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C37207A IS

BEGIN
     TEST ("C37207A","DISCRIMINANT CONSTRAINT CAN BE SUPPLIED TO " &
          "DECLARATIONS AND DEFINITIONS USING TYPES WITH OR WITHOUT " &
          "DEFAULT DISCRIMINANT VALUES");

     DECLARE
          TYPE REC1 (DISC : INTEGER := 5) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE REC2 (DISC : INTEGER) IS
               RECORD
                    NULL;
                END RECORD;

          OBJ1 : REC1(6);                     -- 1.
          OBJ2 : REC2(6);                     -- 1.
          BADOBJ1 : REC1(7);                  -- 1.
          BADOBJ2 : REC2(7);                  -- 1.

          TYPE REC3 IS
               RECORD
                    COMP1 : REC1(6);          -- 2.
                    COMP2 : REC2(6);          -- 2.
               END RECORD;

          OBJ3 : REC3;

          TYPE ARR1 IS ARRAY (1..10) OF REC1(6);  -- 3.
          TYPE ARR2 IS ARRAY (1..10) OF REC2(6);  -- 3.

          A1 : ARR1;
          A2 : ARR2;

          TYPE REC1_NAME IS ACCESS REC1(6);   -- 4.
          TYPE REC2_NAME IS ACCESS REC2(6);   -- 4.

          ACC1 : REC1_NAME;
          ACC2 : REC2_NAME;

          SUBTYPE REC16 IS REC1(6);
          SUBTYPE REC26 IS REC2(6);

          PROCEDURE PROC (P1 : IN OUT REC16;     -- 6.
                         P2 : IN OUT REC26) IS  -- 6.
          BEGIN
               IF NOT (P1'CONSTRAINED AND P2'CONSTRAINED) THEN  -- 6.
                    FAILED ("'CONSTRAINED ATTRIBUTE INCORRECT FOR " &
                            "CONSTRAINED FORMAL PARAMETERS");
               END IF;
               BEGIN
                    P1 := (DISC => 7);         -- 6.
                    FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                            "ATTEMPT TO CHANGE DISCRIMINANT OF " &
                            "CONSTRAINED FORMAL PARAMETER " &                                                                                  
                            INTEGER'IMAGE(P1.DISC));
               EXCEPTION
                    WHEN CONSTRAINT_ERROR => NULL;
                    WHEN OTHERS => FAILED ("WRONG EXCEPTION     (1)");
               END;
               BEGIN
                    P2 := (DISC => 7);         -- 6.
                    FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                            "ATTEMPT TO CHANGE DISCRIMINANT OF " &
                            "CONSTRAINED FORMAL PARAMETER " & 
                            INTEGER'IMAGE(P2.DISC));
               EXCEPTION
                    WHEN CONSTRAINT_ERROR => NULL;
                    WHEN OTHERS => FAILED ("WRONG EXCEPTION     (2)");
               END;
          END PROC;
     BEGIN
---------------------------------------------------------------

          BEGIN
               OBJ1 := (DISC => IDENT_INT(7));           -- 1.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "ATTEMPT TO CHANGE DISCRIMINANT OF " &
                       "CONSTRAINED OBJECT");
               IF OBJ1 = (DISC => 7) THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (3)");
          END;

---------------------------------------------------------------

          BEGIN
               OBJ3 := ((DISC => IDENT_INT(7)),      -- 2.
                        (DISC => IDENT_INT(7)));     -- 2.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "ATTEMPT TO CHANGE DISCRIMINANT OF " &
                       "CONSTRAINED RECORD COMPONENT");
               IF OBJ3 = ((DISC => 7), (DISC => 7)) THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (4)");
          END;

--------------------------------------------------------------

          BEGIN
               A2(2) := (DISC => IDENT_INT(7));          -- 3.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "ATTEMPT TO CHANGE DISCRIMINANT OF " &
                       "CONSTRAINED ARRAY COMPONENT");
               IF A2(2) = (DISC => 7) THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (5)");
          END;

--------------------------------------------------------------

          BEGIN
               ACC1 := NEW REC1(DISC => IDENT_INT(7));   -- 4.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "ATTEMPT TO ASSIGN INCOMPATIBLE OBJECT " &
                       "TO ACCESS VARIABLE");
               IF ACC1 =  NEW REC1(DISC => 7) THEN
                    COMMENT ("PREVENTING DEAD VARIABLE OPTIMIZATION");
               END IF;
         EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (6)");
          END;

----------------------------------------------------------------

               ACC1 := NEW REC1(DISC => IDENT_INT(6));  -- OK.

          BEGIN
               ACC1.ALL := BADOBJ1;           -- 5.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "ATTEMPT TO ASSIGN INCOMPATIBLE OBJECT " &
                       "TO ACCESSED OBJECT");
               IF ACC1.ALL = BADOBJ1 THEN
                    COMMENT ("PREVENT DEAD VARIABLE OPTIMIZATION");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (7)");
          END;

-----------------------------------------------------------------

               PROC (OBJ1,OBJ2);              -- OK.

          BEGIN
               PROC (BADOBJ1,BADOBJ2);        -- 6.
               FAILED ("CONSTRAINT_ERROR NOT RAISED UPON " &
                       "PASSING OF CONSTRAINED ACTUAL " &
                       "PARAMETERS TO DIFFERENTLY CONSTRAINED " &
                       "FORMAL PARAMETERS");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION     (8)");
          END;

---------------------------------------------------------------
     END;

     RESULT;
END C37207A;
