-- C32001D.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR ACCESS TYPES, THE 
-- SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED
-- ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE 
-- INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE EVALUATIONS 
-- YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

WITH REPORT; USE REPORT;

PROCEDURE C32001D IS

     TYPE ARR IS ARRAY (1 .. 2) OF INTEGER;
     BUMP : ARR := (0, 0);
     F1 : ARR;

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          F1 (I) := BUMP (I);
          RETURN BUMP (I);
     END F;

     FUNCTION G (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          RETURN BUMP (I);
     END G;

BEGIN
     TEST ("C32001D", "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " & 
                      "FOR ACCESS TYPES, THE SUBTYPE INDICATION " & 
                      "AND THE INITIALIZATION EXPRESSIONS ARE " &
                      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
                      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
                      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
                      "EVALUATIONS YIELD THE SAME RESULT AS A " &
                      "SEQUENCE OF SINGLE OBJECT DECLARATIONS" );
     
     DECLARE
     
          TYPE CELL (SIZE : INTEGER) IS
               RECORD
                    VALUE : INTEGER;
               END RECORD;

          TYPE LINK IS ACCESS CELL;

          L1, L2   : LINK (F (1)) := NEW CELL'(F1 (1), G (1));

          CL1, CL2 : CONSTANT LINK (F (2)) := NEW CELL'(F1 (2), G (2));
          
          PROCEDURE CHECK (L : LINK; V1, V2 : INTEGER; S : STRING) IS
          BEGIN
               IF L.SIZE /= V1 THEN
                    FAILED ( S & ".SIZE INITIALIZED INCORRECTLY TO " &
                             INTEGER'IMAGE (L.SIZE));
               END IF;

               IF L.VALUE /= V2 THEN
                    FAILED ( S & ".VALUE INITIALIZED INCORRECTLY TO " &
                             INTEGER'IMAGE (L.VALUE));
               END IF;
          END CHECK;

     BEGIN
          CHECK (L1, 1, 2, "L1");
          CHECK (L2, 3, 4, "L2");

          CHECK (CL1, 1, 2, "CL1");
          CHECK (CL2, 3, 4, "CL2");
     END;

     RESULT;
END C32001D;
