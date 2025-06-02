-- CD3015E.ADA

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
--     CHECK THAT WHEN THERE IS NO ENUMERATION CLAUSE FOR THE PARENT
--     TYPE IN A GENERIC UNIT, THE DERIVED TYPE CAN BE USED CORRECTLY
--     IN ORDERING RELATIONS, INDEXING ARRAYS, AND IN GENERIC
--     INSTANTIATIONS.

-- HISTORY
--     DHH 10/05/87 CREATED ORIGINAL TEST
--     DHH 03/30/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND ADDED
--                  CHECK FOR REPRESENTATION CLAUSE.
--     RJW 03/20/90 MODIFIED CHECK FOR ARRAY INDEXING.
--     THS 09/18/90 REVISED WORDING ON FAILURE ERROR MESSAGE.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;                        -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD3015E IS

BEGIN

     TEST ("CD3015E", "CHECK THAT WHEN THERE " &
                      "IS NO ENUMERATION CLAUSE FOR THE PARENT " &
                      "TYPE IN A GENERIC UNIT, THE " &
                      "DERIVED TYPE CAN BE USED CORRECTLY IN " &
                      "ORDERING RELATIONS, INDEXING ARRAYS, AND IN " &
                      "GENERIC INSTANTIATIONS");

     DECLARE

          GENERIC
          PACKAGE GENPACK IS

               TYPE MAIN IS (RED,BLUE,YELLOW,'R','B','Y');

               TYPE HUE IS NEW MAIN;
               FOR HUE USE
                         (RED => 1, BLUE => 6,
                               YELLOW => 11, 'R' => 16,
                               'B' => 22, 'Y' => 30);

               TYPE BASE IS ARRAY(HUE) OF INTEGER;
               COLOR,BASIC : HUE;
               BARRAY : BASE;
               T : INTEGER := 1;

               TYPE INT1 IS RANGE 1 .. 30;
               FOR INT1'SIZE USE HUE'SIZE;

               PROCEDURE CHECK_1 IS NEW ENUM_CHECK(HUE, INT1);

               GENERIC
                    TYPE ENUM IS (<>);
               PROCEDURE CHANGE(X,Y : IN OUT ENUM);

          END GENPACK;

          PACKAGE BODY GENPACK IS

               PROCEDURE CHANGE(X,Y : IN OUT ENUM) IS
                    T : ENUM;
               BEGIN
                    T := X;
                    X := Y;
                    Y := T;
               END CHANGE;

               PROCEDURE PROC IS NEW CHANGE(HUE);

          BEGIN
               BASIC := RED;
               COLOR := HUE'SUCC(BASIC);
               IF (COLOR < BASIC OR
                        BASIC >= 'R' OR
                        'Y' <= COLOR OR
                        COLOR > 'B') THEN
                    FAILED("ORDERING RELATIONS ARE INCORRECT");
               END IF;

               PROC(BASIC,COLOR);

               IF COLOR /= RED THEN
                    FAILED("VALUES OF PARAMETERS TO INSTANCE OF " &
                           "GENERIC UNIT NOT CORRECT AFTER CALL");
               END IF;

               FOR I IN HUE LOOP
                    BARRAY(I) := IDENT_INT(T);
                    T := T + 1;
               END LOOP;

               IF (BARRAY (RED) /= 1 OR BARRAY (BLUE) /= 2 OR
                   BARRAY (YELLOW) /= 3 OR BARRAY ('R') /= 4 OR
                   BARRAY ('B') /= 5 OR BARRAY ('Y') /= 6) THEN
                    FAILED("INDEXING ARRAY FAILURE");
               END IF;

               CHECK_1 (YELLOW, 11, "HUE");

          END GENPACK;

          PACKAGE P IS NEW GENPACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD3015E;
