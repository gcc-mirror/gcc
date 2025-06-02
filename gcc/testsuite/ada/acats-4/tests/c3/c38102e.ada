-- C38102E.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE REDECLARED AS A DERIVED GENERIC 
-- FORMAL TYPE.

-- AH     8/15/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
-- DNT 11/28/95  CHANGED TO FLAG1 := F4.

WITH REPORT; USE REPORT;
PROCEDURE C38102E IS
     TYPE RAINBOW IS (RED, ORANGE, YELLOW, GREEN, BLUE, INDIGO, VIOLET);
     TYPE T_FLOAT IS DIGITS 5 RANGE -4.0 .. 4.0;
     TYPE T_FIXED IS DELTA 0.01 RANGE 0.0 .. 1.5;
     SUBTYPE P1 IS INTEGER;
     TYPE P2 IS RANGE 0 .. 10;
     TYPE P3 IS ARRAY (P2) OF INTEGER;
     TYPE P4 IS ARRAY (P2, P2) OF INTEGER;

     F1, F2 : BOOLEAN;

     GENERIC
          TYPE G1 IS (<>);
          TYPE G2 IS RANGE <>;
     FUNCTION G_DISCRETE RETURN BOOLEAN;

     FUNCTION G_DISCRETE RETURN BOOLEAN IS
          TYPE INC1;
          TYPE INC2;
          TYPE F1 IS NEW G1;
          TYPE INC1 IS NEW G1;
          TYPE INC2 IS NEW G2;

          OBJ1_0 : INC1;
          OBJ1_1 : INC1;
          OBJ2_0 : INC2;
          OBJ2_1 : INC2;
          OBJ3 : F1;

          RESULT_VALUE1 : BOOLEAN := FALSE;
          RESULT_VALUE2 : BOOLEAN := FALSE;
     BEGIN
          OBJ3 := F1'LAST;
          OBJ3 := F1'PRED(OBJ3);
          IF INC1(OBJ3) = INC1'PRED(INC1'LAST) THEN
               RESULT_VALUE1 := TRUE;
          END IF;
          OBJ2_0 := INC2'FIRST;
          OBJ2_1 := INC2'LAST;
          IF (OBJ2_0 + OBJ2_1) = (INC2'SUCC(OBJ2_0) +
                                  INC2'PRED(OBJ2_1)) THEN
               RESULT_VALUE2 := TRUE;
          END IF;

          RETURN (RESULT_VALUE1 AND RESULT_VALUE2);
     END G_DISCRETE;

     GENERIC
          TYPE G3 IS DIGITS <>;
          TYPE G4 IS DELTA <>;
     PROCEDURE REALS (FLAG1, FLAG2 : OUT BOOLEAN);

     PROCEDURE REALS (FLAG1, FLAG2 : OUT BOOLEAN) IS
          F1, F2, F3, F4, F5, F6, F7, F8 : BOOLEAN;
          TYPE INC3;
          TYPE INC4;
          TYPE P1 IS NEW G3;
          TYPE P2 IS NEW G4;
          TYPE INC3 IS NEW G3;
          TYPE INC4 IS NEW G4;
     BEGIN
          F4 := P1'LAST = P1(INC3'LAST) AND P1'FIRST = P1(INC3'FIRST);

          F5 := P2'FORE = INC4'FORE;
          F6 := P2'AFT = INC4'AFT;
          F7 := ABS(P2'LAST - P2'FIRST) = P2(ABS(INC4'LAST -
                                                 INC4'FIRST));
          F8 := INC4(P2'LAST / P2'LAST) = INC4(INC4'LAST / INC4'LAST);

          FLAG1 := F4;
          FLAG2 := F5 AND F6 AND F7 AND F8;
     END REALS;

     GENERIC
          TYPE ITEM IS PRIVATE;
          TYPE INDEX IS RANGE <>;
          TYPE G5 IS ARRAY (INDEX) OF ITEM;
          TYPE G6 IS ARRAY (INDEX, INDEX) OF ITEM;
     PACKAGE DIMENSIONS IS
          TYPE INC5;
          TYPE INC6;
          TYPE D1 IS NEW G5;
          TYPE D2 IS NEW G6;
          TYPE INC5 IS NEW G5;
          TYPE INC6 IS NEW G6;
          FUNCTION CHECK RETURN BOOLEAN;
     END DIMENSIONS;

     PACKAGE BODY DIMENSIONS IS
          FUNCTION CHECK RETURN BOOLEAN IS
               A1 : INC5;
               A2 : INC6;
               DIM1 : D1;
               DIM2 : D2;
               F1, F2 : BOOLEAN;
          BEGIN
               F1 := A1(INDEX'FIRST)'SIZE = DIM1(INDEX'FIRST)'SIZE;
               F2 := A2(INDEX'FIRST, INDEX'LAST)'SIZE = 
                     DIM2(INDEX'FIRST, INDEX'LAST)'SIZE;

               RETURN (F1 AND F2);
          END CHECK;
     END DIMENSIONS;
     
     PROCEDURE PROC IS NEW REALS (G3 => T_FLOAT, G4 => T_FIXED);
     FUNCTION DISCRETE IS NEW G_DISCRETE (G1 => RAINBOW, G2 => P2);
     PACKAGE PKG IS NEW DIMENSIONS (ITEM => P1, INDEX => P2, G5 => P3,
                                    G6 => P4);

     USE PKG;
BEGIN
     TEST ("C38102E", "INCOMPLETE TYPES CAN BE DERIVED GENERIC " &
                      "FORMAL TYPES");

     IF NOT DISCRETE THEN
          FAILED ("INTEGER AND ENUMERATED TYPES NOT DERIVED");
     END IF;

     PROC (F1, F2);
     IF (NOT F1) THEN
          FAILED ("FLOAT TYPES NOT DERIVED");
     END IF;
     IF (NOT F2) THEN
          FAILED ("FIXED TYPES NOT DERIVED");
     END IF;

     IF NOT CHECK THEN
          FAILED ("ONE AND TWO DIMENSIONAL ARRAY TYPES NOT DERIVED");
     END IF;

     RESULT;
END C38102E;
