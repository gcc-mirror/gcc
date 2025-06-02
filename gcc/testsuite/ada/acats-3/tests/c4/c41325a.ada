-- C41325A.ADA

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
-- CHECK THAT THE FOLLOWING IMPLICITLY DECLARED ENTITIES CAN BE SELECTED
-- FROM OUTSIDE THE PACKAGE USING AN EXPANDED NAME, FOR AN ARRAY TYPE.
--     CASE 1: CHECK EQUALITY AND INEQUALITY WHEN COMPONENT TYPE IS
--               NON-LIMITED, FOR MULTIDIMENSIONAL ARRAYS.
--     CASE 2: FOR ONE DIMENSIONAL ARRAYS:
--               A) CHECK CATENATION, EQUALITY, AND INEQUALITY WHEN 
--                    COMPONENT TYPE IS NON-LIMITED.
--               B) CHECK RELATIONAL OPERATORS WHEN COMPONENT TYPE IS 
--                    DISCRETE.
--               C) CHECK THE "NOT" OPERATOR AND THE LOGICAL OPERATORS 
--                    WHEN COMPONENT TYPE IS BOOLEAN.

-- TBN  7/17/86

WITH REPORT; USE REPORT;
PROCEDURE C41325A IS

     PACKAGE P IS
          TYPE CATARRAY IS ARRAY (INTEGER RANGE <>) OF INTEGER;

          TYPE ARRAY_1 IS ARRAY (1..10) OF INTEGER;
          TYPE ARRAY_2 IS ARRAY (1..4, 1..4) OF INTEGER;
          TYPE ARRAY_3 IS ARRAY (1..2, 1..3, 1..4) OF INTEGER;
          TYPE ARRAY_4 IS ARRAY (1..10) OF BOOLEAN;
          TYPE ARRAY_5 IS ARRAY (1..4, 1..4) OF BOOLEAN;
          TYPE ARRAY_6 IS ARRAY (1..2, 1..3, 1..4) OF BOOLEAN;

          OBJ_ARA_1 : ARRAY_1 := (1..10 => IDENT_INT(0));
          OBJ_ARA_2 : ARRAY_2 := (1..4 => (1..4 => IDENT_INT(0)));
          OBJ_ARA_3 : ARRAY_3 := (1..2 => (1..3 =>
                                               (1..4 => IDENT_INT(0))));
          OBJ_ARA_4 : ARRAY_4 := (1..10 => IDENT_BOOL(FALSE));
          OBJ_ARA_5 : ARRAY_5 := (1..4 => (1..4 => IDENT_BOOL(FALSE)));
          OBJ_ARA_6 : ARRAY_6 := (1..2 => (1..3 =>
                                          (1..4 => IDENT_BOOL(FALSE))));
          OBJ_ARA_7 : CATARRAY (1..10) := (1..10 => IDENT_INT(0));
          OBJ_ARA_20 : CATARRAY (1..20) := (1..10 => 1,
                                                11..20 => IDENT_INT(0));
     END P;

     VAR_ARA_1 : P.ARRAY_1 := (1..10 => IDENT_INT(1));
     VAR_ARA_2 : P.ARRAY_2 := (1..4 => (1..4 => IDENT_INT(1)));
     VAR_ARA_3 : P.ARRAY_3 := (1..2 => (1..3 =>
                                               (1..4 => IDENT_INT(1))));
     VAR_ARA_4 : P.ARRAY_4 := (1..10 => IDENT_BOOL(TRUE));
     VAR_ARA_5 : P.ARRAY_5 := (1..4 => (1..4 => IDENT_BOOL(TRUE)));
     VAR_ARA_6 : P.ARRAY_6 := (1..2 => (1..3 =>
                                           (1..4 => IDENT_BOOL(TRUE))));
     VAR_ARA_7 : P.CATARRAY (1..10) := (1..10 => IDENT_INT(1));
     VAR_ARA_8 : P.ARRAY_4 := (1..10 => IDENT_BOOL(TRUE));
     VAR_ARA_20 : P.CATARRAY (1..20) := (1..20 => IDENT_INT(0));

BEGIN
     TEST ("C41325A", "CHECK THAT IMPLICITLY DECLARED ENTITIES CAN " &
                      "BE SELECTED FROM OUTSIDE THE PACKAGE USING AN " &
                      "EXPANDED NAME, FOR AN ARRAY TYPE");

     -- CASE 1: MULTIDIMENSIONAL ARRAYS.

     IF P."=" (VAR_ARA_2, P.OBJ_ARA_2) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."=" (VAR_ARA_5, P.OBJ_ARA_5) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."/=" (VAR_ARA_2, P.ARRAY_2'(1..4 => (1..4 => 1))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P."/=" (VAR_ARA_5, P.ARRAY_5'(1..4 => (1..4 => TRUE))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P."=" (VAR_ARA_3, P.OBJ_ARA_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     IF P."/=" (VAR_ARA_6, P.ARRAY_6'(1..2 =>(1..3 =>(1..4 => TRUE)))) 
          THEN
               FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
     END IF;

     -- CASE 2: ONE DIMENSIONAL ARRAYS.

     IF P."=" (VAR_ARA_1, P.OBJ_ARA_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
     END IF;

     IF P."/=" (VAR_ARA_1, P.ARRAY_1'(1..10 => 1)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
     END IF;

     VAR_ARA_20 := P."&" (VAR_ARA_7, P.OBJ_ARA_7);
     IF P."/=" (VAR_ARA_20, P.OBJ_ARA_20) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
     END IF;

     IF P."<" (VAR_ARA_1, P.OBJ_ARA_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
     END IF;

     IF P.">" (P.OBJ_ARA_1, VAR_ARA_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
     END IF;

     IF P."<=" (VAR_ARA_1, P.OBJ_ARA_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
     END IF;

     IF P."<=" (VAR_ARA_1, P.ARRAY_1'(1..10 => 1)) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
     END IF;

     IF P.">=" (VAR_ARA_1, P.ARRAY_1'(1..10 => 2)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
     END IF;

     IF P.">=" (VAR_ARA_1, P.ARRAY_1'(1..10 => 1)) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
     END IF;

     VAR_ARA_8 := P."NOT" (VAR_ARA_4);
     IF P."/=" (VAR_ARA_8, P.OBJ_ARA_4) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 16");
     END IF;

     VAR_ARA_8 := P."OR" (VAR_ARA_4, P.OBJ_ARA_4);
     IF P."=" (VAR_ARA_8, P.OBJ_ARA_4) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 17");
     END IF;

     VAR_ARA_8 := P."AND" (VAR_ARA_4, P.OBJ_ARA_4);
     IF P."/=" (VAR_ARA_8, P.OBJ_ARA_4) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 18");
     END IF;

     VAR_ARA_8 := P."XOR" (VAR_ARA_4, P.OBJ_ARA_4);
     IF P."=" (VAR_ARA_8, P.OBJ_ARA_4) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 19");
     END IF;

     RESULT;
END C41325A;
