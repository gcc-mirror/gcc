-- C41103B.ADA

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
--     CHECK THAT THE NAME IN AN INDEXED_COMPONENT MAY BE:
--        AN IDENTIFIER DENOTING AN ARRAY OBJECT - N1;
--        AN IDENTIFIER DENOTING AN ACCESS OBJECT WHOSE VALUE
--           DESIGNATES AN ARRAY OBJECT - N2;
--        A FUNCTION CALL DELIVERING AN ARRAY OBJECT USING
--           PREDEFINED FUNCTIONS - &, AND THE LOGICAL OPERATORS
--        A USER-DEFINED FUNCTION - F1;
--        A FUNCTION CALL DELIVERING AN ACCESS VALUE THAT
--           DESIGNATES AN ARRAY - F2;
--        A SLICE (CHECKING UPPER AND LOWER BOUND COMPONENTS) - N3;
--        AN INDEXED COMPONENT DENOTING AN ARRAY OBJECT
--           (ARRAY OF ARRAYS) - N4;
--        AN IDENTIFIER PREFIXED BY THE NAME OF THE INNERMOST UNIT
--           ENCLOSING ITS DECLARATION - C41103B.N1;
--        A RECORD COMPONENT (OF A RECORD CONTAINING ONE OR MORE
--           ARRAYS WHOSE BOUNDS DEPEND ON A DISCRIMINANT) - N5.
--     CHECK THAT THE APPROPRIATE COMPONENT IS ACCESSED (FOR
--     DYNAMIC INDICES).

-- HISTORY:
--     WKB 08/05/81  CREATED ORIGINAL TEST.
--     SPS 10/26/82
--     BCB 08/02/88  MODIFIED HEADER FORMAT AND ADDED CALLS TO THE
--                   LOGICAL OPERATORS.
--     BCB 04/16/90  MODIFIED SLICE TEST TO INCLUDE A READING OF THE
--                   COMPONENT DESIGNATED BY THE LOWER BOUND OF THE
--                   SLICE.  ADDED TEST FOR PREFIX OF INDEXED COMPONENT
--                   HAVING A LIMITED TYPE.
--     PWN 11/30/94  SUBTYPE QUALIFIED LITERALS FOR ADA 9X.

WITH REPORT;
USE REPORT;
PROCEDURE C41103B IS

     TYPE A1 IS ARRAY (INTEGER RANGE 1..4) OF INTEGER;
     N1 : A1 := (1,2,3,4);

BEGIN
     TEST ("C41103B", "CHECK THAT AN INDEXED_COMPONENT MAY BE OF " &
                      "CERTAIN FORMS AND THAT THE APPROPRIATE " &
                      "COMPONENT IS ACCESSED (FOR DYNAMIC INDICES)");

     DECLARE

          TYPE A2 IS ARRAY (INTEGER RANGE 1..4) OF BOOLEAN;
          TYPE A3 IS ACCESS A1;
          TYPE A4 IS ARRAY (INTEGER RANGE 1..4) OF A1;
          TYPE R (LENGTH : INTEGER) IS
               RECORD
                    S : STRING (1..LENGTH);
               END RECORD;

          N2 : A3 := NEW A1' (1,2,3,4);
          N3 : ARRAY (1..7) OF INTEGER := (1,2,3,4,5,6,7);
          N4 : A4 := (1 => (1,2,3,4), 2 => (5,6,7,8),
                      3 => (9,10,11,12), 4 => (13,14,15,16));
          N5 : R(4) := (LENGTH => 4, S => "ABCD");

          M2A : A2 := (TRUE,FALSE,TRUE,FALSE);
          M2B : A2 := (TRUE,TRUE,FALSE,FALSE);

          FUNCTION F1 RETURN A2 IS
          BEGIN
               RETURN (FALSE,FALSE,TRUE,FALSE);
          END F1;

          FUNCTION F2 RETURN A3 IS
          BEGIN
               RETURN N2;
          END F2;

          PROCEDURE P1 (X : IN INTEGER; Y : IN OUT INTEGER;
                        Z : OUT INTEGER; W : IN STRING) IS
          BEGIN
               IF X /= 2 THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - " & W);
               END IF;
               IF Y /= 3 THEN
                    FAILED ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
               END IF;
               Y := 8;
               Z := 9;
          END P1;

          PROCEDURE P2 (X : CHARACTER) IS
          BEGIN
               IF X /= 'C' THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - '&'");
               END IF;
          END P2;

          PROCEDURE P3 (X : BOOLEAN) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - F1");
               END IF;
          END P3;

          PROCEDURE P5 (X : IN CHARACTER; Y : IN OUT CHARACTER;
                        Z : OUT CHARACTER) IS
          BEGIN
               IF X /= 'A' THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - N5");
               END IF;
               IF Y /= 'D' THEN
                    FAILED ("WRONG VALUE FOR IN OUT PARAMETER - N5");
               END IF;
               Y := 'Y';
               Z := 'Z';
          END P5;

          PROCEDURE P6 (X : BOOLEAN) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - NOT");
               END IF;
          END P6;

          PROCEDURE P7 (X : BOOLEAN) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - AND");
               END IF;
          END P7;

          PROCEDURE P8 (X : BOOLEAN) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - OR");
               END IF;
          END P8;

          PROCEDURE P9 (X : BOOLEAN) IS
          BEGIN
               IF X /= TRUE THEN
                    FAILED ("WRONG VALUE FOR IN PARAMETER - XOR");
               END IF;
          END P9;

     BEGIN

          IF N1(IDENT_INT(2)) /= 2 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - N1");
          END IF;
          N1(IDENT_INT(2)) := 7;
          IF N1 /= (1,7,3,4) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - N1");
          END IF;
          N1 := (1,2,3,4);
          P1 (N1(IDENT_INT(2)), N1(IDENT_INT(3)),
              N1(IDENT_INT(1)), "N1");
          IF N1 /= (9,2,8,4) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - N1");
          END IF;

          IF N2(IDENT_INT(3)) /= 3 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - N2");
          END IF;
          N2(IDENT_INT(3)) := 7;
          IF N2.ALL /= (1,2,7,4) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - N2");
          END IF;
          N2.ALL := (2,1,4,3);
          P1 (N2(IDENT_INT(1)), N2(IDENT_INT(4)),
              N2(IDENT_INT(2)), "N2");
          IF N2.ALL /= (2,9,4,8) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - N2");
          END IF;

          IF "&" (STRING'("AB"), STRING'("CDEF"))(IDENT_INT(5)) 
             /= CHARACTER'('E') THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - '&'");
          END IF;
          P2 ("&" ("AB", "CD")(IDENT_INT(3)));

          IF "NOT" (M2A)(IDENT_INT(4)) /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - 'NOT'");
          END IF;
          P6 ("NOT" (M2A)(IDENT_INT(4)));

          IF "AND" (M2A,M2B)(IDENT_INT(3)) /= FALSE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - 'AND'");
          END IF;
          P7 ("AND" (M2A,M2B)(IDENT_INT(1)));

          IF "OR" (M2A,M2B)(IDENT_INT(3)) /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - 'OR'");
          END IF;
          P8 ("OR" (M2A,M2B)(IDENT_INT(3)));

          IF "XOR" (M2A,M2B)(IDENT_INT(1)) /= FALSE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - 'XOR'");
          END IF;
          P9 ("XOR" (M2A,M2B)(IDENT_INT(3)));

          IF F1(IDENT_INT(3)) /= TRUE THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - F1");
          END IF;
          P3 (F1(IDENT_INT(3)));

          N2 := NEW A1'(1,2,3,4);
          IF F2(IDENT_INT(2)) /= 2 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - F2");
          END IF;
          F2(IDENT_INT(3)) := 7;
          IF N2.ALL /= (1,2,7,4) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - F2");
          END IF;
          N2.ALL := (1,2,3,4);
          P1 (F2(IDENT_INT(2)), F2(IDENT_INT(3)),
              F2(IDENT_INT(1)), "F2");
          IF N2.ALL /= (9,2,8,4) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - F2");
          END IF;

          IF N3(2..5)(IDENT_INT(2)) /= 2 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION (LOWER BOUND) - N3");
          END IF;
          IF N3(2..5)(IDENT_INT(5)) /= 5 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION (UPPER BOUND) - N3");
          END IF;
          N3(2..5)(IDENT_INT(2)) := 8;
          IF N3 /= (1,8,3,4,5,6,7) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - N3");
          END IF;
          N3 := (5,3,4,2,1,6,7);
          P1 (N3(2..5)(IDENT_INT(4)), N3(2..5)(IDENT_INT(2)),
              N3(2..5)(IDENT_INT(5)), "N3");
          IF N3 /= (5,8,4,2,9,6,7) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - N3");
          END IF;

          IF N4(1)(IDENT_INT(2)) /= 2 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - N4");
          END IF;
          N4(3)(IDENT_INT(1)) := 20;
          IF N4 /= ((1,2,3,4),(5,6,7,8),(20,10,11,12),
                    (13,14,15,16)) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - N4");
          END IF;
          N4 := (1 => (0,6,4,2), 2 => (10,11,12,13),
                 3 => (14,15,16,17), 4 => (7,5,3,1));
          P1 (N4(1)(IDENT_INT(4)), N4(4)(IDENT_INT(3)),
              N4(2)(IDENT_INT(1)), "N4");
          IF N4 /= ((0,6,4,2),(9,11,12,13),(14,15,16,17),
                    (7,5,8,1)) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - N4");
          END IF;

          N1 := (1,2,3,4);
          IF C41103B.N1(IDENT_INT(2)) /= 2 THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - C41103B.N1");
          END IF;
          C41103B.N1(IDENT_INT(2)) := 7;
          IF N1 /= (1,7,3,4) THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - C41103B.N1");
          END IF;
          N1 := (1,2,3,4);
          P1 (C41103B.N1(IDENT_INT(2)), C41103B.N1(IDENT_INT(3)),
              C41103B.N1(IDENT_INT(1)), "C41103B.N1");
          IF N1 /= (9,2,8,4) THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER " &
                       "- C41103B.N1");
          END IF;

          IF N5.S(IDENT_INT(3)) /= 'C' THEN
               FAILED ("WRONG VALUE FOR EXPRESSION - N5");
          END IF;
          N5.S(IDENT_INT(4)) := 'X';
          IF N5.S /= "ABCX" THEN
               FAILED ("WRONG TARGET FOR ASSIGNMENT - N5");
          END IF;
          N5.S := "ABCD";
          P5 (N5.S(IDENT_INT(1)), N5.S(IDENT_INT(4)),
              N5.S(IDENT_INT(2)));
          IF N5.S /= "AZCY" THEN
               FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - N5");
          END IF;

          DECLARE
               PACKAGE P IS
                    TYPE LIM IS LIMITED PRIVATE;
                    PROCEDURE INIT (V : OUT LIM; X,Y,Z : INTEGER);
                    PROCEDURE ASSIGN (ONE : OUT LIM; TWO : LIM);
                    FUNCTION "=" (ONE,TWO : LIM) RETURN BOOLEAN;
               PRIVATE
                    TYPE LIM IS ARRAY(1..3) OF INTEGER;
               END P;

               USE P;

               TYPE A IS ARRAY(1..3) OF LIM;

               H : A;

               N6 : LIM;

               PACKAGE BODY P IS
                    PROCEDURE INIT (V : OUT LIM; X,Y,Z : INTEGER) IS
                    BEGIN
                         V := (X,Y,Z);
                    END INIT;

                    PROCEDURE ASSIGN (ONE : OUT LIM; TWO : LIM) IS
                    BEGIN
                         ONE := TWO;
                    END ASSIGN;

                    FUNCTION "=" (ONE,TWO : LIM) RETURN BOOLEAN IS
                    BEGIN
                         IF ONE(1) = TWO(1) AND ONE(2) = TWO(2) AND
                           ONE(3) = TWO(3) THEN
                              RETURN TRUE;
                         ELSE
                              RETURN FALSE;
                         END IF;
                    END "=";
               END P;

               FUNCTION FR RETURN A IS
               BEGIN
                    RETURN H;
               END FR;

          BEGIN
               INIT (H(1),1,2,3);
               INIT (H(2),4,5,6);
               INIT (H(3),7,8,9);
               INIT (N6,0,0,0);

               ASSIGN (N6,FR(2));

               IF N6 /= FR(2) THEN
                    FAILED ("WRONG VALUE FROM LIMITED COMPONENT TYPE");
               END IF;

          END;
     END;

     RESULT;
END C41103B;
