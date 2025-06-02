-- C64005C.ADA

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
-- CHECK THAT NESTED SUBPROGRAMS CAN BE CALLED RECURSIVELY AND THAT
-- NON-LOCAL VARIABLES AND FORMAL PARAMETERS ARE PROPERLY ACCESSED FROM
-- WITHIN RECURSIVE INVOCATIONS.  THIS TEST CHECKS THAT EVERY DISPLAY OR
-- STATIC CHAIN LEVEL CAN BE ACCESSED.

-- THIS TEST USES 3 LEVELS OF NESTED RECURSIVE PROCEDURES.

-- JRK 7/26/84

WITH REPORT; USE REPORT;

PROCEDURE C64005C IS

     SUBTYPE LEVEL IS CHARACTER RANGE 'A' .. 'C';
     SUBTYPE CALL IS CHARACTER RANGE '1' .. '3';

     MAX_LEV : CONSTANT := LEVEL'POS (LEVEL'LAST) -
                           LEVEL'POS (LEVEL'FIRST) + 1;
     T_LEN : CONSTANT := 2 * (1 + 3 * (MAX_LEV +
                                       MAX_LEV*(MAX_LEV+1)/2*2)) + 1;
     G_LEN : CONSTANT := 2 + 4 * MAX_LEV;

     TYPE TRACE IS
          RECORD
               E : NATURAL := 0;
               S : STRING (1 .. T_LEN);
          END RECORD;

     V : CHARACTER := IDENT_CHAR ('<');
     L : CHARACTER := IDENT_CHAR ('>');
     T : TRACE;
     G : STRING (1 .. G_LEN);

     PROCEDURE C64005CA (L : LEVEL; C : CALL; T : IN OUT TRACE) IS

          V : STRING (1..2);

          M : CONSTANT NATURAL := LEVEL'POS (L) -
                                  LEVEL'POS (LEVEL'FIRST) + 1;
          N : CONSTANT NATURAL := 2 * M + 1;

          PROCEDURE C64005CB (L : LEVEL; C : CALL; T : IN OUT TRACE) IS

               V : STRING (1..2);

               M : CONSTANT NATURAL := LEVEL'POS (L) -
                                       LEVEL'POS (LEVEL'FIRST) + 1;
               N : CONSTANT NATURAL := 2 * M + 1;

               PROCEDURE C64005CC (L : LEVEL; C : CALL;
                                   T : IN OUT TRACE) IS

                    V : STRING (1..2);

                    M : CONSTANT NATURAL := LEVEL'POS (L) -
                                            LEVEL'POS (LEVEL'FIRST) + 1;
                    N : CONSTANT NATURAL := 2 * M + 1;

               BEGIN

                    V (1) := IDENT_CHAR (ASCII.LC_C);
                    V (2) := C;

                    -- APPEND ALL V TO T.
                    T.S (T.E+1 .. T.E+N) := C64005C.V & C64005CA.V &
                                            C64005CB.V & C64005CC.V;
                    T.E := T.E + N;

                    CASE C IS

                         WHEN '1' =>
                              C64005CA (IDENT_CHAR(LEVEL'FIRST),
                                        IDENT_CHAR('2'), T);

                         WHEN '2' =>
                              C64005CC (L, IDENT_CHAR('3'), T);

                         WHEN '3' =>
                              -- APPEND MID-POINT SYMBOL TO T.
                              T.S (T.E+1) := IDENT_CHAR ('=');
                              T.E := T.E + 1;

                              -- G := CATENATE ALL V, L, C;
                              G := C64005C.V & C64005C.L &
                                  C64005CA.V & C64005CA.L & C64005CA.C &
                                  C64005CB.V & C64005CB.L & C64005CB.C &
                                  C64005CC.V & C64005CC.L & C64005CC.C;
                    END CASE;

                    -- APPEND ALL L AND C TO T IN REVERSE ORDER.
                    T.S (T.E+1 .. T.E+N) := C64005CC.L & C64005CC.C &
                                            C64005CB.L & C64005CB.C &
                                            C64005CA.L & C64005CA.C &
                                            C64005C.L;
                    T.E := T.E + N;

               END C64005CC;

          BEGIN

               V (1) := IDENT_CHAR (ASCII.LC_B);
               V (2) := C;

               -- APPEND ALL V TO T.
               T.S (T.E+1 .. T.E+N) := C64005C.V & C64005CA.V &
                                       C64005CB.V;
               T.E := T.E + N;

               CASE C IS

                    WHEN '1' =>
                         C64005CC (LEVEL'SUCC(L), IDENT_CHAR('1'), T);

                    WHEN '2' =>
                         C64005CB (L, IDENT_CHAR('3'), T);

                    WHEN '3' =>
                         C64005CC (LEVEL'SUCC(L), IDENT_CHAR('2'), T);
               END CASE;

               -- APPEND ALL L AND C TO T IN REVERSE ORDER.
               T.S (T.E+1 .. T.E+N) := C64005CB.L & C64005CB.C &
                                       C64005CA.L & C64005CA.C &
                                       C64005C.L;
               T.E := T.E + N;

          END C64005CB;

     BEGIN

          V (1) := IDENT_CHAR (ASCII.LC_A);
          V (2) := C;

          -- APPEND ALL V TO T.
          T.S (T.E+1 .. T.E+N) := C64005C.V & C64005CA.V;
          T.E := T.E + N;

          CASE C IS

               WHEN '1' =>
                    C64005CB (LEVEL'SUCC(L), IDENT_CHAR('1'), T);

               WHEN '2' =>
                    C64005CA (L, IDENT_CHAR('3'), T);

               WHEN '3' =>
                    C64005CB (LEVEL'SUCC(L), IDENT_CHAR('2'), T);
          END CASE;

          -- APPEND ALL L AND C TO T IN REVERSE ORDER.
          T.S (T.E+1 .. T.E+N) := C64005CA.L & C64005CA.C & C64005C.L;
          T.E := T.E + N;

     END C64005CA;

BEGIN
     TEST ("C64005C", "CHECK THAT NON-LOCAL VARIABLES AND FORMAL " &
                      "PARAMETERS AT ALL LEVELS OF NESTED " &
                      "RECURSIVE PROCEDURES ARE ACCESSIBLE");

     -- APPEND V TO T.
     T.S (T.E+1) := V;
     T.E := T.E + 1;

     C64005CA (IDENT_CHAR(LEVEL'FIRST), IDENT_CHAR('1'), T);

     -- APPEND L TO T.
     T.S (T.E+1) := L;
     T.E := T.E + 1;

     COMMENT ("FINAL CALL TRACE LENGTH IS: " & INTEGER'IMAGE(T.E));
     COMMENT ("FINAL CALL TRACE IS: " & T.S(1..T.E));
     COMMENT ("GLOBAL SNAPSHOT IS: " & G);

     -- CHECK THAT T AND G ARE CORRECT BY COMPUTING THEM ITERATIVELY.

     DECLARE
          SUBTYPE LC_LEVEL IS CHARACTER RANGE ASCII.LC_A ..
               CHARACTER'VAL (CHARACTER'POS(ASCII.LC_A) + MAX_LEV - 1);

          CT : TRACE;
          CG : STRING (1 .. G_LEN);
     BEGIN
          COMMENT ("CORRECT FINAL CALL TRACE LENGTH IS: " &
                   INTEGER'IMAGE(T_LEN));

          IF T.E /= IDENT_INT (T_LEN) THEN
               FAILED ("WRONG FINAL CALL TRACE LENGTH");

          ELSE CT.S (CT.E+1) := '<';
               CT.E := CT.E + 1;

               FOR I IN LC_LEVEL LOOP
                    CT.S (CT.E+1) := '<';
                    CT.E := CT.E + 1;

                    FOR J IN LC_LEVEL'FIRST .. I LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '1';
                         CT.E := CT.E + 2;
                    END LOOP;
               END LOOP;

               FOR I IN LC_LEVEL LOOP
                    CT.S (CT.E+1) := '<';
                    CT.E := CT.E + 1;

                    FOR J IN LC_LEVEL'FIRST .. LC_LEVEL'PRED(I) LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '3';
                         CT.E := CT.E + 2;
                    END LOOP;

                    CT.S (CT.E+1) := I;
                    CT.S (CT.E+2) := '2';
                    CT.E := CT.E + 2;

                    CT.S (CT.E+1) := '<';
                    CT.E := CT.E + 1;

                    FOR J IN LC_LEVEL'FIRST .. I LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '3';
                         CT.E := CT.E + 2;
                    END LOOP;
               END LOOP;

               CT.S (CT.E+1) := '=';
               CT.E := CT.E + 1;

               FOR I IN REVERSE LEVEL LOOP
                    FOR J IN REVERSE LEVEL'FIRST .. I LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '3';
                         CT.E := CT.E + 2;
                    END LOOP;

                    CT.S (CT.E+1) := '>';
                    CT.E := CT.E + 1;

                    CT.S (CT.E+1) := I;
                    CT.S (CT.E+2) := '2';
                    CT.E := CT.E + 2;

                    FOR J IN REVERSE LEVEL'FIRST .. LEVEL'PRED(I) LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '3';
                         CT.E := CT.E + 2;
                    END LOOP;

                    CT.S (CT.E+1) := '>';
                    CT.E := CT.E + 1;
               END LOOP;

               FOR I IN REVERSE LEVEL LOOP
                    FOR J IN REVERSE LEVEL'FIRST .. I LOOP
                         CT.S (CT.E+1) := J;
                         CT.S (CT.E+2) := '1';
                         CT.E := CT.E + 2;
                    END LOOP;

                    CT.S (CT.E+1) := '>';
                    CT.E := CT.E + 1;
               END LOOP;

               CT.S (CT.E+1) := '>';
               CT.E := CT.E + 1;

               IF CT.E /= IDENT_INT (T_LEN) THEN
                    FAILED ("WRONG ITERATIVE TRACE LENGTH");

               ELSE COMMENT ("CORRECT FINAL CALL TRACE IS: " & CT.S);

                    IF T.S /= CT.S THEN
                         FAILED ("WRONG FINAL CALL TRACE");
                    END IF;
               END IF;
          END IF;

          DECLARE
               E : NATURAL := 0;
          BEGIN
               CG (1..2) := "<>";
               E := E + 2;

               FOR I IN LEVEL LOOP
                    CG (E+1) := LC_LEVEL'VAL (LEVEL'POS(I) -
                                              LEVEL'POS(LEVEL'FIRST) +
                                              LC_LEVEL'POS
                                                      (LC_LEVEL'FIRST));
                    CG (E+2) := '3';
                    CG (E+3) := I;
                    CG (E+4) := '3';
                    E := E + 4;
               END LOOP;

               COMMENT ("CORRECT GLOBAL SNAPSHOT IS: " & CG);

               IF G /= CG THEN
                    FAILED ("WRONG GLOBAL SNAPSHOT");
               END IF;
          END;
     END;

     RESULT;
END C64005C;
