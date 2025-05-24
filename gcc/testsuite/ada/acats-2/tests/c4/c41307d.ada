-- C41307D.ADA

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
-- CHECK THAT L.R IS ALLOWED INSIDE A PACKAGE, GENERIC PACKAGE,
-- SUBPROGRAM, GENERIC SUBPROGRAM, TASK, BLOCK, LOOP, OR AN ACCEPT
-- STATEMENT NAMED L, IF R IS DECLARED INSIDE THE UNIT.

-- TBN 12/15/86

WITH REPORT; USE REPORT;
PROCEDURE C41307D IS

BEGIN
     TEST ("C41307D", "CHECK THAT L.R IS ALLOWED INSIDE A PACKAGE, " &
                      "GENERIC PACKAGE, SUBPROGRAM, GENERIC " &
                      "SUBPROGRAM, TASK, BLOCK, LOOP, OR AN ACCEPT " &
                      "STATEMENT NAMED L, IF R IS DECLARED INSIDE " &
                      "THE UNIT");
     DECLARE
          PACKAGE L IS
               R : INTEGER := 5;
               A : INTEGER := L.R;
          END L;

          PACKAGE BODY L IS
               B : INTEGER := L.R + 1;
          BEGIN
               IF IDENT_INT(A) /= 5 OR IDENT_INT(B) /= 6 THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
               END IF;
          END L;

          GENERIC
               S : INTEGER;
          PACKAGE M IS
               X : INTEGER := M.S;
          END M;

          PACKAGE BODY M IS
               Y : INTEGER := M.S + 1;
          BEGIN
               IF IDENT_INT(X) /= 2 OR
                  IDENT_INT(Y) /= 3 OR
                  IDENT_INT(M.X) /= 2 THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
               END IF;
          END M;

          PACKAGE Q IS NEW M(2);
     BEGIN
          IF IDENT_INT(Q.X) /= 2 THEN
               FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
          END IF;
     END;
     -------------------------------------------------------------------

     DECLARE
          CH : CHARACTER := '6';

          PROCEDURE L (R : IN OUT CHARACTER) IS
               A : CHARACTER := L.R;
          BEGIN
               IF IDENT_CHAR(L.A) /= '6' THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
               END IF;
               L.R := IDENT_CHAR('7');
          END L;

          GENERIC
               S : CHARACTER;
          PROCEDURE M;

          PROCEDURE M IS
               T : CHARACTER := M.S;
          BEGIN
               IF IDENT_CHAR(T) /= '3' OR IDENT_CHAR(M.S) /= '3' THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
               END IF;
          END M;

          PROCEDURE P1 IS NEW M('3');

     BEGIN
          L (CH);
          IF CH /= IDENT_CHAR('7') THEN
               FAILED ("INCORRECT RESULTS RETURNED FROM PROCEDURE - 6");
          END IF;
          P1;
     END;
     -------------------------------------------------------------------

     DECLARE
          INT : INTEGER := 3;

          FUNCTION L (R : INTEGER) RETURN INTEGER IS
               A : INTEGER := L.R;
          BEGIN
               IF IDENT_INT(L.A) /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
               END IF;
               RETURN IDENT_INT(4);
          END L;

          GENERIC
               S : INTEGER;
          FUNCTION M RETURN INTEGER;

          FUNCTION M RETURN INTEGER IS
               T : INTEGER := M.S;
          BEGIN
               IF IDENT_INT(M.T) /= 4 OR M.S /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
               END IF;
               RETURN IDENT_INT(1);
          END M;

          FUNCTION F1 IS NEW M(4);

     BEGIN
          IF L(INT) /= 4 OR F1 /= 1 THEN
               FAILED ("INCORRECT RESULTS RETURNED FROM FUNCTION - 9");
          END IF;
     END;
     -------------------------------------------------------------------

     DECLARE
          TASK L IS
               ENTRY E (A : INTEGER);
          END L;

          TASK TYPE M IS
               ENTRY E1 (A : INTEGER);
          END M;

          T1 : M;

          TASK BODY L IS
               X : INTEGER := IDENT_INT(1);
               R : INTEGER RENAMES X;
               Y : INTEGER := L.R;
          BEGIN
               X := X + L.R;
               IF X /= IDENT_INT(2) OR Y /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - " &
                            "10");
               END IF;
          END L;

          TASK BODY M IS
               X : INTEGER := IDENT_INT(2);
               R : INTEGER RENAMES X;
               Y : INTEGER := M.R;
          BEGIN
               ACCEPT E1 (A : INTEGER) DO
                    X := X + M.R;
                    IF X /= IDENT_INT(4) OR Y /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT RESULTS FROM EXPANDED " &
                                 "NAME - 11");
                    END IF;
                    IF E1.A /= IDENT_INT(3) THEN
                         FAILED ("INCORRECT RESULTS FROM EXPANDED " &
                                 "NAME - 12");
                    END IF;
               END E1;
          END M;
     BEGIN
          T1.E1 (3);
     END;
     -------------------------------------------------------------------

     DECLARE
          TASK T IS
               ENTRY G (1..2) (A : INTEGER);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT G (1) (A : INTEGER) DO
                    IF G.A /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT RESULTS FROM EXPANDED " &
                                 "NAME - 13");
                    END IF;
                    BLK:
                         DECLARE
                              B : INTEGER := 7;
                         BEGIN
                              IF T.BLK.B /= IDENT_INT(7) THEN
                                   FAILED ("INCORRECT RESULTS FROM " &
                                           "EXPANDED NAME - 14");
                              END IF;
                         END BLK;
               END G;
               ACCEPT G (2) (A : INTEGER) DO
                    IF G.A /= IDENT_INT(1) THEN
                         FAILED ("INCORRECT RESULTS FROM EXPANDED " &
                                 "NAME - 15");
                    END IF;
               END G;
          END T;
     BEGIN
          T.G (1) (2);
          T.G (2) (1);
     END;
     -------------------------------------------------------------------

     SWAP:
          DECLARE
               VAR : CHARACTER := '*';
               RENAME_VAR : CHARACTER RENAMES VAR;
               NEW_VAR : CHARACTER;
          BEGIN
               IF EQUAL (3, 3) THEN
                    NEW_VAR := SWAP.RENAME_VAR;
               END IF;
               IF NEW_VAR /= IDENT_CHAR('*') THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - " &
                            "16");
               END IF;
               LP:  FOR I IN 1..2 LOOP
                         IF SWAP.LP.I = IDENT_INT(2) OR
                            LP.I = IDENT_INT(1) THEN
                              GOTO SWAP.LAB1;
                         END IF;
                         NEW_VAR := IDENT_CHAR('+');
                         <<LAB1>>
                         NEW_VAR := IDENT_CHAR('-');
                    END LOOP LP;
               IF NEW_VAR /= IDENT_CHAR('-') THEN
                    FAILED ("INCORRECT RESULTS FROM FOR LOOP - 17");
               END IF;
          END SWAP;

     RESULT;
END C41307D;
