-- C43106A.ADA

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
--     CHECK THAT BOTH NAMED AND POSITIONAL NOTATIONS ARE PERMITTED
--     WITHIN THE SAME RECORD AGGREGATE, (PROVIDED THAT ALL POSITIONAL
--     ASSOCIATIONS APPEAR BEFORE ANY NAMED ASSOCIATION).

-- HISTORY:
--     DHH 08/10/88 CREATED ORIGIANL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43106A IS

     TYPE REC IS
          RECORD
               A : INTEGER;
               B : CHARACTER;
               C : BOOLEAN;
               D, E, F, G : INTEGER;
               H, I, J, K : CHARACTER;
               L, M, N, O : BOOLEAN;
               P, Q, R, S : STRING(1 .. 3);
               T, U, V, W, X, Y, Z : BOOLEAN;
          END RECORD;
     AGG : REC := (12, 'A', TRUE, 1, 2, 3, 4, 'B', 'C', 'D', 'E',
                   P|R => "ABC", S|Q => "DEF", L|X|O|U => TRUE,
                   OTHERS => FALSE);

     FUNCTION IDENT_CHAR(X : CHARACTER) RETURN CHARACTER IS
     BEGIN
          IF EQUAL(3, 3) THEN
               RETURN X;
          ELSE
               RETURN 'Z';
          END IF;
     END IDENT_CHAR;

BEGIN
     TEST("C43106A", "CHECK THAT BOTH NAMED AND POSITIONAL NOTATIONS " &
                     "ARE PERMITTED WITHIN THE SAME RECORD " &
                     "AGGREGATE, (PROVIDED THAT ALL POSITIONAL " &
                     "ASSOCIATIONS APPEAR BEFORE ANY NAMED " &
                     "ASSOCIATION)");

     IF NOT IDENT_BOOL(AGG.C) OR NOT IDENT_BOOL(AGG.L) OR
        NOT IDENT_BOOL(AGG.X) OR NOT IDENT_BOOL(AGG.O) OR
        NOT IDENT_BOOL(AGG.U) OR IDENT_BOOL(AGG.M) OR
            IDENT_BOOL(AGG.N) OR IDENT_BOOL(AGG.T) OR
            IDENT_BOOL(AGG.V) OR IDENT_BOOL(AGG.W) OR
            IDENT_BOOL(AGG.Y) OR IDENT_BOOL(AGG.Z) THEN
          FAILED("BOOLEANS NOT INITIALIZED TO AGGREGATE VALUES");
     END IF;

     IF IDENT_STR(AGG.P) /= IDENT_STR(AGG.R) OR
        IDENT_STR(AGG.Q) /= IDENT_STR(AGG.S) THEN
          FAILED("STRINGS NOT INITIALIZED CORRECTLY");
     END IF;

     IF IDENT_CHAR(AGG.B) /= IDENT_CHAR('A') OR
        IDENT_CHAR(AGG.H) /= IDENT_CHAR('B') OR
        IDENT_CHAR(AGG.I) /= IDENT_CHAR('C') OR
        IDENT_CHAR(AGG.J) /= IDENT_CHAR('D') OR
        IDENT_CHAR(AGG.K) /= IDENT_CHAR('E') THEN
          FAILED("CHARACTERS NOT INITIALIZED CORRECTLY");
     END IF;

     RESULT;
END C43106A;
