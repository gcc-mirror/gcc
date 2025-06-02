--C37404A.ADA

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
--     CHECK THAT 'CONSTRAINED IS TRUE FOR VARIABLES DECLARED WITH A
--     CONSTRAINED TYPE, FOR CONSTANT OBJECTS (EVEN IF NOT DECLARED
--     WITH A CONSTRAINED TYPE), AND DESIGNATED OBJECTS.

-- HISTORY:
--     DHH 02/25/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C37404A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;
     TYPE REC(A : INT) IS
          RECORD
               I : INT;
     END RECORD;

     TYPE ACC_REC IS ACCESS REC(4);
     TYPE ACC_REC1 IS ACCESS REC;
     SUBTYPE REC4 IS REC(4);
     SUBTYPE REC5 IS REC;

     TYPE REC_DEF(A : INT := 5) IS
          RECORD
               I : INT := 1;
     END RECORD;

     TYPE ACC_DEF IS ACCESS REC_DEF(4);
     TYPE ACC_DEF1 IS ACCESS REC_DEF;
     SUBTYPE REC6 IS REC_DEF(6);
     SUBTYPE REC7 IS REC_DEF;

     A : REC4 := (A => 4, I => 1);                    -- CONSTRAINED.
     B : REC5(4) := (A => 4, I => 1);                 -- CONSTRAINED.
     C : REC6;                                        -- CONSTRAINED.
     D : REC7(6);                                     -- CONSTRAINED.
     E : ACC_REC1(4);                                 -- CONSTRAINED.
     F : ACC_DEF1(4);                                 -- CONSTRAINED.
     G : ACC_REC1;                                    -- UNCONSTRAINED.
     H : ACC_DEF1;                                    -- UNCONSTRAINED.

     R : REC(5) := (A => 5, I => 1);                  -- CONSTRAINED.
     T : REC_DEF(5);                                  -- CONSTRAINED.
     U : ACC_REC;                                     -- CONSTRAINED.
     V : ACC_DEF;                                     -- CONSTRAINED.
     W : CONSTANT REC(5) := (A => 5, I => 1);         -- CONSTANT.
     X : CONSTANT REC := (A => 5, I => 1);            -- CONSTANT.
     Y : CONSTANT REC_DEF(5) := (A => 5, I => 1);     -- CONSTANT.
     Z : CONSTANT REC_DEF := (A => 5, I => 1);        -- CONSTANT.

BEGIN
     TEST("C37404A", "CHECK THAT 'CONSTRAINED IS TRUE FOR VARIABLES " &
                     "DECLARED WITH A  CONSTRAINED TYPE, FOR " &
                     "CONSTANT OBJECTS (EVEN IF NOT DECLARED WITH A " &
                     "CONSTRAINED TYPE), AND DESIGNATED OBJECTS");

     U := NEW REC(4);
     V := NEW REC_DEF(4);
     E := NEW REC(4);
     F := NEW REC_DEF(4);
     G := NEW REC(4);                                 -- CONSTRAINED.
     H := NEW REC_DEF(4);                             -- CONSTRAINED.

     IF NOT A'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR SUBTYPE1");
     END IF;

     IF NOT B'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR SUBTYPE2");
     END IF;

     IF NOT C'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT SUBTYPE1");
     END IF;

     IF NOT D'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT SUBTYPE2");
     END IF;

     IF NOT R'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR RECORD COMPONENT");
     END IF;

     IF NOT T'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT VARIABLE");
     END IF;

     IF NOT E.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR ACCESS 1");
     END IF;

     IF NOT F.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 1");
     END IF;

     IF NOT G.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR ACCESS 2");
     END IF;

     IF NOT H.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 2");
     END IF;

     IF NOT U.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR ACCESS 3");
     END IF;

     IF NOT V.ALL'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT ACCESS 3");
     END IF;

     IF NOT W'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR CONSTANT, CONSTRAINED");
     END IF;

     IF NOT X'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR CONSTANT, UNCONSTRAINED");
     END IF;

     IF NOT Y'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT CONSTANT, " &
                 "CONSTRAINED");
     END IF;

     IF NOT Z'CONSTRAINED THEN
          FAILED("'CONSTRAINED NOT TRUE FOR DEFAULT CONSTANT, " &
                 "UNCONSTRAINED");
     END IF;

     IF IDENT_INT(T.I) /= 1 OR
        IDENT_INT(C.I) /= 1 OR
        IDENT_INT(D.I) /= 1 OR
        IDENT_INT(W.A) /= 5 OR
        IDENT_INT(X.A) /= 5 OR
        IDENT_INT(Y.A) /= 5 OR
        IDENT_INT(Z.I) /= 1 OR
        IDENT_INT(A.I) /= 1 OR
        IDENT_INT(B.I) /= 1 OR
        IDENT_BOOL(R.I /= 1) THEN
             FAILED("INCORRECT INITIALIZATION VALUES");
     END IF;

     RESULT;
END C37404A;
