--C37404B.ADA

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
--     CHECK THAT 'CONSTRAINED IS FALSE FOR VARIABLES THAT HAVE
--     DISCRIMINANTS WITH DEFAULT VALUES.

-- HISTORY:
--     LDC 06/08/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C37404B IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;

     TYPE REC_DEF(A : INT := 5) IS
          RECORD
               I : INT := 1;
     END RECORD;

     SUBTYPE REC_DEF_SUB IS REC_DEF;

     TYPE REC_DEF_ARR     IS ARRAY (INTEGER RANGE -8..7) OF REC_DEF;
     TYPE REC_DEF_SARR IS ARRAY (INTEGER RANGE -8..7) OF REC_DEF_SUB;

     PACKAGE PRI_PACK IS
          TYPE REC_DEF_PRI(A : INTEGER := 5) IS PRIVATE;
          TYPE REC_DEF_LIM_PRI(A : INTEGER := 5) IS LIMITED PRIVATE;

     PRIVATE

          TYPE REC_DEF_PRI(A : INTEGER := 5) IS
               RECORD
                    I : INTEGER := 1;
          END RECORD;

          TYPE REC_DEF_LIM_PRI(A : INTEGER := 5) IS
               RECORD
                    I : INTEGER := 1;
          END RECORD;

     END PRI_PACK;
     USE PRI_PACK;

     A : REC_DEF;
     B : REC_DEF_SUB;
     C : ARRAY (0..15) OF REC_DEF;
     D : ARRAY (0..15) OF REC_DEF_SUB;
     E : REC_DEF_ARR;
     F : REC_DEF_SARR;
     G : REC_DEF_PRI;
     H : REC_DEF_LIM_PRI;

     Z : REC_DEF;

     PROCEDURE SUBPROG(REC : OUT REC_DEF) IS

     BEGIN
          IF REC'CONSTRAINED THEN
               FAILED("'CONSTRAINED TRUE FOR SUBPROGRAM OUT " &
                      "PARAMETER INSIDE THE SUBPROGRAM");
          END IF;
     END SUBPROG;

BEGIN
     TEST("C37404B", "CHECK THAT 'CONSTRAINED IS FALSE FOR VARIABLES" &
                     " THAT HAVE DISCRIMINANTS WITH DEFAULT VALUES.");

     IF A'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR RECORD COMPONENT");
     END IF;

     IF B'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR SUBTYPE");
     END IF;

     IF C(1)'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR ARRAY TYPE");
     END IF;

     IF D(1)'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR ARRAY OF SUBTYPE");
     END IF;

     IF E(1)'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR ARRAY TYPE");
     END IF;

     IF F(1)'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR ARRAY OF SUBTYPE");
     END IF;

     IF G'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR PRIVATE TYPE");
     END IF;

     IF H'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR LIMITED PRIVATE TYPE");
     END IF;

     SUBPROG(Z);
     IF Z'CONSTRAINED THEN
          FAILED("'CONSTRAINED TRUE FOR SUBPROGRAM OUT PARAMETER " &
                 "AFTER THE CALL");
     END IF;

     IF IDENT_INT(A.I)    /= 1 OR
        IDENT_INT(B.I)    /= 1 OR
        IDENT_INT(C(1).I) /= 1 OR
        IDENT_INT(D(1).I) /= 1 OR
        IDENT_INT(E(1).I) /= 1 OR
        IDENT_INT(F(1).I) /= 1 OR
        IDENT_INT(Z.I)    /= 1 OR
        IDENT_INT(A.A)    /= 5 OR
        IDENT_INT(B.A)    /= 5 OR
        IDENT_INT(C(1).A) /= 5 OR
        IDENT_INT(D(1).A) /= 5 OR
        IDENT_INT(E(1).A) /= 5 OR
        IDENT_INT(F(1).A) /= 5 OR
        IDENT_INT(G.A)    /= 5 OR
        IDENT_INT(H.A)    /= 5 OR
        IDENT_INT(Z.A)    /= 5 THEN
             FAILED("INCORRECT INITIALIZATION VALUES");
     END IF;

     RESULT;
END C37404B;
