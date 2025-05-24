-- CC1005B.ADA

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
--     CHECK THAT A GENERIC UNIT'S IDENTIFIER CAN BE USED IN ITS
--     FORMAL PART:
--
--     (A) AS THE SELECTOR IN AN EXPANDED NAME TO DENOTE AN ENTITY IN THE
--         VISIBLE PART OF A PACKAGE, OR TO DENOTE AN ENTITY IMMEDIATELY
--         ENCLOSED IN A CONSTRUCT OTHER THAN THE CONSTRUCT IMMEDIATELY
--         ENCLOSING THE GENERIC UNIT.
--
--     (B) AS A SELECTOR TO DENOTE A COMPONENT OF A RECORD OBJECT,
--         AS THE NAME OF A RECORD OR DISCRIMINANT COMPONENT IN A RECORD
--         AGGREGATE, AND AS THE NAME OF A FORMAL PARAMETER IN A
--         FUNCTION CALL.

-- HISTORY:
--     BCB 08/03/88  CREATED ORIGINAL TEST.
--     JRL 03/20/92  DELETED TEST IN BLOCK STATEMENT; CONSOLIDATED
--                   WITH CC1005C.

WITH REPORT; USE REPORT;

PROCEDURE CC1005B IS

     S : INTEGER := IDENT_INT(0);

     PACKAGE CC1005B IS
          I : INTEGER;
          S : INTEGER := IDENT_INT(5);
          GENERIC
               S : INTEGER := IDENT_INT(10);
               V : INTEGER := STANDARD.CC1005B.S;
               W : INTEGER := STANDARD.CC1005B.CC1005B.S;
          FUNCTION CC1005B RETURN INTEGER;
     END CC1005B;

     PACKAGE BODY CC1005B IS
          FUNCTION CC1005B RETURN INTEGER IS
          BEGIN
               IF NOT EQUAL(V,0) THEN
                    FAILED ("WRONG VALUE OF S USED IN ASSIGNMENT OF V");
               END IF;

               IF NOT EQUAL(W,5) THEN
                    FAILED ("WRONG VALUE OF S USED IN ASSIGNMENT OF W");
               END IF;

               RETURN 0;
          END CC1005B;

          FUNCTION NEW_CC IS NEW CC1005B;

     BEGIN
          TEST ("CC1005B", "CHECK THAT A GENERIC UNIT'S IDENTIFIER " &
                           "CAN BE USED IN ITS FORMAL PART: AS THE " &
                           "SELECTOR IN AN EXPANDED NAME TO DENOTE " &
                           "AN ENTITY IN THE VISIBLE PART OF A " &
                           "PACKAGE, OR TO DENOTE AN ENTITY " &
                           "IMMEDIATELY ENCLOSED IN A CONSTRUCT " &
                           "OTHER THAN THE CONSTRUCT IMMEDIATELY " &
                           "ENCLOSING THE GENERIC UNIT; AND AS A " &
                           "SELECTOR TO DENOTE A COMPONENT OF A " &
                           "RECORD OBJECT, AS THE NAME OF A RECORD " &
                           "OR DISCRIMINANT COMPONENT IN A RECORD " &
                           "AGGREGATE, AND AS THE NAME OF A FORMAL " &
                           "PARAMETER IN A FUNCTION CALL");

          I := NEW_CC;
     END CC1005B;

     FUNCTION F (P : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN P;
     END F;

BEGIN

     BLOCK1:
        DECLARE
             TYPE REC IS RECORD
                  P : INTEGER := IDENT_INT(0);
             END RECORD;

             TYPE REC2 (P : INTEGER) IS RECORD
                  NULL;
             END RECORD;

             R : REC;

             J : INTEGER;

             GENERIC
                  V : INTEGER := R.P;
                  X : REC := (P => IDENT_INT(10));
                  Y : REC2 := (P => IDENT_INT(15));
                  Z : INTEGER := F(P => IDENT_INT(20));
             FUNCTION P RETURN INTEGER;

             FUNCTION P RETURN INTEGER IS
             BEGIN
                  IF NOT EQUAL(V,0) THEN
                       FAILED ("WRONG VALUE OF P USED IN ASSIGNMENT " &
                               "OF V");
                  END IF;

                  IF NOT EQUAL(X.P,10) THEN
                       FAILED ("WRONG VALUE USED IN ASSIGNMENT OF X.P");
                  END IF;

                  IF NOT EQUAL(Y.P,15) THEN
                       FAILED ("WRONG VALUE USED IN ASSIGNMENT OF Y.P");
                  END IF;

                  IF NOT EQUAL(Z,20) THEN
                       FAILED ("WRONG VALUE OF P USED IN ASSIGNMENT " &
                               "OF Z");
                  END IF;

                  RETURN 0;
             END P;

             FUNCTION NEW_P IS NEW P;
        BEGIN
             J := NEW_P;
        END BLOCK1;

     RESULT;
END CC1005B;
