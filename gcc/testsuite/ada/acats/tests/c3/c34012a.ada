-- C34012A.ADA

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
--     CHECK THAT DEFAULT EXPRESSIONS IN DERIVED RECORD TYPES AND
--     DERIVED SUBPROGRAMS ARE EVALUATED USING THE ENTITIES DENOTED BY
--     THE EXPRESSIONS IN THE PARENT TYPE.

-- HISTORY:
--     RJW 06/19/86  CREATED ORIGINAL TEST.
--     BCB 08/19/87  CHANGED HEADER TO STANDARD HEADER FORMAT.  CHANGED
--                   PACKAGE B SO WOULD HAVE ONE CASE WHERE DEFAULT IS
--                   DECLARED BEFORE THE DERIVED TYPE DECLARATION.

WITH REPORT; USE REPORT;

PROCEDURE C34012A IS

BEGIN
     TEST ("C34012A", "CHECK THAT DEFAULT EXPRESSIONS IN DERIVED " &
                      "RECORD TYPES AND DERIVED SUBPROGRAMS ARE " &
                      "EVALUATED USING THE ENTITIES DENOTED BY THE " &
                      "EXPRESSIONS IN THE PARENT TYPE" );

     DECLARE
          PACKAGE P IS
               X : INTEGER := 5;
               TYPE REC IS
                    RECORD
                         C : INTEGER := X;
                    END RECORD;
          END P;

          PACKAGE Q IS
               X : INTEGER := 6;
               TYPE NEW_REC IS NEW P.REC;
               QVAR : NEW_REC;
          END Q;

          PACKAGE R IS
               X : INTEGER := 7;
               TYPE BRAND_NEW_REC IS NEW Q.NEW_REC;
               RVAR : BRAND_NEW_REC;
          END R;

          USE Q;
          USE R;
     BEGIN
          IF QVAR.C = 5 THEN
               NULL;
          ELSE
               FAILED ( "INCORRECT VALUE FOR QVAR" );
          END IF;

          IF RVAR.C = 5 THEN
               NULL;
          ELSE
               FAILED ( "INCORRECT VALUE FOR RVAR" );
          END IF;
     END;

     DECLARE
          PACKAGE A IS
               TYPE T IS RANGE 1 .. 10;
               DEFAULT : T := 5;
               FUNCTION F (X : T := DEFAULT) RETURN T;
          END A;

          PACKAGE BODY A IS
               FUNCTION F (X : T := DEFAULT) RETURN T IS
               BEGIN
                    RETURN X;
               END F;
          END A;

          PACKAGE B IS
               DEFAULT : A.T:= 6;
               TYPE NEW_T IS NEW A.T;
               BVAR : NEW_T := F;
          END B;

          PACKAGE C IS
               TYPE BRAND_NEW_T IS NEW B.NEW_T;
               DEFAULT : BRAND_NEW_T := 7;
               CVAR : BRAND_NEW_T :=F;
          END C;

          USE B;
          USE C;
     BEGIN
          IF BVAR = 5 THEN
               NULL;
          ELSE
               FAILED ( "INCORRECT VALUE FOR BVAR" );
          END IF;

          IF CVAR = 5 THEN
               NULL;
          ELSE
               FAILED ( "INCORRECT VALUE FOR CVAR" );
          END IF;

          DECLARE
               VAR : BRAND_NEW_T := F;
          BEGIN
               IF VAR = 5 THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT VALUE FOR VAR" );
               END IF;
          END;
     END;

     RESULT;
END C34012A;
