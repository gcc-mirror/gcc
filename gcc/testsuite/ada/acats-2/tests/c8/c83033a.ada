-- C83033A.ADA

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
--     CHECK THAT AN IMPLICIT DECLARATION OF A BLOCK NAME, A LOOP NAME,
--     OR A STATEMENT LABEL HIDES THE DECLARATION OF AN ENUMERATION
--     LITERAL OR OF A DERIVED SUBPROGRAM DECLARED BY A DERIVED TYPE
--     DEFINITION.

-- HISTORY:
--     DHH 09/21/88  CREATED ORIGINAL TEST.
--     WMC 03/25/92  REMOVED TEST REDUNDANCIES.


WITH REPORT; USE REPORT;
PROCEDURE C83033A IS

     PACKAGE BASE_P IS
          TYPE A IS (RED, BLUE, YELO);
          FUNCTION RED(T : INTEGER; X : A) RETURN A;
          FUNCTION BLUE(T : INTEGER; X : A) RETURN A;
     END BASE_P;

     PACKAGE BODY BASE_P IS
          FUNCTION RED(T : INTEGER; X : A) RETURN A IS
          BEGIN
               IF EQUAL(T, T) THEN
                    RETURN X;
               ELSE
                    RETURN YELO;
               END IF;
          END RED;

          FUNCTION BLUE(T : INTEGER; X : A) RETURN A IS
          BEGIN
               IF EQUAL(T, T) THEN
                    RETURN X;
               ELSE
                    RETURN YELO;
               END IF;
          END BLUE;

     END BASE_P;
BEGIN
     TEST ("C83033A", "CHECK THAT AN IMPLICIT DECLARATION OF A BLOCK " &
                      "NAME, A LOOP NAME, OR A STATEMENT LABEL HIDES " &
                      "THE DECLARATION OF AN ENUMERATION LITERAL OR " &
                      "OF A DERIVED SUBPROGRAM DECLARED BY A DERIVED " &
                      "TYPE DEFINITION");

     B1:
     DECLARE
          TYPE STMT2 IS NEW BASE_P.A;
     BEGIN

          DECLARE
               C, D : STMT2;
          BEGIN
               C := C83033A.B1.RED(3, C83033A.B1.RED);
               D := C83033A.B1.RED;

               GOTO RED;              -- DEMONSTRATES USE OF STATEMENT LABEL.
               FAILED("STATEMENT LABEL - 1");

     <<RED>>   IF C /= D THEN
                  FAILED("STATEMENT LABEL - 2");
               END IF;
          END;
     END B1;

     B2:
     DECLARE
          TYPE STMT2 IS NEW BASE_P.A;
     BEGIN

          DECLARE
               A : STMT2 := BLUE;
               B : STMT2 := BLUE(3, BLUE);
          BEGIN

               BLUE:
               FOR I IN 1 .. 1 LOOP
                    IF A /= B THEN
                         FAILED("LOOP NAME - 1");
                    END IF;
                    EXIT BLUE;                -- DEMONSTRATES USE OF LOOP LABEL.
                    FAILED("LOOP NAME - 2");
               END LOOP BLUE;
          END;
     END B2;

     B4:
     DECLARE
          PACKAGE P IS
               GLOBAL : INTEGER := 1;
               TYPE ENUM IS (GREEN, BLUE);
               TYPE PRIV IS PRIVATE;
               FUNCTION GREEN RETURN PRIV;
          PRIVATE
               TYPE PRIV IS NEW ENUM;
          END P;

          PACKAGE BODY P IS
               FUNCTION GREEN RETURN PRIV IS
               BEGIN
                    GLOBAL := GLOBAL + 1;
                    RETURN BLUE;
               END GREEN;
          BEGIN
               NULL;
          END P;
          USE P;
     BEGIN
          GREEN:
          DECLARE
               COLOR : PRIV := C83033A.B4.P.GREEN;
          BEGIN
               IF GREEN.COLOR /= C83033A.B4.P.GREEN OR ELSE GLOBAL /= 3 THEN
                  FAILED("BLOCK NAME");
               END IF;
          END GREEN;
     END B4;

     RESULT;
END C83033A;
