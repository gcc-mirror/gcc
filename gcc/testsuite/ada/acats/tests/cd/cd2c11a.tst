--CD2C11A.TST

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
--     IF A TASK STORAGE SIZE SPECIFICATION IS GIVEN FOR A TASK
--     TYPE, THEN OPERATIONS ON VALUES OF THE TASK TYPE ARE NOT
--     AFFECTED.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY
--     DHH 09/24/87  CREATED ORIGINAL TEST.
--     RJW 07/06/88  REVISED THE TEST TO REMOVE UNINITIALIZED 'IN OUT'
--                   PARAMETER.  CHANGED EXTENSION TO 'TST'.

WITH REPORT; USE REPORT;
PROCEDURE CD2C11A IS

    TASK_STORAGE_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

BEGIN

     TEST ("CD2C11A", "IF A TASK STORAGE SIZE SPECIFICATION IS " &
                      "GIVEN FOR A TASK TYPE, THEN OPERATIONS " &
                      "ON VALUES OF THE TASK TYPE ARE NOT AFFECTED");

     DECLARE
          PACKAGE PACK IS

               TYPE FLT IS DIGITS 1;

               TASK TYPE TTYPE IS
                    ENTRY ADD(J :IN INTEGER; K : IN OUT INTEGER);
                    ENTRY MULT(Y : IN FLT; Z : IN OUT FLT);
               END TTYPE;


               M : INTEGER := 81;
               N : INTEGER := 0;
               V,W : FLT RANGE 1.0..512.0 := 2.0;

               FOR TTYPE'STORAGE_SIZE USE TASK_STORAGE_SIZE;

               T : TTYPE;

          END PACK;

          PACKAGE BODY PACK IS
               FUNCTION IDENT_FLT(FT : FLT) RETURN FLT IS
               BEGIN
                    IF EQUAL(5,5) THEN
                         RETURN FT;
                    ELSE
                         RETURN 0.0;
                    END IF;
               END IDENT_FLT;

               TASK BODY TTYPE IS
                    ITEMP : INTEGER := 0;
                    FTEMP : FLT := 0.0;
               BEGIN
                    ACCEPT ADD(J :IN INTEGER; K : IN OUT INTEGER) DO
                         ITEMP := J;
                         IF EQUAL(3,3) THEN
                              K := ITEMP;
                         ELSE
                              K := 0;
                         END IF;
                    END ADD;
                    ACCEPT MULT(Y : IN FLT; Z : IN OUT FLT) DO
                         FTEMP := Y;
                         IF EQUAL(3,3) THEN
                              Z := FTEMP;
                         ELSE
                              Z := 0.0;
                         END IF;
                    END MULT;
               END TTYPE;

               PROCEDURE TEST_TASK(G : IN TTYPE;
                                 S : IN  FLT; T : IN OUT FLT) IS
                    R : FLT := 4.0;
               BEGIN
                    IF NOT (G'CALLABLE) OR G'TERMINATED THEN
                         FAILED("TASK INSIDE PROCEDURE IS SHOWING " &
                                "WRONG VALUE FOR 'CALLABLE OR " &
                                "'TERMINATED");
                    END IF;
                    G.MULT(S,T);
               END TEST_TASK;

          BEGIN

               IF TTYPE'STORAGE_SIZE < IDENT_INT(TASK_STORAGE_SIZE) THEN
                    FAILED("ACTUAL 'STORAGE_SIZE USED IS SMALLER " &
                           "THAN SIZE REQUESTED");
               END IF;

               T.ADD(M,N);

               IF M /= IDENT_INT(N) THEN
                    FAILED("TASK CALL PARAMETERS NOT EQUAL");
               END IF;

               V := IDENT_FLT(13.0);
               TEST_TASK(T,V,W);
               IF V /= IDENT_FLT(W)  THEN
                    FAILED("TASK AS PARAMETER FAILED");
               END IF;

          END PACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD2C11A;
