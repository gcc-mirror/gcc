-- C95078A.ADA

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
--     CHECK THAT AN EXCEPTION RAISED DURING THE EXECUTION OF AN ACCEPT
--     STATEMENT CAN BE HANDLED WITHIN THE ACCEPT BODY.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     DHH 03/21/88 CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE C95078A IS

BEGIN

     TEST("C95078A", "CHECK THAT AN EXCEPTION RAISED DURING THE " &
                     "EXECUTION OF AN ACCEPT STATEMENT CAN BE " &
                     "HANDLED WITHIN THE ACCEPT BODY");

     DECLARE
          O,PT,QT,R,S,TP,B,C,D :INTEGER := 0;
          TASK TYPE PROG_ERR IS
               ENTRY START(M,N,A : IN OUT INTEGER);
               ENTRY STOP;
          END PROG_ERR;

          TASK T IS
               ENTRY START(M,N,A : IN OUT INTEGER);
               ENTRY STOP;
          END T;

          TYPE REC IS
               RECORD
                    B : PROG_ERR;
               END RECORD;

          TYPE ACC IS ACCESS PROG_ERR;

          SUBTYPE X IS INTEGER RANGE 1 .. 10;

          PACKAGE P IS
               OBJ : REC;
          END P;

          TASK BODY PROG_ERR IS
               FAULT : X;
          BEGIN
               ACCEPT START(M,N,A : IN OUT INTEGER) DO
                    BEGIN
                         M := IDENT_INT(1);
                         FAULT := IDENT_INT(11);
                         FAULT := IDENT_INT(FAULT);
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("UNEXPECTED ERROR RAISED - " &
                                     "CONSTRAINT - TASK TYPE");
                    END; -- EXCEPTION
                    BEGIN
                         N := IDENT_INT(1);
                         FAULT := IDENT_INT(5);
                         FAULT := FAULT/IDENT_INT(0);
                         FAULT := IDENT_INT(FAULT);
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("UNEXPECTED ERROR RAISED - " &
                                     "CONSTRAINT - TASK TYPE");
                    END; -- EXCEPTION
                    A := IDENT_INT(1);
               END START;

               ACCEPT STOP;
          END PROG_ERR;

          TASK BODY T IS
               FAULT : X;
          BEGIN
               ACCEPT START(M,N,A : IN OUT INTEGER) DO
                    BEGIN
                         M := IDENT_INT(1);
                         FAULT := IDENT_INT(11);
                         FAULT := IDENT_INT(FAULT);
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("UNEXPECTED ERROR RAISED - " &
                                     "CONSTRAINT - TASK");
                    END; -- EXCEPTION
                    BEGIN
                         N := IDENT_INT(1);
                         FAULT := IDENT_INT(5);
                         FAULT := FAULT/IDENT_INT(0);
                         FAULT := IDENT_INT(FAULT);
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED("UNEXPECTED ERROR RAISED - " &
                                     "CONSTRAINT - TASK");
                    END; -- EXCEPTION
                    A := IDENT_INT(1);
               END START;

               ACCEPT STOP;
          END T;

          PACKAGE BODY P IS
          BEGIN
               OBJ.B.START(O,PT,B);
               OBJ.B.STOP;

               IF O /= IDENT_INT(1) OR PT /= IDENT_INT(1) THEN
                    FAILED("EXCEPTION HANDLER NEVER ENTERED " &
                           "PROPERLY - TASK TYPE OBJECT");
               END IF;

               IF B /= IDENT_INT(1) THEN
                    FAILED("TASK NOT EXITED PROPERLY - TASK TYPE " &
                           "OBJECT");
               END IF;
          END P;

          PACKAGE Q IS
               OBJ : ACC;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               OBJ := NEW PROG_ERR;
               OBJ.START(QT,R,C);
               OBJ.STOP;

               IF QT /= IDENT_INT(1) OR R /= IDENT_INT(1) THEN
                    FAILED("EXCEPTION HANDLER NEVER ENTERED " &
                           "PROPERLY - ACCESS TASK TYPE");
               END IF;

               IF C /= IDENT_INT(1) THEN
                    FAILED("TASK NOT EXITED PROPERLY - ACCESS TASK " &
                           "TYPE");
               END IF;
          END;

     BEGIN
          T.START(S,TP,D);
          T.STOP;

          IF S /= IDENT_INT(1) OR TP /= IDENT_INT(1) THEN
               FAILED("EXCEPTION HANDLER NEVER ENTERED PROPERLY " &
                      "- TASK");
          END IF;

          IF D /= IDENT_INT(1) THEN
               FAILED("TASK NOT EXITED PROPERLY - TASK");
          END IF;
     END; -- DECLARE

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED("EXCEPTION NOT HANDLED INSIDE ACCEPT BODY");
          RESULT;
END C95078A;
