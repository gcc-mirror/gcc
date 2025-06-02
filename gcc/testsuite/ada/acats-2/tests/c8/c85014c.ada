-- C85014C.ADA

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
--     CHECK THAT THE PRESENCE OR ABSENCE OF A RESULT TYPE IS USED TO
--     DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING RENAMED.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85014C IS

     I, J : INTEGER;

     TASK TYPE T IS
          ENTRY Q (I1 : INTEGER);
     END T;

     TASK0 : T;

     PACKAGE FUNC IS
          FUNCTION Q (I1 : INTEGER) RETURN INTEGER;
          FUNCTION FUNC RETURN T;
     END FUNC;
     USE FUNC;

     PROCEDURE PROC (I1: INTEGER) IS
     BEGIN
          I := I1;
     END PROC;

     FUNCTION PROC (I1: INTEGER) RETURN INTEGER IS
     BEGIN
          I := I1 + 1;
          RETURN 0;
     END PROC;

     TASK BODY T IS
     BEGIN
          ACCEPT Q (I1 : INTEGER) DO
               I := I1;
          END Q;
     END T;

     PACKAGE BODY FUNC IS
          FUNCTION Q (I1 : INTEGER) RETURN INTEGER IS
          BEGIN
               I := I1 + 1;
               RETURN 0;
          END Q;

          FUNCTION FUNC RETURN T IS
          BEGIN
               RETURN TASK0;
          END FUNC;
     END FUNC;

BEGIN
     TEST ("C85014C", "CHECK THAT THE PRESENCE OR ABSENCE OF A " &
                      "RESULT TYPE IS USED TO DETERMINE WHICH " &
                      "SUBPROGRAM OR ENTRY IS BEING RENAMED");

     DECLARE
          PROCEDURE PROC1 (J1: INTEGER) RENAMES PROC;

          FUNCTION PROC2 (J1: INTEGER) RETURN INTEGER RENAMES PROC;
     BEGIN
          PROC1(1);
          IF I /= IDENT_INT(1) THEN
               FAILED("INCORRECT VALUE OF I AFTER PROC1");
          END IF;

          J := PROC2(1);
          IF I /= IDENT_INT(2) THEN
               FAILED("INCORRECT VALUE OF I AFTER PROC2");
          END IF;
     END;

     DECLARE
          PROCEDURE FUNC1 (J1 : INTEGER) RENAMES FUNC.FUNC.Q;

          FUNCTION FUNC2 (J1 : INTEGER) RETURN INTEGER RENAMES FUNC.Q;
     BEGIN
          FUNC1(1);
          IF I /= IDENT_INT(1) THEN
               FAILED("INCORRECT VALUE OF I AFTER FUNC1");
          END IF;

          J := FUNC2(1);
          IF I /= IDENT_INT(2) THEN
               FAILED("INCORRECT VALUE OF I AFTER FUNC2");
          END IF;
     END;

     RESULT;
END C85014C;
