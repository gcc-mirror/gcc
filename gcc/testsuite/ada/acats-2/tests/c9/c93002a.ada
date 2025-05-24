-- C93002A.ADA

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
-- CHECK THAT DECLARED TASK OBJECTS ARE ACTIVATED BEFORE EXECUTION
--   OF THE FIRST STATEMENT FOLLOWING THE DECLARATIVE PART.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (B)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (C)  A RECORD OF TASK OBJECT, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK OBJECT, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- JRK 9/28/81
-- SPS 11/1/82
-- SPS 11/21/82
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C93002A IS

     GLOBAL : INTEGER;

     FUNCTION SIDE_EFFECT (I : INTEGER) RETURN INTEGER IS
     BEGIN
          GLOBAL := IDENT_INT (I);
          RETURN 0;
     END SIDE_EFFECT;

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TASK BODY TT IS
          I : INTEGER := SIDE_EFFECT (1);
     BEGIN
          NULL;
     END TT;


BEGIN
     TEST ("C93002A", "CHECK THAT DECLARED TASK OBJECTS ARE " &
                      "ACTIVATED BEFORE EXECUTION OF THE FIRST " &
                      "STATEMENT FOLLOWING THE DECLARATIVE PART");

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (A)

          T : TT;

     BEGIN -- (A)

          IF GLOBAL /= 1 THEN
               FAILED ("A SIMPLE TASK OBJECT IN A BLOCK WAS " &
                       "ACTIVATED TOO LATE - (A)");
          END IF;

     END; -- (A)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (B)

          J : INTEGER;

          FUNCTION F RETURN INTEGER IS
               A : ARRAY (1..1) OF TT;
          BEGIN
               IF GLOBAL /= 1 THEN
                    FAILED ("AN ARRAY OF TASK OBJECT IN A FUNCTION " &
                            "WAS ACTIVATED TOO LATE - (B)");
               END IF;
               RETURN 0;
          END F;

     BEGIN -- (B)

          J := F ;

     END; -- (B)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C1)

          PACKAGE P IS
               TYPE ARR IS ARRAY (1..1) OF TT;
               TYPE RT IS
                    RECORD
                         A : ARR;
                    END RECORD;
               R : RT;
          END P;

          PACKAGE BODY P IS
          BEGIN
               IF GLOBAL /= 1 THEN
                    FAILED ("A RECORD OF TASK OBJECT IN A PACKAGE " &
                            "SPECIFICATION WAS ACTIVATED TOO LATE " &
                            "- (C1)");
               END IF;
          END P;

     BEGIN -- (C1)

          NULL;

     END; -- (C1)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C2)

          PACKAGE Q IS
               J : INTEGER;
          PRIVATE
               TYPE RT IS
                    RECORD
                         T : TT;
                    END RECORD;
               R : RT;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               IF GLOBAL /= 1 THEN
                    FAILED ("A RECORD OF TASK OBJECT IN A PACKAGE " &
                            "SPECIFICATION WAS ACTIVATED TOO LATE " &
                            "- (C2)");
               END IF;
          END Q;

     BEGIN -- (C2)

          NULL;

     END; -- (C2)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (D)

          PACKAGE P IS
               TYPE ARR IS ARRAY (1..1) OF TT;
               TYPE RAT IS
                    RECORD
                         A : ARR;
                    END RECORD;
          END P;

          PACKAGE BODY P IS
               RA : RAT;
          BEGIN
               IF GLOBAL /= 1 THEN
                    FAILED ("A RECORD OF ARRAY OF TASK OBJECT IN A " &
                            "PACKAGE BODY WAS ACTIVATED " &
                            "TOO LATE - (D)");
               END IF;
          END P;

     BEGIN -- (D)

          NULL;

     END; -- (D)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (E)

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
               TYPE RT IS
                    RECORD
                         T : TT;
                    END RECORD;
               AR : ARRAY (1..1) OF RT;
          BEGIN
               IF GLOBAL /= 1 THEN
                    FAILED ("AN ARRAY OF RECORD OF TASK OBJECT IN A " &
                            "TASK BODY WAS ACTIVATED TOO LATE - (E)");
               END IF;
          END T;

     BEGIN -- (E)

          NULL;

     END; -- (E)

     --------------------------------------------------

     RESULT;
END C93002A;
