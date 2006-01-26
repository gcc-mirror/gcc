-- C99004A.ADA

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
--     CHECK THAT THE PREFIX OF 'TERMINATED AND 'CALLABLE CAN BE A
--     FUNCTION CALL RETURNING AN OBJECT HAVING A TASK TYPE.

--     NOTE: SEE TEST C38202A FOR CHECKS INVOLVING PREFIXES WHICH ARE
--     ACCESS TYPES DENOTING TASK TYPES OR WHICH ARE FUNCTIONS
--     RETURNING ACCESS TYPES DENOTING TASK TYPES.

-- HISTORY:
--     RJW 09/16/86 CREATED ORIGINAL TEST.
--     DHH 10/15/87 CORRECTED HEADER COMMENTS.

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C99004A IS

     TYPE ENUM IS (A, B, C, D);

     EARRAY : ARRAY (ENUM) OF STRING (1 .. 17) :=
                                        (A => "BEFORE ACTIVATION",
                                         B => "DURING ACTIVATION",
                                         C => "DURING EXECUTION ",
                                         D => "AFTER TERMINATION" );

     FUNCTION CHECK (S : STRING; CALL, B1, TERM, B2 : BOOLEAN;
                     E : ENUM) RETURN BOOLEAN IS
     BEGIN
          IF CALL /= B1 THEN
               FAILED ( "INCORRECT VALUE FOR " & S & "'CALLABLE " &
                         EARRAY (E) & " OF TASK" );
          END IF;

          IF TERM /= B2 THEN
               FAILED ( "INCORRECT VALUE FOR " & S & "'TERMINATED " &
                         EARRAY (E) & " OF TASK" );
          END IF;

          RETURN IDENT_BOOL (TRUE);
     END CHECK;


BEGIN
     TEST ( "C99004A", "CHECK THAT THE PREFIX OF 'TERMINATED AND " &
                       "'CALLABLE CAN BE A FUNCTION CALL RETURNING " &
                       "AN OBJECT HAVING A TASK TYPE" );

     DECLARE

          TASK TYPE TT IS
               ENTRY E;
          END TT;

          PACKAGE PKG1 IS
               T1 : TT;
          END PKG1;

          FUNCTION F RETURN TT IS
          BEGIN
               RETURN PKG1.T1;
          END F;

          PACKAGE PKG2 IS
               A1 : BOOLEAN := CHECK ("F", F'CALLABLE, TRUE,
                                      F'TERMINATED, FALSE, A);
          END PKG2;

          TASK MAIN_TASK IS
               ENTRY E (INTEGER RANGE 1 .. 2);
          END MAIN_TASK;

          TASK BODY TT IS
               B1 : BOOLEAN := CHECK ("F", F'CALLABLE, TRUE,
                                      F'TERMINATED, FALSE, B);
               C1 : BOOLEAN;
          BEGIN
               C1 := CHECK ("F", F'CALLABLE, TRUE,
                            F'TERMINATED, FALSE, C);
               MAIN_TASK.E (1);
               MAIN_TASK.E (2);
          END TT;

          PACKAGE BODY PKG1 IS
          BEGIN
               NULL;
          END;

          TASK BODY MAIN_TASK IS
               D1 : BOOLEAN;
          BEGIN
               ACCEPT E (1);
               ABORT PKG1.T1;
               DELAY 5.0 * Impdef.One_Long_Second;
               D1 := CHECK ("F", F'CALLABLE, FALSE,
                             F'TERMINATED, TRUE, D);
          END MAIN_TASK;

     BEGIN
          NULL;
     END;

     DECLARE

          TASK TYPE TT IS
               ENTRY E;
          END TT;

          T2 : TT;

          A2 : BOOLEAN := CHECK ("T2", T2'CALLABLE, TRUE,
                                  T2'TERMINATED, FALSE, A);

          TASK MAIN_TASK IS
               ENTRY E (INTEGER RANGE 1 .. 2);
          END MAIN_TASK;

          TASK BODY TT IS
               B2 : BOOLEAN := CHECK ("T2", T2'CALLABLE, TRUE,
                                       T2'TERMINATED, FALSE, B);
               C2 : BOOLEAN;
          BEGIN
               C2 := CHECK ("T2", T2'CALLABLE, TRUE,
                             T2'TERMINATED, FALSE, C);
               MAIN_TASK.E (1);
               MAIN_TASK.E (2);
          END TT;

          TASK BODY MAIN_TASK IS
               D2 : BOOLEAN;
          BEGIN
               ACCEPT E (1);
               ABORT T2;
               DELAY 5.0 * Impdef.One_Long_Second;
               D2 := CHECK ("T2", T2'CALLABLE, FALSE,
                             T2'TERMINATED, TRUE, D);
          END MAIN_TASK;

     BEGIN
          NULL;
     END;

     RESULT;
END C99004A;
