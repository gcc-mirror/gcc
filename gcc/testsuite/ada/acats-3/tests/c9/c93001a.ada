-- C93001A.ADA

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
-- CHECK THAT DECLARED TASK OBJECTS ARE NOT ACTIVATED BEFORE
--   THE END OF THE DECLARATIVE PART.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (B)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (C)  A RECORD OF TASK OBJECT, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK OBJECT, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- THIS TEST ASSUMES THAT ACTIVATION IS A SEQUENTIAL STEP
--   IN THE FLOW OF CONTROL OF THE PARENT (AS IS REQUIRED BY THE
--   ADA RM).  IF AN IMPLEMENTATION (ILLEGALLY) ACTIVATES A
--   TASK IN PARALLEL WITH ITS PARENT, THIS TEST
--   IS NOT GUARANTEED TO DETECT THE VIOLATION, DUE TO A
--   RACE CONDITION.

-- JRK 9/23/81
-- SPS 11/1/82
-- SPS 11/21/82
-- R.WILLIAMS 10/8/86  ADDED CHECKS ON INITIALIZATIONS OF NON-TASK
--                     COMPONENTS OF RECORD TYPES.
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C93001A IS

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
     TEST ("C93001A", "CHECK THAT DECLARED TASK OBJECTS ARE NOT " &
                      "ACTIVATED BEFORE THE END OF THE DECLARATIVE " &

                      "PART");

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (A)

          T : TT;
          I : INTEGER := GLOBAL;

     BEGIN -- (A)

          IF I /= 0 THEN
               FAILED ("A SIMPLE TASK OBJECT IN A BLOCK WAS " &
                       "ACTIVATED TOO SOON - (A)");
          END IF;

     END; -- (A)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (B)

          J : INTEGER;

          FUNCTION F RETURN INTEGER IS
               A : ARRAY (1..1) OF TT;
               I : INTEGER := GLOBAL;
          BEGIN
               IF I /= 0 THEN
                    FAILED ("AN ARRAY OF TASK OBJECT IN A FUNCTION " &
                            "WAS ACTIVATED TOO SOON - (B)");
               END IF;
               RETURN 0;
          END F;

     BEGIN -- (B)

          J := F ;

     END; -- (B)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C)

          PACKAGE P IS

               TYPE REC IS
                    RECORD
                         T  : TT;
                         N1 : INTEGER := GLOBAL;
                    END RECORD;

               TYPE RT IS
                    RECORD
                         M : INTEGER := GLOBAL;
                         T : TT;
                         N : REC;
                    END RECORD;
               R : RT;
               I : INTEGER := GLOBAL;
          END P;

          PACKAGE Q IS
               J : INTEGER;
          PRIVATE
               TYPE RT IS
                    RECORD
                         N : P.REC;
                         T : TT;
                         M : INTEGER := GLOBAL;
                    END RECORD;
               R : RT;
          END Q;

          K : INTEGER := GLOBAL;

          PACKAGE BODY Q IS
          BEGIN
               IF R.M /= 0  OR R.N.N1 /= 0 THEN
                    FAILED ( "NON-TASK COMPONENTS OF RECORD R NOT " &
                             "INITIALIZED BEFORE TASKS ACTIVATED " &
                             "- (C.1)" );
               END IF;
          END Q;

     BEGIN -- (C)

          IF P.R.M /= 0 OR P.R.N.N1 /= 0 THEN
               FAILED ( "NON-TASK COMPONENTS OF RECORDS NOT " &
                        "INITIALIZED BEFORE TASKS ACTIVATED " &
                        "- (C.2)" );
          END IF;

          IF P.I /= 0 OR K /= 0 THEN
               FAILED ("A RECORD OF TASK OBJECT IN A PACKAGE " &
                       "SPECIFICATION WAS ACTIVATED TOO SOON - (C)");
          END IF;

     END; -- (C)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (D)

          PACKAGE P IS

               TYPE GRADE IS (GOOD, FAIR, POOR);
     
               TYPE REC (G : GRADE) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE ACCR IS ACCESS REC;
               TYPE ACCI IS ACCESS INTEGER;

               TYPE ARR IS ARRAY (1..1) OF TT;
               TYPE RAT IS
                    RECORD
                         M : ACCR := NEW REC (GRADE'VAL (GLOBAL));
                         A : ARR;
                         N : ACCI := NEW INTEGER'(GLOBAL);
                    END RECORD;
               RA1 : RAT;
          PRIVATE
               RA2 : RAT;
          END P;

          PACKAGE BODY P IS
               RA3 : RAT;
               I : INTEGER := GLOBAL;
          BEGIN
               IF RA1.M.G /= GOOD  OR RA1.N.ALL /= 0 THEN
                    FAILED ( "NON-TASK COMPONENTS OF RECORD RA1 NOT " &
                             "INITIALIZED BEFORE TASKS ACTIVATED " &
                             "- (D)" );
               END IF;
               
               IF RA2.M.G /= GOOD  OR RA2.N.ALL /= 0 THEN
                    FAILED ( "NON-TASK COMPONENTS OF RECORD RA2 NOT " &
                             "INITIALIZED BEFORE TASKS ACTIVATED " &
                             "- (D)" );
               END IF;
               
               IF RA3.M.G /= GOOD  OR RA3.N.ALL /= 0 THEN
                    FAILED ( "NON-TASK COMPONENTS OF RECORD RA3 NOT " &
                             "INITIALIZED BEFORE TASKS ACTIVATED " &
                             "- (D)" );
               END IF;
               
               IF I /= 0 THEN
                    FAILED ("A RECORD OF ARRAY OF TASK OBJECT IN A " &
                            "PACKAGE SPEC OR BODY WAS ACTIVATED " &
                            "TOO SOON - (D)");
               END IF;
          END P;

     BEGIN -- (D)

          NULL;

     END; -- (D)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (E)

          TYPE REC IS
               RECORD
                    B : BOOLEAN := BOOLEAN'VAL (GLOBAL);
                    T : TT;
                    C :CHARACTER :=CHARACTER'VAL (GLOBAL);
               END RECORD;

          TASK T IS
               ENTRY E;
          END T;

          TASK BODY T IS
               TYPE RT IS
                    RECORD
                         M : REC;
                         T : TT;
                         N : REC;
                    END RECORD;
               AR : ARRAY (1..1) OF RT;
               I : INTEGER := GLOBAL;
          BEGIN
               IF AR (1).M.B /= FALSE OR AR (1).M.C /= ASCII.NUL OR
                  AR (1).N.B /= FALSE OR AR (1).N.C /= ASCII.NUL THEN
                    FAILED ( "NON-TASK COMPONENTS OF RECORD RT NOT " &
                             "INITIALIZED BEFORE TASKS ACTIVATED " &
                             "- (E)" );
               END IF;
               
               IF I /= 0 THEN
                    FAILED ("AN ARRAY OF RECORD OF TASK OBJECT IN A " &
                            "TASK BODY WAS ACTIVATED TOO SOON - (E)");
               END IF;
          END T;

     BEGIN -- (E)

          NULL;

     END; -- (E)

     --------------------------------------------------

     RESULT;
END C93001A;
