-- C93003A.ADA

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
-- CHECK THAT ACTIVATION OF TASKS CREATED BY ALLOCATORS PRESENT IN A
--   DECLARATIVE PART TAKES PLACE DURING ELABORATION OF THE
--   CORRESPONDING DECLARATION.
-- SUBTESTS ARE:
--   (A)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--   (B)  AN ARRAY OF TASK ALLOCATOR, IN A FUNCTION.
--   (C)  A RECORD OF TASK ALLOCATOR, IN A PACKAGE SPECIFICATION.
--   (D)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A PACKAGE BODY.
--   (E)  AN ARRAY OF RECORD OF TASK ALLOCATOR, IN A TASK BODY.

-- JRK 9/28/81
-- SPS 11/11/82
-- SPS 11/21/82
-- RJW 8/4/86    ADDED CHECKS ON INITIALIZATIONS OF NON-TASK COMPONENTS
--               OF RECORD TYPES.
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C93003A IS

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
     TEST ("C93003A", "CHECK THAT ACTIVATION OF TASKS CREATED BY " &
                      "ALLOCATORS PRESENT IN A DECLARATIVE PART " &
                      "TAKES PLACE DURING ELABORATION OF THE " &
                      "CORRESPONDING DECLARATION");

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (A)

          TYPE A IS ACCESS TT;
          T1 : A := NEW TT;
          I1 : INTEGER := GLOBAL;
          J  : INTEGER := SIDE_EFFECT (0);
          T2 : A := NEW TT;
          I2 : INTEGER := GLOBAL;

     BEGIN -- (A)

          IF I1 /= 1 OR I2 /= 1 THEN
               FAILED ("A SIMPLE TASK ALLOCATOR IN A BLOCK WAS " &
                       "ACTIVATED TOO LATE - (A)");
          END IF;

     END; -- (A)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (B)

          J : INTEGER;

          FUNCTION F RETURN INTEGER IS

               TYPE A_T IS ARRAY (1 .. 1) OF TT;
               TYPE A IS ACCESS A_T;
               A1 : A := NEW A_T;
               I1 : INTEGER := GLOBAL;
               J  : INTEGER := SIDE_EFFECT (0);
               A2 : A := NEW A_T;
               I2 : INTEGER := GLOBAL;

          BEGIN
               IF I1 /= 1 OR I2 /= 1 THEN
                    FAILED ("AN ARRAY OF TASK ALLOCATOR IN A " &
                            "FUNCTION WAS ACTIVATED TOO LATE - (B)");
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

               TYPE INTREC IS
                    RECORD
                         N1 : INTEGER := GLOBAL;
                    END RECORD;

               TYPE RT IS
                    RECORD
                         M : INTEGER := GLOBAL;
                         T : TT;
                         N : INTREC;
                    END RECORD;

               TYPE A IS ACCESS RT;

               R1 : A := NEW RT;
               I1 : INTEGER := GLOBAL;
               J  : INTEGER := SIDE_EFFECT (0);
               R2 : A := NEW RT;
               I2 : INTEGER := GLOBAL;

          END P;

     BEGIN -- (C1)

          IF P.R1.M /= 0 OR P.R1.N.N1 /= 0 THEN 
               FAILED ("NON-TASK COMPONENTS OF RECORD R1 NOT " &
                       "INITIALIZED BEFORE TASK ACTIVATED - (C1)" );
          END IF;

          IF P.R2.M /= 0 OR P.R2.N.N1 /= 0 THEN 
               FAILED ("NON-TASK COMPONENTS OF RECORD R2 NOT " &
                       "INITIALIZED BEFORE TASK ACTIVATED - (C1)" );
          END IF;

          IF P.I1 /= 1 OR P.I2 /= 1 THEN
               FAILED ("A RECORD OF TASK ALLOCATOR IN A PACKAGE " &
                       "SPECIFICATION WAS ACTIVATED TOO LATE - (C1)");
          END IF;

     END; -- (C1)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C2)

          PACKAGE Q IS
               J1 : INTEGER;
          PRIVATE
               
               TYPE GRADE IS (GOOD, FAIR, POOR);
     
               TYPE REC (G : GRADE) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE ACCR IS ACCESS REC;

               TYPE ACCI IS ACCESS INTEGER;

               TYPE RT IS
                    RECORD
                         M : ACCR := NEW REC (GRADE'VAL (GLOBAL));
                         T : TT;
                         N : ACCI := NEW INTEGER'(GLOBAL);
                    END RECORD;

               TYPE A IS ACCESS RT;

               R1 : A := NEW RT;
               I1 : INTEGER := GLOBAL;
               J2 : INTEGER := SIDE_EFFECT (0);
               R2 : A := NEW RT;
               I2 : INTEGER := GLOBAL;

          END Q;

          PACKAGE BODY Q IS
          BEGIN
               IF R1.M.G /= GOOD OR R1.N.ALL /= 0 THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD R1 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (C2)" );
               END IF;

               IF R2.M.G /= GOOD OR R2.N.ALL /= 0 THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD R2 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (C2)" );
               END IF;

               IF I1 /= 1 OR I2 /= 1 THEN
                    FAILED ("A RECORD OF TASK ALLOCATOR IN A PACKAGE " &
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

               TYPE ARR IS ARRAY (1 .. 1) OF TT;
               TYPE INTARR IS ARRAY (1 .. 1) OF INTEGER;

               TYPE RAT IS
                    RECORD
                         M : INTARR := (1 => GLOBAL);
                         A : ARR;
                         N : INTARR := (1 => GLOBAL);
                    END RECORD;
          END P;

          PACKAGE BODY P IS

               TYPE A IS ACCESS RAT;

               RA1 : A := NEW RAT;
               I1  : INTEGER := GLOBAL;
               J   : INTEGER := SIDE_EFFECT (0);
               RA2 : A := NEW RAT;
               I2  : INTEGER := GLOBAL;

          BEGIN
               IF RA1.M (1) /= 0 OR RA1.N (1) /= 0 THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD RA1 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (D)" );
               END IF;

               IF RA2.M (1) /= 0 OR RA2.N (1) /= 0 THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD RA2 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (D)" );
               END IF;

               IF I1 /= 1 OR I2 /= 1 THEN
                    FAILED ("A RECORD OF ARRAY OF TASK ALLOCATOR IN " &
                            "A PACKAGE BODY WAS ACTIVATED " &
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
                         M : BOOLEAN := BOOLEAN'VAL (GLOBAL);
                         T : TT;
                         N : CHARACTER := CHARACTER'VAL (GLOBAL);
                    END RECORD;

               TYPE ART IS ARRAY (1 .. 1) OF RT;
               TYPE A IS ACCESS ART;

               AR1 : A := NEW ART;
               I1  : INTEGER := GLOBAL;
               J   : INTEGER := SIDE_EFFECT (0);
               AR2 : A := NEW ART;
               I2  : INTEGER := GLOBAL;

          BEGIN
               IF AR1.ALL (1).M /= FALSE     OR 
                  AR1.ALL (1).N /= ASCII.NUL THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD AR1 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (E)" );
               END IF;

               IF AR2.ALL (1).M /= FALSE     OR 
                  AR2.ALL (1).N /= ASCII.NUL THEN 
                    FAILED ("NON-TASK COMPONENTS OF RECORD AR2 NOT " &
                            "INITIALIZED BEFORE TASK ACTIVATED " &
                            "- (E)" );
               END IF;

               IF I1 /= 1 OR I2 /= 1 THEN
                    FAILED ("AN ARRAY OF RECORD OF TASK ALLOCATOR IN " &
                            "A TASK BODY WAS ACTIVATED TOO LATE - (E)");
               END IF;
          END T;

     BEGIN -- (E)

          NULL;

     END; -- (E)

     --------------------------------------------------

     RESULT;
END C93003A;
