-- C37003A.ADA

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
-- CHECK THAT MULTIPLE COMPONENT DECLARATIONS ARE TREATED AS A SERIES
-- OF SINGLE COMNENT DECLARATIONS, I.E., THE COMPONENTS ALL HAVE THE
-- SAME TYPE AND ANY EXPRESSION USED IN CONSTRAINTS OR INITIALIZATIONS
-- IS EVALUATED ONCE FOR EACH COMPONENT.

-- DAT 3/30/81
-- SPS 10/26/82
-- JWC 10/23/85  RENAMED FROM C37013A-AB.ADA.
--               ADDED TEST TO ENSURE THAT ANY EXPRESSION USED
--               IN A CONSTRAINT IS EVALUATED ONCE FOR EACH
--               COMPONENT.
-- JRK 11/15/85  ADDED INITIALIZATION EVALUATION CHECKS.

WITH REPORT; USE REPORT;

PROCEDURE C37003A IS

     X : INTEGER := 0;

     FUNCTION F RETURN INTEGER IS
     BEGIN
          X := X + 1;
          RETURN X;
     END F;

     PROCEDURE RESET IS
     BEGIN
          X := 0;
     END RESET;

BEGIN
     TEST ("C37003A", "CHECK THAT MULTIPLE COMPONENT DECLARATIONS " &
                      "ARE TREATED AS A SERIES OF SINGLE COMPONENT " &
                      "DECLARATIONS");

     DECLARE

          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;

          TYPE REC1 IS RECORD
               A1, A2 : ARR (1 .. F) := (OTHERS => F);
          END RECORD;

          R1 : REC1 := (OTHERS => (OTHERS => 1));
          Y : INTEGER := X;
          R1A : REC1;

     BEGIN

          IF R1.A1 = R1.A2 THEN        -- TEST TO SEE IF THE COMPONENTS
               NULL;                   -- ARE OF THE SAME TYPE.
          END IF;

          IF Y /= 2 THEN
               FAILED ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " &
                       "FOR ARRAYS");
          END IF;

          IF X /= 5 THEN
               FAILED ("INITIALIZATION EXPRESSION NOT EVALUATED FOR " &
                       "EACH ARRAY COMPONENT");
          END IF;

          RESET;

     END;

     DECLARE

          TYPE REC2 IS RECORD
               I1, I2 : INTEGER RANGE 1 .. F := F * IDENT_INT(0) + 1;
          END RECORD;

          R2 : REC2 := (OTHERS => 1);
          Y : INTEGER := X;
          R2A : REC2;

     BEGIN

          IF R2.I1 = R2.I2 THEN        -- TEST TO SEE IF THE COMPONENTS
               NULL;                   -- ARE OF THE SAME TYPE.
          END IF;

          IF Y /= 2 THEN
               FAILED ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " &
                       "FOR SCALARS");
          END IF;

          IF X /= 4 THEN
               FAILED ("INITIALIZATION EXPRESSION NOT EVALUATED FOR " &
                       "EACH SCALAR COMPONENT");
          END IF;

          RESET;

     END;

     DECLARE

          TYPE REC3X (DSC : INTEGER) IS RECORD
               NULL;
          END RECORD;

          TYPE REC3Y IS RECORD
               I : INTEGER;
          END RECORD;

          TYPE REC3 IS RECORD
               RX1, RX2 : REC3X (F);
               RY1, RY2 : REC3Y := (I => F);
          END RECORD;

          R3 : REC3 := ((DSC => 1), (DSC => 2), (I => 0), (I => 0));
          Y : INTEGER := X;
          R3A : REC3;

     BEGIN

          IF R3.RX1 = R3.RX2 THEN       -- TEST TO SEE IF THE COMPONENTS
               NULL;                    -- ARE OF THE SAME TYPE.
          END IF;

          IF Y /= 2 THEN
               FAILED ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " &
                       "FOR RECORDS");
          END IF;

          IF X /= 4 THEN
               FAILED ("INITIALIZATION EXPRESSION NOT EVALUATED " &
                       "FOR EACH RECORD COMPONENT");
          END IF;

          RESET;

     END;

     DECLARE

          TYPE REC4X (DSC : INTEGER) IS RECORD
               NULL;
          END RECORD;

          TYPE ACR IS ACCESS REC4X;
          TYPE ACI IS ACCESS INTEGER;

          TYPE REC4 IS RECORD
               AC1, AC2 : ACR (F);
               AC3, AC4 : ACI := NEW INTEGER'(F);
          END RECORD;

          R4 : REC4 := (NULL, NULL, NULL, NULL);
          Y : INTEGER := X;
          R4A : REC4;

     BEGIN

          IF R4.AC1 = R4.AC2 THEN       -- TEST TO SEE IF THE COMPONENTS
               NULL;                    -- ARE OF THE SAME TYPE.
          END IF;

          IF Y /= 2 THEN
               FAILED ("CONSTRAINT EXPRESSION NOT EVALUATED TWICE " &
                       "FOR ACCESS");
          END IF;

          IF X /= 4 THEN
               FAILED ("INITIALIZATION EXPRESSION NOT EVALUATED " &
                       "FOR EACH ACCESS COMPONENT");
          END IF;

     END;

     RESULT;
END C37003A;
