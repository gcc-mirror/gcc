-- C95071A.ADA

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
-- CHECK THAT OBJECTS DESIGNATED BY IN PARAMETERS OF ACCESS TYPES CAN
-- BE USED AS THE TARGET OF AN ASSIGNMENT STATEMENT AND AS AN ACTUAL
-- PARAMETER OF ANY MODE.  SUBTESTS ARE:
--        (A) INTEGER ACCESS TYPE.
--        (B) ARRAY ACCESS TYPE.
--        (C) RECORD ACCESS TYPE.

-- JWC 7/11/85

WITH REPORT; USE REPORT;
PROCEDURE C95071A IS

BEGIN

     TEST ("C95071A", "CHECK THAT COMPONENTS OF ACCESS IN PARAMETERS " &
                      "MAY BE USED IN ASSIGNMENT CONTEXTS");

     --------------------------------------------------

     DECLARE   -- (A)

          TYPE PTRINT IS ACCESS INTEGER;
          PI : PTRINT;

          TASK TA IS
               ENTRY EA (PI : IN PTRINT);
          END TA;

          TASK BODY TA IS
          BEGIN
               ACCEPT EA (PI : IN PTRINT) DO
                    DECLARE
                         TASK TA1 IS
                              ENTRY EA1 (I : OUT INTEGER);
                              ENTRY EA2 (I : IN OUT INTEGER);
                         END TA1;

                         TASK BODY TA1 IS
                         BEGIN
                              ACCEPT EA1 (I : OUT INTEGER) DO
                                   I := 7;
                              END EA1;

                              ACCEPT EA2 (I : IN OUT INTEGER) DO
                                   I := I + 1;
                              END EA2;
                         END TA1;

                    BEGIN
                         TA1.EA1 (PI.ALL);
                         TA1.EA2 (PI.ALL);
                         PI.ALL := PI.ALL + 1;
                         IF (PI.ALL /= 9) THEN
                              FAILED ("ASSIGNMENT TO COMPONENT OF " &
                                      "INTEGER ACCESS PARAMETER " &
                                      "FAILED");
                         END IF;
                    END;
               END EA;
          END TA;

     BEGIN     -- (A)

          PI := NEW INTEGER'(0);
          TA.EA (PI);

     END;      -- (A)

     ---------------------------------------------

     DECLARE   -- (B)

          TYPE TBL IS ARRAY (1..3) OF INTEGER;
          TYPE PTRTBL IS ACCESS TBL;
          PT : PTRTBL;

          TASK TB IS
               ENTRY EB (PT : IN PTRTBL);
          END TB;

          TASK BODY TB IS
          BEGIN
               ACCEPT EB (PT : IN PTRTBL) DO
                    DECLARE
                         TASK TB1 IS
                              ENTRY EB1 (T : OUT TBL);
                              ENTRY EB2 (T : IN OUT TBL);
                              ENTRY EB3 (I : OUT INTEGER);
                              ENTRY EB4 (I : IN OUT INTEGER);
                         END TB1;

                         TASK BODY TB1 IS
                         BEGIN
                              ACCEPT EB1 (T : OUT TBL) DO
                                   T := (1,2,3);
                              END EB1;

                              ACCEPT EB2 (T : IN OUT TBL) DO
                                   T(3) := T(3) - 1;
                              END EB2;

                              ACCEPT EB3 (I : OUT INTEGER) DO
                                   I := 7;
                              END EB3;

                              ACCEPT EB4 (I : IN OUT INTEGER) DO
                                   I := I + 1;
                              END EB4;
                         END TB1;

                    BEGIN
                         TB1.EB1 (PT.ALL);         -- (1,2,3)
                         TB1.EB2 (PT.ALL);         -- (1,2,2)
                         TB1.EB3 (PT(2));          -- (1,7,2)
                         TB1.EB4 (PT(1));          -- (2,7,2)
                         PT(3) := PT(3) + 7;      -- (2,7,9)
                         IF (PT.ALL /= (2,7,9)) THEN
                              FAILED ("ASSIGNMENT TO COMPONENT OF " &
                                      "ARRAY ACCESS PARAMETER FAILED");
                         END IF;
                    END;
               END EB;
          END TB;

     BEGIN     -- (B)

          PT := NEW TBL'(0,0,0);
          TB.EB (PT);

     END;      -- (B)

     ---------------------------------------------

     DECLARE   -- (C)

          TYPE REC IS
               RECORD
                    I1   : INTEGER;
                    I2   : INTEGER;
                    I3   : INTEGER;
               END RECORD;

          TYPE PTRREC IS ACCESS REC;
          PR : PTRREC;

          TASK TC IS
               ENTRY EC (PR : IN PTRREC);
          END TC;

          TASK BODY TC IS
          BEGIN
               ACCEPT EC (PR : IN PTRREC) DO
                    DECLARE
                         TASK TC1 IS
                              ENTRY EC1 (R : OUT REC);
                              ENTRY EC2 (R : IN OUT REC);
                              ENTRY EC3 (I : OUT INTEGER);
                              ENTRY EC4 (I : IN OUT INTEGER);
                         END TC1;

                         TASK BODY TC1 IS
                         BEGIN
                              ACCEPT EC1 (R : OUT REC) DO
                                   R := (1,2,3);
                              END EC1;

                              ACCEPT EC2 (R : IN OUT REC) DO
                                   R.I3 := R.I3 - 1;
                              END EC2;

                              ACCEPT EC3 (I : OUT INTEGER) DO
                                   I := 7;
                              END  EC3;

                              ACCEPT EC4 (I : IN OUT INTEGER) DO
                                   I := I + 1;
                              END EC4;
                         END TC1;

                    BEGIN
                         TC1.EC1 (PR.ALL);         -- (1,2,3)
                         TC1.EC2 (PR.ALL);         -- (1,2,2)
                         TC1.EC3 (PR.I2);          -- (1,7,2)
                         TC1.EC4 (PR.I1);          -- (2,7,2)
                         PR.I3 := PR.I3 + 7;       -- (2,7,9)
                         IF (PR.ALL /= (2,7,9)) THEN
                              FAILED ("ASSIGNMENT TO COMPONENT OF " &
                                      "RECORD ACCESS PARAMETER " &
                                      "FAILED");
                         END IF;
                    END;
               END EC;
          END TC;

     BEGIN     -- (C)

          PR := NEW REC'(0,0,0);
          TC.EC (PR);

     END;      -- (C)

     ---------------------------------------------

     RESULT;

END C95071A;
