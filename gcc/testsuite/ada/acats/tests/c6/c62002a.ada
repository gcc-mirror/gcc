-- C62002A.ADA

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
-- CHECK THAT THE COMPONENTS OF ACCESS IN PARAMETERS CAN BE USED AS THE
--   TARGET OF AN ASSIGNMENT STATEMENT OR AS AN ACTUAL PARAMETER OF
--   ANY MODE.  SUBTESTS ARE:
--        (A) INTEGER ACCESS TYPE.
--        (B) ARRAY ACCESS TYPE.
--        (C) RECORD ACCESS TYPE.

-- DAS  1/23/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C62002A IS

     USE REPORT;

BEGIN

     TEST ("C62002A", "CHECK THAT COMPONENTS OF ACCESS IN PARAMETERS" &
                      " MAY BE USED IN ASSIGNMENT CONTEXTS");

     --------------------------------------------------

     DECLARE   -- (A)

          TYPE PTRINT IS ACCESS INTEGER;
          PI   : PTRINT;

          PROCEDURE PROCA (PI : IN PTRINT) IS

               PROCEDURE PROCA1 (I : OUT INTEGER) IS
               BEGIN
                    I := 7;
               END PROCA1;

               PROCEDURE PROCA2 (I : IN OUT INTEGER) IS
               BEGIN
                    I := I + 1;
               END PROCA2;
          BEGIN

               PROCA1 (PI.ALL);
               PROCA2 (PI.ALL);
               PI.ALL := PI.ALL + 1;
               IF (PI.ALL /= 9) THEN
                    FAILED ("ASSIGNMENT TO COMPONENT OF INTEGER" &
                            " ACCESS PARAMETER FAILED");
               END IF;
          END PROCA;

     BEGIN     -- (A)

          PI := NEW INTEGER '(0);
          PROCA (PI);

     END;      -- (A)

     ---------------------------------------------

     DECLARE   -- (B)

          TYPE TBL IS ARRAY (1..3) OF INTEGER;
          TYPE PTRTBL IS ACCESS TBL;
          PT   : PTRTBL;

          PROCEDURE PROCB (PT : IN PTRTBL) IS

               PROCEDURE PROCB1 (I : OUT INTEGER) IS
               BEGIN
                    I := 7;
               END  PROCB1;

               PROCEDURE PROCB2 (I : IN OUT INTEGER) IS
               BEGIN
                    I := I + 1;
               END PROCB2;

               PROCEDURE PROCB3 (T : OUT TBL) IS
               BEGIN
                    T := (1,2,3);
               END PROCB3;

               PROCEDURE PROCB4 (T : IN OUT TBL) IS
               BEGIN
                    T(3) := T(3) - 1;
               END PROCB4;

          BEGIN

               PROCB3 (PT.ALL);         -- (1,2,3)
               PROCB4 (PT.ALL);         -- (1,2,2)
               PROCB1 (PT(2));          -- (1,7,2)
               PROCB2 (PT(1));          -- (2,7,2)
               PT(3) := PT(3) + 7;      -- (2,7,9)
               IF (PT.ALL /= (2,7,9)) THEN
                    FAILED ("ASSIGNMENT TO COMPONENT OF ARRAY" &
                            " ACCESS PARAMETER FAILED");
               END IF;
          END PROCB;

     BEGIN     -- (B)

          PT := NEW TBL '(0,0,0);
          PROCB (PT);

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
          PR   : PTRREC;

          PROCEDURE PROCC (PR : IN PTRREC) IS

               PROCEDURE PROCC1 (I : OUT INTEGER) IS
               BEGIN
                    I := 7;
               END  PROCC1;

               PROCEDURE PROCC2 (I : IN OUT INTEGER) IS
               BEGIN
                    I := I + 1;
               END PROCC2;

               PROCEDURE PROCC3 (R : OUT REC) IS
               BEGIN
                    R := (1,2,3);
               END PROCC3;

               PROCEDURE PROCC4 (R : IN OUT REC) IS
               BEGIN
                    R.I3 := R.I3 - 1;
               END PROCC4;

          BEGIN

               PROCC3 (PR.ALL);         -- (1,2,3)
               PROCC4 (PR.ALL);         -- (1,2,2)
               PROCC1 (PR.I2);          -- (1,7,2)
               PROCC2 (PR.I1);          -- (2,7,2)
               PR.I3 := PR.I3 + 7;      -- (2,7,9)
               IF (PR.ALL /= (2,7,9)) THEN
                    FAILED ("ASSIGNMENT TO COMPONENT OF RECORD" &
                            " ACCESS PARAMETER FAILED");
               END IF;
          END PROCC;

     BEGIN     -- (C)

          PR := NEW REC '(0,0,0);
          PROCC (PR);

     END;      -- (C)

     ---------------------------------------------

     RESULT;

END C62002A;
