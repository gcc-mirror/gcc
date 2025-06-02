-- C53007A.ADA

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
-- CHECK THAT CONTROL FLOWS CORRECTLY IN SIMPLE NESTED IF_STATEMENTS.

-- JRK 7/23/80
-- SPS 3/4/83

WITH REPORT;
PROCEDURE C53007A IS

     USE REPORT;

     CI1 : CONSTANT INTEGER := 1;
     CI9 : CONSTANT INTEGER := 9;
     CBT : CONSTANT BOOLEAN := TRUE;
     CBF : CONSTANT BOOLEAN := FALSE;

     VI1 : INTEGER := IDENT_INT(1);
     VI9 : INTEGER := IDENT_INT(9);
     VBT : BOOLEAN := IDENT_BOOL(TRUE);
     VBF : BOOLEAN := IDENT_BOOL(FALSE);

     FLOW_COUNT : INTEGER := 0;

BEGIN
     TEST ("C53007A", "CHECK THAT CONTROL FLOWS CORRECTLY IN SIMPLE " &
           "NESTED IF_STATEMENTS");

     IF VBF THEN  -- (FALSE)
          FAILED ("INCORRECT CONTROL FLOW 1");
     ELSIF CI9 < 20 THEN  -- (TRUE)
          FLOW_COUNT := FLOW_COUNT + 1;
          IF VI1 /= 0 AND TRUE THEN  -- (TRUE)
               FLOW_COUNT := FLOW_COUNT + 1;
          ELSE FAILED ("INCORRECT CONTROL FLOW 2");
          END IF;
     ELSE FAILED ("INCORRECT CONTROL FLOW 3");
     END IF;

     IF CBF OR ELSE VI9 = 9 THEN  -- (TRUE)
          IF VI1 + CI9 > 0 OR (CBF AND VBT) THEN  -- (TRUE)
               FLOW_COUNT := FLOW_COUNT + 1;
          END IF;
     ELSIF VBF OR VI1 > 10 THEN  -- (FALSE)
          FAILED ("INCORRECT CONTROL FLOW 4");
     END IF;

     IF NOT CBT AND THEN NOT VBT AND THEN CI9 < 0 THEN  -- (FALSE)
          IF FALSE OR NOT TRUE THEN  -- (FALSE)
               FAILED ("INCORRECT CONTROL FLOW 5");
          ELSIF VI1 >= 0 THEN  -- (TRUE)
               NULL;
          ELSE FAILED ("INCORRECT CONTROL FLOW 6");
          END IF;
          FAILED ("INCORRECT CONTROL FLOW 7");
     ELSIF (VI1 * CI9 + 3 < 0) OR (VBT AND NOT (CI1 < 0)) THEN -- (TRUE)
          FLOW_COUNT := FLOW_COUNT + 1;
          IF NOT CBT OR ELSE CI9 + 1 = 0 THEN  -- (FALSE)
               FAILED ("INCORRECT CONTROL FLOW 8");
          ELSE FLOW_COUNT := FLOW_COUNT + 1;
               IF VI1 * 2 > 0 THEN  -- (TRUE)
                    FLOW_COUNT := FLOW_COUNT + 1;
               ELSIF TRUE THEN  -- (TRUE)
                    FAILED ("INCORRECT CONTROL FLOW 9");
               ELSE NULL;
               END IF;
          END IF;
     ELSIF FALSE AND CBF THEN  -- (FALSE)
          FAILED ("INCORRECT CONTROL FLOW 10");
     ELSE IF VBT THEN  -- (TRUE)
               FAILED ("INCORRECT CONTROL FLOW 11");
          ELSIF VI1 = 0 THEN  -- (FALSE)
               FAILED ("INCORRECT CONTROL FLOW 12");
          ELSE FAILED ("INCORRECT CONTROL FLOW 13");
          END IF;
     END IF;

     IF 3 = 5 OR NOT VBT THEN  -- (FALSE)
          FAILED ("INCORRECT CONTROL FLOW 14");
          IF TRUE AND CBT THEN  -- (TRUE)
               FAILED ("INCORRECT CONTROL FLOW 15");
          ELSE FAILED ("INCORRECT CONTROL FLOW 16");
          END IF;
     ELSIF CBF THEN  -- (FALSE)
          IF VI9 >= 0 OR FALSE THEN  -- (TRUE)
               IF VBT THEN  -- (TRUE)
                    FAILED ("INCORRECT CONTROL FLOW 17");
               END IF;
               FAILED ("INCORRECT CONTROL FLOW 18");
          ELSIF VI1 + CI9 /= 0 THEN  -- (TRUE)
               FAILED ("INCORRECT CONTROL FLOW 19");
          END IF;
          FAILED ("INCORRECT CONTROL FLOW 20");
     ELSE IF VBT AND CI9 - 9 = 0 THEN  -- (TRUE)
               IF FALSE THEN  -- (FALSE)
                    FAILED ("INCORRECT CONTROL FLOW 21");
               ELSIF NOT VBF AND THEN CI1 > 0 THEN  -- (TRUE)
                    FLOW_COUNT := FLOW_COUNT + 1;
               ELSE FAILED ("INCORRECT CONTROL FLOW 22");
               END IF;
               FLOW_COUNT := FLOW_COUNT + 1;
          ELSIF NOT CBF OR VI1 /= 0 THEN  -- (TRUE)
               IF VBT THEN  -- (TRUE)
                    NULL;
               END IF;
               FAILED ("INCORRECT CONTROL FLOW 23");
          ELSE FAILED ("INCORRECT CONTROL FLOW 24");
          END IF;
          FLOW_COUNT := FLOW_COUNT + 1;
     END IF;

     IF FLOW_COUNT /= 9 THEN
          FAILED ("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;
END C53007A;
