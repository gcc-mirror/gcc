-- C83023A.ADA

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
--     CHECK THAT A DECLARATION IN A DECLARATIVE REGION OF A TASK
--     HIDES AN OUTER DECLARATION OF A HOMOGRAPH. ALSO CHECK THAT THE
--     OUTER DECLARATION IS DIRECTLY VISIBLE IN BOTH DECLARATIVE
--     REGIONS BEFORE THE DECLARATION OF THE INNER HOMOGRAPH AND THE
--     OUTER DECLARATION IS VISIBLE BY SELECTION AFTER THE INNER
--     HOMOGRAPH DECLARATION.

-- HISTORY:
--     BCB 08/29/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83023A IS

     GENERIC
          TYPE T IS PRIVATE;
          X : T;
     FUNCTION GEN_FUN RETURN T;

     FUNCTION GEN_FUN RETURN T IS
     BEGIN
          RETURN X;
     END GEN_FUN;

BEGIN
     TEST ("C83023A", "CHECK THAT A DECLARATION IN A DECLARATIVE " &
                      "REGION OF A TASK HIDES AN OUTER " &
                      "DECLARATION OF A HOMOGRAPH");

     ONE:
     DECLARE                      -- DECLARATIVE REGION.
          A : INTEGER := IDENT_INT(2);
          B : INTEGER := A;

          TASK INNER IS
               ENTRY HERE (X : IN OUT INTEGER);
          END INNER;

          TASK BODY INNER IS
               C : INTEGER := A;
               A : INTEGER := IDENT_INT(3);
          BEGIN
               ACCEPT HERE (X : IN OUT INTEGER) DO
                    IF A /= IDENT_INT(3) THEN
                         FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH" &
                                 " - 1");
                    END IF;

                    IF ONE.A /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH" &
                                 " - 2");
                    END IF;

                    IF ONE.B /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR OUTER VARIABLE " &
                                 "- 3");
                    END IF;

                    IF C /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR INNER VARIABLE " &
                                 "- 4");
                    END IF;

                    IF X /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE PASSED IN - 5");
                    END IF;

                    IF EQUAL(1,1) THEN
                         X := A;
                    ELSE
                         X := ONE.A;
                    END IF;
               END HERE;
          END INNER;

     BEGIN  -- ONE
          INNER.HERE(A);

          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 6");
          END IF;
     END ONE;

     TWO:
     DECLARE            -- AFTER THE SPECIFICATION OF TASK.
          TASK INNER IS
               ENTRY HERE (X : IN OUT INTEGER);
          END INNER;

          A : INTEGER := IDENT_INT(2);

          B : INTEGER := A;

          TASK BODY INNER IS
               C : INTEGER := A;
               A : INTEGER := IDENT_INT(3);
          BEGIN
               ACCEPT HERE (X : IN OUT INTEGER) DO
                    IF A /= IDENT_INT(3) THEN
                         FAILED ("INCORRECT VALUE FOR INNER HOMOGRAPH" &
                                 " - 10");
                    END IF;

                    IF TWO.A /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR OUTER HOMOGRAPH" &
                                 " - 11");
                    END IF;

                    IF TWO.B /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR OUTER VARIABLE " &
                                 "- 12");
                    END IF;

                    IF C /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE FOR INNER VARIABLE " &
                                 "- 13");
                    END IF;

                    IF X /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE PASSED IN - 14");
                    END IF;

                    IF EQUAL(1,1) THEN
                         X := A;
                    ELSE
                         NULL;
                    END IF;
               END HERE;
          END INNER;

     BEGIN  -- TWO
          INNER.HERE(A);

          IF A /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE PASSED OUT - 15");
          END IF;
     END TWO;

     THREE:
     DECLARE                 --  OVERLOADING OF FUNCTIONS.

          OBJ : INTEGER := 1;
          FLO : FLOAT := 5.0;

          FUNCTION F IS NEW GEN_FUN (INTEGER, OBJ);

          TASK INNER IS
               ENTRY HERE (X : IN OUT INTEGER);
          END INNER;

          FUNCTION F IS NEW GEN_FUN (FLOAT, FLO);

          TASK BODY INNER IS
               F : FLOAT := 6.25;
          BEGIN
               ACCEPT HERE (X : IN OUT INTEGER) DO
                    X := INTEGER(F);
               END HERE;
          END INNER;

     BEGIN
          INNER.HERE (OBJ);

          IF OBJ /= IDENT_INT(6) THEN
               FAILED ("INCORRECT VALUE RETURNED FROM FUNCTION - 20");
          END IF;
     END THREE;

     RESULT;
END C83023A;
