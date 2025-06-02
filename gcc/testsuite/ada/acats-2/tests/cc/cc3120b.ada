-- CC3120B.ADA

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
-- CHECK THAT TASKS ARE NOT COPIED AS GENERIC IN OUT PARMS.

-- DAT 8/27/81
-- SPS 4/6/82
-- JBG 3/23/83

WITH REPORT; USE REPORT;

PROCEDURE CC3120B IS
BEGIN
     TEST ("CC3120B", "TASKS ARE NOT COPIED AS GENERIC PARAMETERS");

     DECLARE
          PACKAGE P IS
               TYPE T IS LIMITED PRIVATE;
               PROCEDURE UPDT (TPARM: IN T; I : IN OUT INTEGER);
          PRIVATE
               TASK TYPE T1 IS
                    ENTRY GET (I : OUT INTEGER);
                    ENTRY PUT (I : IN INTEGER);
               END T1;
               TYPE T IS RECORD
                    C : T1;
               END RECORD;
          END P;
          USE P;
          TT : T;
          GENERIC
               TYPE T IS LIMITED PRIVATE;
               T1 : IN OUT T;
               WITH PROCEDURE UPDT (TPARM : IN T; I: IN OUT INTEGER)
                    IS <> ;
          PROCEDURE PR;

          PROCEDURE PR IS
               I : INTEGER;
          BEGIN
               I := 5;
                                        -- PR.I
                                        -- UPDT.I      UPDT.T1.I
                                        --   5            4
               UPDT (T1, I); 
                                        --   4            5
               IF I /= 4 THEN
                    FAILED ("BAD VALUE 1");
               END IF;
               I := 6;
                                        --   6            5
               UPDT (T1, I);
                                        --   5            6
               IF I /= 5 THEN
                    FAILED ("BAD VALUE 3");
               END IF;
               RAISE TASKING_ERROR;
               FAILED ("INCORRECT RAISE STATEMENT");
          END PR;

          PACKAGE BODY P IS
               PROCEDURE UPDT (TPARM : IN T; I : IN OUT INTEGER) IS
                    V : INTEGER := I;
                    -- UPDT.I => V
                    -- T1.I => UPDT.I
                    -- V => T1.I
               BEGIN
                    TPARM.C.GET (I);
                    TPARM.C.PUT (V);
               END UPDT;

               TASK BODY T1 IS
                    I : INTEGER;
               BEGIN
                    I := 1;
                    LOOP
                         SELECT
                              ACCEPT GET (I : OUT INTEGER) DO
                                   I := T1.I;
                              END GET;
                         OR
                              ACCEPT PUT (I : IN INTEGER) DO
                                   T1.I := I;
                              END PUT;
                         OR
                              TERMINATE;
                         END SELECT;
                    END LOOP;
               END T1;
          END P;
     BEGIN
          DECLARE
               X : INTEGER := 2;
               PROCEDURE PPP IS NEW PR (T, TT);
          BEGIN
                                        -- X
                                        -- UPDT.I      UPDT.T1.I
                                        --   2            1
               UPDT (TT, X);
                                        --   1            2
               X := X + 3;
                                        --   4            2
               UPDT (TT, X);
                                        --   2            4
               IF X /= 2 THEN 
                    FAILED ("WRONG VALUE FOR X");
               END IF;
               BEGIN
                    PPP;
                    FAILED ("PPP NOT CALLED");
               EXCEPTION
                    WHEN TASKING_ERROR => NULL;
               END;
               X := 12;
                                        --   12           6
               UPDT (TT, X);
                                        --   6            12
               IF X /= 6 THEN
                    FAILED ("WRONG FINAL VALUE IN TASK");
               END IF;
          END;
     END;

     RESULT;
END CC3120B;
