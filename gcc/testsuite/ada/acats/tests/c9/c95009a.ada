-- C95009A.ADA

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
-- CHECK THAT A TASK OBJECT CAN CALL ENTRIES OF OTHER TASKS.

-- THIS TEST CONTAINS SHARED VARIABLES.

-- JRK 11/5/81
-- JRK 8/3/84

WITH REPORT; USE REPORT;
PROCEDURE C95009A IS

     V1 : INTEGER := 0;
     V2 : INTEGER := 0;

     PI : INTEGER := 0;
     PO : INTEGER := 0;

BEGIN
     TEST ("C95009A", "CHECK THAT A TASK OBJECT CAN CALL ENTRIES " &
                      "OF OTHER TASKS");

     DECLARE

          SUBTYPE INT IS INTEGER RANGE 1..5;

          TASK T1 IS
               ENTRY E1N;
               ENTRY EF1P (INT) (I : OUT INTEGER);
          END T1;

          TASK TYPE T2T IS
               ENTRY E2P (I : INTEGER);
               ENTRY EF2N (INT);
          END T2T;

          TYPE AT2T IS ACCESS T2T;
          AT2 : AT2T;

          TASK BODY T1 IS
          BEGIN
               V1 := 1;
               ACCEPT E1N;
               V1 := 2;
               AT2.E2P (1);
               V1 := 3;
               ACCEPT EF1P (2) (I : OUT INTEGER) DO
                    I := 2;
               END EF1P;
               V1 := 4;
               AT2.EF2N (IDENT_INT(3));
               V1 := 5;
          END T1;

          TASK BODY T2T IS
          BEGIN
               V2 := 1;
               T1.E1N;
               V2 := 2;
               ACCEPT E2P (I : INTEGER) DO
                    PI := I;
               END E2P;
               V2 := 3;
               T1.EF1P (2) (PO);
               V2 := 4;
               ACCEPT EF2N (1+IDENT_INT(2));
               V2 := 5;
          END T2T;

          PACKAGE DUMMY IS
          END DUMMY;

          PACKAGE BODY DUMMY IS
          BEGIN
               AT2 := NEW T2T;
          END DUMMY;

     BEGIN
          NULL;
     END;

     IF V1 /= 5 THEN
          FAILED ("TASK T1 ONLY REACHED V1 = " & INTEGER'IMAGE(V1));
     END IF;

     IF V2 /= 5 THEN
          FAILED ("TASK AT2 ONLY REACHED V2 = " & INTEGER'IMAGE(V2));
     END IF;

     IF PI /= 1 THEN
          FAILED ("ENTRY IN PARAMETER NOT PASSED CORRECTLY");
     END IF;

     IF PO /= 2 THEN
          FAILED ("ENTRY OUT PARAMETER NOT PASSED CORRECTLY");
     END IF;

     RESULT;
END C95009A;
