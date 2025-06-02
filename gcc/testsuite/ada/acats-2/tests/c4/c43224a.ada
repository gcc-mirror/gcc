-- C43224A.ADA

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
--     CHECK THAT A NON-STATIC CHOICE OF AN ARRAY AGGREGATE CAN BE A
--     'RANGE ATTRIBUTE.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43224A IS

     M, O : INTEGER := IDENT_INT(2);
     N : INTEGER := IDENT_INT(3);

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE D3_ARR IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>,
                          INTEGER RANGE <>) OF INTEGER;

     SUBTYPE ARR1 IS ARR(IDENT_INT(2) .. IDENT_INT(3));
     SUBTYPE ARR2 IS D3_ARR(1 .. M, 1 .. N, 1 ..O);

     SUB : ARR1;
     SUB1 : ARR2;

     PROCEDURE PROC(ARRY : IN OUT ARR) IS
     BEGIN
          ARRY := (ARR1'RANGE => IDENT_INT(7));
          IF ARRY(IDENT_INT(ARRY'FIRST)) /= IDENT_INT(7) THEN
               FAILED("RANGE NOT INITIALIZED - 1");
          END IF;
     END PROC;

     PROCEDURE PROC1(ARRY : IN OUT D3_ARR) IS
     BEGIN
          ARRY := (ARR2'RANGE(1) => (ARRY'RANGE(2) =>
                  (ARRY'RANGE(3) => IDENT_INT(7))));

          IF ARRY(IDENT_INT(1), IDENT_INT(2), IDENT_INT(1)) /=
                  IDENT_INT(7) THEN
               FAILED("RANGE NOT INITIALIZED - 2");
          END IF;
     END PROC1;

BEGIN
     TEST("C43224A", "CHECK THAT A NON-STATIC CHOICE OF AN ARRAY " &
                     "AGGREGATE CAN BE A 'RANGE ATTRIBUTE");

     PROC(SUB);
     PROC1(SUB1);

     RESULT;
END C43224A;
