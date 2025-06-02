-- C38005A.ADA

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
-- CHECK THAT ALL (UNINITIALIZED) ACCESS OBJECTS ARE INITIALIZED
-- TO NULL BY DEFAULT. VARIABLES, ARRAYS, RECORDS, ARRAYS OF RECORDS,
-- ARRAYS OF ARRAYS, RECORDS WITH ARRAYS AND RECORD COMPONENTS
-- ARE ALL CHECKED.
-- FUNCTION RESULTS (I.E. RETURNED FROM IMPLICIT FUNCTION RETURN)
-- ARE NOT CHECKED.

-- DAT 3/6/81
-- VKG 1/5/83
-- SPS 2/17/83

WITH REPORT; USE REPORT;

PROCEDURE C38005A IS

     TYPE REC;
     TYPE ACC_REC IS ACCESS REC;
     TYPE VECTOR IS ARRAY ( NATURAL RANGE <> ) OF ACC_REC;
     TYPE REC IS RECORD
          VECT : VECTOR (3 .. 5);
     END RECORD;

     TYPE ACC_VECT IS ACCESS VECTOR;
     TYPE ARR_REC IS ARRAY (1 .. 2) OF REC;
     TYPE REC2;
     TYPE ACC_REC2 IS ACCESS REC2;
     TYPE REC2 IS RECORD
          C1 : ACC_REC;
          C2 : ACC_VECT;
          C3 : ARR_REC;
          C4 : REC;
          C5 : ACC_REC2;
     END RECORD;

     N_REC      : REC;
     N_ACC_REC  : ACC_REC;
     N_VEC      : VECTOR (3 .. IDENT_INT (5));
     N_ACC_VECT : ACC_VECT;
     N_ARR_REC  : ARR_REC;
     N_REC2     : REC2;
     N_ACC_REC2 : ACC_REC2;
     N_ARR      : ARRAY (1..2) OF VECTOR (1..2);
     Q : REC2 :=
                (C1 => NEW REC,
                 C2 => NEW VECTOR'(NEW REC, NEW REC'(N_REC)),
                 C3 => (1 | 2 => (VECT=>(3|4=> NEW REC, 
                                         5=>N_ACC_REC)
                                 )),
                 C4 => N_REC2.C4,
                 C5 => NEW REC2'(N_REC2));

BEGIN
     TEST ("C38005A", "DEFAULT VALUE FOR ACCESS OBJECTS IS NULL");

     IF N_REC /= REC'(VECT => (3..5 => NULL))
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 1");
     END IF;

     IF N_ACC_REC /= NULL
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 2");
     END IF;

     IF N_VEC /= N_REC.VECT
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 3");
     END IF;

     IF N_ARR /= ((NULL, NULL), (NULL, NULL))
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 4");
     END IF;

     IF N_ACC_VECT /= NULL
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 5");
     END IF;

     IF N_ARR_REC /= (N_REC, N_REC)
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 6");
     END IF;

     IF N_REC2 /= (NULL, NULL, N_ARR_REC, N_REC, NULL)
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 7");
     END IF;

     IF N_ACC_REC2 /= NULL
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 8");
     END IF;

     IF Q /= (Q.C1, Q.C2, (Q.C3(1), Q.C3(2)), N_REC, Q.C5)
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 9");
     END IF;

     IF Q.C1.ALL /= N_REC
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 10");
     END IF;

     IF Q.C2.ALL(0).ALL /= N_REC
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 11");
     END IF;

     IF Q.C2(1).VECT /= N_VEC
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 12");
     END IF;

     IF Q.C3(2).VECT /= (3 => Q.C3(2).VECT(3),
                         4 => Q.C3(2).VECT(4),
                         5=>NULL)
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 13");
     END IF;

     IF Q.C3(2).VECT(3).ALL /= N_REC
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 14");
     END IF;

     IF Q.C5.ALL /= N_REC2
     THEN
          FAILED ("INCORRECT ACCESS TYPE INITIALIZATION - 15");
     END IF;

     DECLARE
          PROCEDURE T (R : OUT REC2) IS
          BEGIN
               NULL;
          END T;
     BEGIN
          N_REC2 := Q;
          T(Q);
          IF Q /= N_REC2 THEN
               FAILED ("INCORRECT OUT PARM INIT 2");
          END IF;
     END;

     RESULT;
END C38005A;
