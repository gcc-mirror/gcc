-- CC3019A.ADA

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
-- CHECK THAT INSTANTIATIONS OF NESTED GENERIC UNITS ARE PROCESSED
-- CORRECTLY.

-- JBG 11/6/85

GENERIC
     TYPE ELEMENT_TYPE IS PRIVATE;
PACKAGE CC3019A_QUEUES IS

     TYPE QUEUE_TYPE IS PRIVATE;

     PROCEDURE ADD (TO_Q : IN OUT QUEUE_TYPE;
                    VALUE : ELEMENT_TYPE);

     GENERIC
          WITH PROCEDURE APPLY (VAL : ELEMENT_TYPE);
     PROCEDURE ITERATOR (TO_Q : QUEUE_TYPE);

PRIVATE

     TYPE CONTENTS_TYPE IS ARRAY (1..3) OF ELEMENT_TYPE;
     TYPE QUEUE_TYPE IS
          RECORD
               CONTENTS : CONTENTS_TYPE;
               SIZE     : NATURAL := 0;
          END RECORD;

END CC3019A_QUEUES;

PACKAGE BODY CC3019A_QUEUES IS

     PROCEDURE ADD (TO_Q : IN OUT QUEUE_TYPE;
                    VALUE : ELEMENT_TYPE) IS
     BEGIN
          TO_Q.SIZE := TO_Q.SIZE + 1;
          TO_Q.CONTENTS(TO_Q.SIZE) := VALUE;
     END ADD;

--   GENERIC
--        WITH PROCEDURE APPLY (VAL : ELEMENT_TYPE);
     PROCEDURE ITERATOR (TO_Q : QUEUE_TYPE) IS
     BEGIN
          FOR I IN TO_Q.CONTENTS'FIRST .. TO_Q.SIZE LOOP
               APPLY (TO_Q.CONTENTS(I));
          END LOOP;
     END ITERATOR;

END CC3019A_QUEUES;

WITH REPORT; USE REPORT;
WITH CC3019A_QUEUES;
PROCEDURE CC3019A IS

     SUBTYPE STR6 IS STRING (1..6);

     TYPE STR6_ARR IS ARRAY (1..3) OF STR6;
     STR6_VALS : STR6_ARR := ("111111", "222222",
                               IDENT_STR("333333"));
     CUR_STR_INDEX : NATURAL := 1;

     TYPE INT_ARR IS ARRAY (1..3) OF INTEGER;
     INT_VALS : INT_ARR := (-1, 3, IDENT_INT(3));
     CUR_INT_INDEX : NATURAL := 1;

-- THIS PROCEDURE IS CALLED ONCE FOR EACH ELEMENT OF THE QUEUE
--
     PROCEDURE CHECK_STR (VAL : STR6) IS
     BEGIN
          IF VAL /= STR6_VALS(CUR_STR_INDEX) THEN
               FAILED ("STR6 ITERATOR FOR INDEX =" &
                       INTEGER'IMAGE(CUR_STR_INDEX) & " WITH VALUE " &
                       """" & VAL & """");
          END IF;
          CUR_STR_INDEX := CUR_STR_INDEX + 1;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("STR6 - CONSTRAINT_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("STR6 - UNEXPECTED EXCEPTION");
     END CHECK_STR;

     PROCEDURE CHECK_INT (VAL : INTEGER) IS
     BEGIN
          IF VAL /= INT_VALS(CUR_INT_INDEX) THEN
               FAILED ("INTEGER ITERATOR FOR INDEX =" &
                       INTEGER'IMAGE(CUR_INT_INDEX) & " WITH VALUE " &
                       """" & INTEGER'IMAGE(VAL) & """");
          END IF;
          CUR_INT_INDEX := CUR_INT_INDEX + 1;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("INTEGER - CONSTRAINT_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("INTEGER - UNEXPECTED EXCEPTION");
     END CHECK_INT;

     PACKAGE STR6_QUEUE IS NEW CC3019A_QUEUES (STR6);
     USE STR6_QUEUE;

     PACKAGE INT_QUEUE IS NEW CC3019A_QUEUES (INTEGER);
     USE INT_QUEUE;

BEGIN

     TEST ("CC3019A", "CHECK NESTED GENERICS - ITERATORS");

     DECLARE
          Q1 : STR6_QUEUE.QUEUE_TYPE;

          PROCEDURE CHK_STR IS NEW STR6_QUEUE.ITERATOR (CHECK_STR);

     BEGIN

          ADD (Q1, "111111");
          ADD (Q1, "222222");
          ADD (Q1, "333333");

          CUR_STR_INDEX := 1;
          CHK_STR (Q1);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - Q1");
     END;

-- REPEAT FOR INTEGERS

     DECLARE
          Q2 : INT_QUEUE.QUEUE_TYPE;

          PROCEDURE CHK_INT IS NEW INT_QUEUE.ITERATOR (CHECK_INT);

     BEGIN

          ADD (Q2, -1);
          ADD (Q2, 3);
          ADD (Q2, 3);

          CUR_INT_INDEX := 1;
          CHK_INT (Q2);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - Q2");
     END;

     RESULT;

END CC3019A;
