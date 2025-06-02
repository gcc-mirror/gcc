-- CD5011S.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF A
--     LIMITED PRIVATE TYPE IN THE DECLARATIVE PART OF A SUBPROGRAM.

-- HISTORY:
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH SPPRT13;

PROCEDURE CD5011S IS

     PACKAGE P IS
          TYPE LIMP_TYPE IS LIMITED PRIVATE;
          PROCEDURE TEST_LIMP (LIMP : IN OUT LIMP_TYPE);
     PRIVATE
          TYPE LIMP_TYPE IS ARRAY (1 .. 10) OF INTEGER;
     END P;

     PACKAGE BODY P IS
          PROCEDURE TEST_LIMP (LIMP : IN OUT LIMP_TYPE) IS
          BEGIN
               FOR I IN LIMP'RANGE LOOP
                    LIMP (I) := IDENT_INT (I);
               END LOOP;

               FOR I IN LIMP'RANGE LOOP
                    IF LIMP (I) /= I THEN
                         FAILED ("INCORRECT VALUE FOR ELEMENT" &
                                 INTEGER'IMAGE (I));
                    END IF;
               END LOOP;
          END TEST_LIMP;
     END P;

     USE P;

     PROCEDURE CD5011S_PROC IS

          LIMP : LIMP_TYPE;
          FOR LIMP USE
               AT SPPRT13.VARIABLE_ADDRESS;

     BEGIN
          TEST_LIMP (LIMP);

          IF LIMP'ADDRESS /= SPPRT13.VARIABLE_ADDRESS THEN
               FAILED ("WRONG ADDRESS FOR VARIABLE OF A LIMITED " &
                       "PRIVATE TYPE");
          END IF;
     END;

BEGIN
     TEST ("CD5011S", "AN ADDRESS CLAUSE CAN BE " &
                      "GIVEN FOR A VARIABLE OF A LIMITED " &
                      "PRIVATE TYPE IN THE DECLARATIVE PART " &
                      "OF A SUBPROGRAM");

     CD5011S_PROC;

     RESULT;

END CD5011S;
