-- CD5011Q.ADA

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
--     PRIVATE TYPE IN THE DECLARATIVE PART OF A BLOCK STATEMENT.

-- HISTORY:
--     JET 09/15/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH SPPRT13;

PROCEDURE CD5011Q IS

     PACKAGE P IS
          TYPE PRIV_TYPE IS PRIVATE;
          FUNCTION INT_TO_PRIV (I : INTEGER) RETURN PRIV_TYPE;
          FUNCTION EQUAL (P : PRIV_TYPE; I : INTEGER) RETURN BOOLEAN;
     PRIVATE
          TYPE PRIV_TYPE IS NEW INTEGER;
     END P;

     PACKAGE BODY P IS

          FUNCTION INT_TO_PRIV (I : INTEGER) RETURN PRIV_TYPE IS
               BEGIN
                    RETURN PRIV_TYPE(I);
               END;

          FUNCTION EQUAL (P : PRIV_TYPE; I : INTEGER) RETURN BOOLEAN IS
               BEGIN
                    RETURN (P = PRIV_TYPE(I));
               END;

     END P;

     USE P;

BEGIN

     TEST ("CD5011Q", "AN ADDRESS CLAUSE CAN BE " &
                      "GIVEN FOR A VARIABLE OF A PRIVATE " &
                      "TYPE IN THE DECLARATIVE PART OF A " &
                      "BLOCK STATEMENT");

     DECLARE

          PRIV : PRIV_TYPE := INT_TO_PRIV (12);
          FOR PRIV USE
               AT SPPRT13.VARIABLE_ADDRESS;

     BEGIN
          PRIV := INT_TO_PRIV (17);

          IF NOT EQUAL (PRIV, IDENT_INT (17)) THEN
               FAILED ("INCORRECT VALUE FOR VARIABLE OF PRIVATE TYPE");
          END IF;

          IF PRIV'ADDRESS /= SPPRT13.VARIABLE_ADDRESS THEN
               FAILED ("INCORRECT ADDRESS FOR VARIABLE OF " &
                       "PRIVATE TYPE");
          END IF;
     END;

     RESULT;

END CD5011Q;
