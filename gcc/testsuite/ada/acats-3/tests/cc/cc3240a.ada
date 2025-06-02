-- CC3240A.ADA

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
--      CHECK THAT A FORMAL PRIVATE AND LIMITED PRIVATE TYPE DENOTES ITS
--      ACTUAL PARAMETER, AND OPERATIONS OF THE FORMAL TYPE ARE
--      IDENTIFIED WITH CORRESPONDING OPERATIONS OF THE ACTUAL TYPE
--      WHEN THE FORMAL TYPE IS A TYPE WITH DISCRIMINANTS.

-- HISTORY:
--      RJW 10/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CC3240A IS

BEGIN
     TEST ("CC3240A", "CHECK THAT A FORMAL PRIVATE OR LIMITED " &
                      "PRIVATE TYPE DENOTES ITS ACTUAL PARAMETER AND " &
                      "OPERATIONS OF THE FORMAL TYPE ARE IDENTIFIED " &
                      "WITH CORRESPONDING OPERATIONS OF THE ACTUAL " &
                      "TYPE, WHEN THE FORMAL TYPE IS A TYPE " &
                      "WITH DISCRIMINANTS");

     DECLARE

          GENERIC
               TYPE T(A : INTEGER) IS PRIVATE;
          PACKAGE P IS
               SUBTYPE S IS T;
               TX : T(5);
          END P;

          TYPE REC (L : INTEGER) IS
               RECORD
                    A : INTEGER;
               END RECORD;

          PACKAGE P1 IS NEW P (REC);
          USE P1;

     BEGIN
          TX := (L => 5, A => 7);
          IF NOT (TX IN REC) THEN
               FAILED ("MEMBERSHIP TEST - PRIVATE");
          END IF;

          IF TX.A /= 7 OR TX.L /= 5 THEN
               FAILED ("SELECTED COMPONENTS - PRIVATE");
          END IF;

          IF S(TX) /= REC(TX) THEN
               FAILED ("EXPLICIT CONVERSION - PRIVATE");
          END IF;

          IF NOT TX'CONSTRAINED THEN
               FAILED ("'CONSTRAINED - PRIVATE");
          END IF;
     END;

     DECLARE
          TYPE REC(L : INTEGER) IS
               RECORD
                    A : INTEGER;
               END RECORD;

          GENERIC
               TYPE T(A : INTEGER) IS LIMITED PRIVATE;
               TX : IN OUT T;
          PACKAGE LP IS
               SUBTYPE S IS T;
          END LP;

          R : REC (5) := (5, 7);

          PACKAGE BODY LP IS
          BEGIN
               IF (TX IN S) /= (R IN REC) THEN
                    FAILED ("MEMBERSHIP TEST - LIMITED PRIVATE");
               END IF;

               IF TX.A /= 5 THEN
                    FAILED ("SELECTED COMPONENTS - LIMITED PRIVATE");
               END IF;

               IF (S(TX) IN S) /= (REC(R) IN REC) THEN
                    FAILED ("EXPLICIT CONVERSION - LIMITED PRIVATE");
               END IF;

               IF NOT TX'CONSTRAINED THEN
                    FAILED ("'CONSTRAINED - LIMITED PRIVATE");
               END IF;
          END LP;

          PACKAGE P1 IS NEW LP (REC, R);
          USE P1;
     BEGIN
          NULL;
     END;

     RESULT;
END CC3240A;
