-- AC3206A.ADA

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
--     CHECK THAT AN INSTANTIATION IS LEGAL IF A FORMAL PRIVATE TYPE IS
--     USED IN  A CONSTANT DECLARATION AND THE ACTUAL PARAMETER IS A
--     TYPE WITH DISCRIMINANTS THAT DO AND DO NOT HAVE DEFAULTS. (CHECK
--     CASES THAT USED TO BE FORBIDDEN).

-- HISTORY:
--     DHH 09/16/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE AC3206A IS

BEGIN
     TEST ("AC3206A", "CHECK THAT AN INSTANTIATION IS LEGAL IF A " &
                      "FORMAL PRIVATE TYPE IS USED IN  A CONSTANT " &
                      "DECLARATION AND THE ACTUAL PARAMETER IS A " &
                      "TYPE WITH DISCRIMINANTS THAT DO AND DO NOT " &
                      "HAVE DEFAULTS");

     DECLARE             -- CHECK DEFAULTS LEGAL UNDER AI-37.

          GENERIC
               TYPE GEN IS PRIVATE;
               INIT : GEN;
          PACKAGE GEN_PACK IS
               CONST : CONSTANT GEN := INIT;
               SUBTYPE NEW_GEN IS GEN;
          END GEN_PACK;

          TYPE REC(A : INTEGER := 4) IS
               RECORD
                    X : INTEGER;
                    Y : BOOLEAN;
               END RECORD;

          PACKAGE P IS NEW GEN_PACK(REC, (4, 5, FALSE));
          USE P;

          CON : CONSTANT P.NEW_GEN := (4, 5, FALSE);

     BEGIN
          NULL;
     END;

     DECLARE

          GENERIC
               TYPE GEN(DIS : INTEGER) IS PRIVATE;
               INIT : GEN;
          PACKAGE GEN_PACK IS
               CONST : CONSTANT GEN := INIT;
               SUBTYPE NEW_GEN IS GEN(4);
          END GEN_PACK;

          TYPE REC(A : INTEGER := 4) IS
               RECORD
                    X : INTEGER;
                    Y : BOOLEAN;
               END RECORD;

          PACKAGE P IS NEW GEN_PACK(REC, (4, 5, FALSE));
          USE P;

          CON : CONSTANT P.NEW_GEN := (4, 5, FALSE);

     BEGIN
          NULL;
     END;

     DECLARE

          GENERIC
               TYPE GEN(DIS : INTEGER) IS PRIVATE;
               INIT : GEN;
          PACKAGE GEN_PACK IS
               CONST : CONSTANT GEN := INIT;
               SUBTYPE NEW_GEN IS GEN(4);
          END GEN_PACK;

          TYPE REC(A : INTEGER) IS
               RECORD
                    X : INTEGER;
                    Y : BOOLEAN;
               END RECORD;

          PACKAGE P IS NEW GEN_PACK(REC, (4, 5, FALSE));
          USE P;

          CON : CONSTANT P.NEW_GEN := (4, 5, FALSE);

     BEGIN
          NULL;
     END;

     RESULT;
END AC3206A;
