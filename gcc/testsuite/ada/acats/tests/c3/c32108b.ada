-- C32108B.ADA

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
-- CHECK THAT IF A DEFAULT EXPRESSION IS EVALUATED FOR A COMPONENT, NO
-- DEFAULT EXPRESSIONS ARE EVALUATED FOR ANY SUBCOMPONENTS.

-- TBN 3/21/86

WITH REPORT; USE REPORT;
PROCEDURE C32108B IS

     FUNCTION DEFAULT_CHECK (NUMBER : INTEGER) RETURN INTEGER IS
     BEGIN
          IF NUMBER /= 0 THEN
               FAILED ("SUBCOMPONENT DEFAULT EXPRESSIONS ARE " &
                       "EVALUATED -" & INTEGER'IMAGE (NUMBER));
          END IF;
          RETURN (1);
     END DEFAULT_CHECK;

BEGIN
     TEST ("C32108B", "CHECK THAT IF A DEFAULT EXPRESSION IS " &
                      "EVALUATED FOR A COMPONENT, NO DEFAULT " &
                      "EXPRESSIONS ARE EVALUATED FOR ANY " &
                      "SUBCOMPONENTS");

     DECLARE     -- (A)

          TYPE REC_TYP1 IS
               RECORD
                    AGE : INTEGER := DEFAULT_CHECK (1);
               END RECORD;

          TYPE REC_TYP2 (D : INTEGER := DEFAULT_CHECK(2)) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE REC_TYP3 (D : INTEGER := DEFAULT_CHECK(3)) IS
               RECORD
                    A : INTEGER := DEFAULT_CHECK(4);
               END RECORD;

          TYPE REC_TYP4 IS
               RECORD
                    ONE : REC_TYP1 := (AGE => DEFAULT_CHECK (0));
                    TWO : REC_TYP2 (DEFAULT_CHECK(0));
                    THREE : REC_TYP3 := (D => DEFAULT_CHECK (0),
                                         A => DEFAULT_CHECK (0));
               END RECORD;

          REC4 : REC_TYP4;

     BEGIN     -- (A)
          NULL;
     END;      -- (A)

     RESULT;
END C32108B;
