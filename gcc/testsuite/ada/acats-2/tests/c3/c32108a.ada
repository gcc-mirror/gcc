-- C32108A.ADA

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
-- CHECK THAT DEFAULT EXPRESSIONS ARE NOT EVALUATED, IF INITIALIZATION
-- EXPRESSIONS ARE GIVEN FOR THE OBJECT DECLARATIONS.

-- TBN 3/20/86

WITH REPORT; USE REPORT;
PROCEDURE C32108A IS

     FUNCTION DEFAULT_CHECK (NUMBER : INTEGER) RETURN INTEGER IS
     BEGIN
          IF NUMBER /= 0 THEN
               FAILED ("DEFAULT EXPRESSIONS ARE EVALUATED -" &
                       INTEGER'IMAGE (NUMBER));
          END IF;
          RETURN (1);
     END DEFAULT_CHECK;

BEGIN
     TEST ("C32108A", "CHECK THAT DEFAULT EXPRESSIONS ARE NOT " &
                      "EVALUATED, IF INITIALIZATION EXPRESSIONS ARE " &
                      "GIVEN FOR THE OBJECT DECLARATIONS");

     DECLARE     -- (A)

          TYPE REC_TYP1 IS
               RECORD
                    AGE : INTEGER := DEFAULT_CHECK (1);
               END RECORD;

          REC1 : REC_TYP1 := (AGE => DEFAULT_CHECK (0));


          TYPE REC_TYP2 (D : INTEGER := DEFAULT_CHECK (2)) IS
               RECORD
                    NULL;
               END RECORD;

          REC2 : REC_TYP2 (DEFAULT_CHECK (0));


          TYPE REC_TYP3 (D : INTEGER := DEFAULT_CHECK (3)) IS
               RECORD
                    A : INTEGER := DEFAULT_CHECK (4);
               END RECORD;

          REC3 : REC_TYP3 := (D => DEFAULT_CHECK (0),
                              A => DEFAULT_CHECK (0));

     BEGIN     -- (A)
          NULL;
     END;      -- (A)

     RESULT;
END C32108A;
