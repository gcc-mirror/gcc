-- C37206A.ADA

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
--     FOR A TYPE WITHOUT DEFAULT DISCRIMINANT VALUES (BUT WITH
--     DISCRIMINANTS) CHECK THAT A TYPEMARK WHICH DENOTES SUCH AN
--     UNCONSTRAINED TYPE CAN BE USED IN:

--      1) A SUBTYPE DECLARATION, AND THE SUBTYPE NAME ACTS SIMPLY AS A
--         NEW NAME FOR THE UNCONSTRAINED TYPE;
--      2) IN A CONSTANT DECLARATION.

-- HISTORY:
--     AH  08/21/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     DTN 11/13/91 DELETED SUBPARTS (2 and 3).

WITH REPORT; USE REPORT;
PROCEDURE C37206A IS
BEGIN

     TEST ("C37206A", "FOR TYPE WITH DEFAULT-LESS DISCRIMINANTS, " &
           "UNCONSTRAINED TYPE_MARK CAN BE USED IN A SUBTYPE " &
           "DECLARATION OR IN A CONSTANT DECLARATION");

     DECLARE
          TYPE REC(DISC : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          SUBTYPE ST IS REC;                               -- 1.

          C1 : CONSTANT REC := (DISC => 5);                -- 2.
          C2 : CONSTANT REC := (DISC => IDENT_INT(5));     -- 2.
     BEGIN

          IF C1 /= C2 OR C1 /= (DISC => 5) THEN
               FAILED ("CONSTANT DECLARATIONS INCORRECT");
          END IF;
     END;

     RESULT;
END C37206A;
