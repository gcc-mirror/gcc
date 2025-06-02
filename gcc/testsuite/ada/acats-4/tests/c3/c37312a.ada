-- C37312A.ADA

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
--     CHECK THAT A DISCRIMINANT CAN HAVE A GENERIC FORMAL DISCRETE
--     TYPE WHEN IT DOES NOT GOVERN A VARIANT PART AND THAT AN
--     OBJECT OF A GENERIC FORMAL TYPE CAN CONSTRAIN A COMPONENT
--     IN A VARIANT PART.

-- HISTORY:
--     AH  08/22/86  CREATED ORIGINAL TEST.
--     JET 08/13/87  REVISED FROM CLASS 'A' TO CLASS 'C' TEST.

WITH REPORT; USE REPORT;

PROCEDURE C37312A IS

BEGIN
     TEST ("C37312A", "DISCRIMINANT TYPE IS GENERIC FORMAL TYPE");

     DECLARE
          TYPE T IS RANGE 1 ..5;

          GENERIC
               TYPE G1 IS RANGE <>;
          PACKAGE P IS
               TYPE G2 (D1 : G1) IS
                    RECORD
                         R1 : G1;
                         R2 : BOOLEAN;
                    END RECORD;

               TYPE STR IS ARRAY(G1 RANGE <>) OF INTEGER;
               TYPE G3 (D : G1; E : INTEGER) IS
                    RECORD
                         CASE E IS
                              WHEN 1 =>
                                   S1 : STR(G1'FIRST..D);
                              WHEN OTHERS =>
                                   S2 : INTEGER;
                         END CASE;
                    END RECORD;

          END P;

          PACKAGE PKG IS NEW P (G1 => T);
          USE PKG;

          A2: G2(1) := (1, 5, FALSE);
          A3: G3(5, 1) := (5, 1, (1, 2, 3, 4, 5));

     BEGIN
          A2.R2 := IDENT_BOOL (TRUE);
          A3.S1(1) := IDENT_INT (6);

          IF A2 /= (1, 5, TRUE) THEN
               FAILED ("INVALID CONTENTS OF RECORD A2");
          END IF;
          IF A3 /= (5, 1, (6, 2, 3, 4, 5)) THEN
               FAILED ("INVALID CONTENTS OF RECORD A3");
          END IF;
     END;

     RESULT;

END C37312A;
