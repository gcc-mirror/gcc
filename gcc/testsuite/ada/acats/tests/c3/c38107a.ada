-- C38107A.ADA

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
--     FOR AN INCOMPLETE TYPE WITH DISCRIMINANTS DECLARED IN THE
--     VISIBLE PART OF A PACKAGE OR IN A DECLARATIVE PART, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF A DISCRIMINANT CONSTRAINT IS
--     SPECIFIED FOR THE TYPE AND ONE OF THE DISCRIMINANT VALUES DOES
--     NOT BELONG TO THE CORRESPONDING DISCRIMINANT'S SUBTYPE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C38107A IS

BEGIN
     TEST ("C38107A", "FOR AN INCOMPLETE TYPE WITH DISCRIMINANTS " &
                      "DECLARED IN THE VISIBLE PART OF A PACKAGE OR " &
                      "IN A DECLARATIVE PART, CHECK THAT CONSTRAINT_" &
                      "ERROR IS RAISED IF A DISCRIMINANT CONSTRAINT " &
                      "IS SPECIFIED FOR THE TYPE AND ONE OF THE " &
                      "DISCRIMINANT VALUES DOES NOT BELONG TO THE " &
                      "CORRESPONDING DISCRIMINANT'S SUBTYPE");

     BEGIN
          DECLARE
               PACKAGE P IS
                    SUBTYPE INT6 IS INTEGER RANGE 1 .. 6;
                    TYPE T_INT6 (D6 : INT6);
                    TYPE TEST IS ACCESS T_INT6(7);  -- CONSTRAINT_ERROR.
                    TYPE T_INT6 (D6 : INT6) IS
                         RECORD
                              NULL;
                         END RECORD;
               END P;
               USE P;
          BEGIN
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 1");
               DECLARE
                    T : P.TEST := NEW T_INT6(7);
               BEGIN
                    IF EQUAL(T.D6, T.D6) THEN
                         COMMENT ("DON'T OPTIMIZE T.D6");
                    END IF;
               END;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 1");
     END;

     BEGIN
          DECLARE
               SUBTYPE INT7 IS INTEGER RANGE 1 .. 7;
               TYPE T_INT7 (D7 : INT7);
               TYPE TEST IS ACCESS T_INT7(8);       -- CONSTRAINT_ERROR.
               TYPE T_INT7 (D7 : INT7) IS
                    RECORD
                         NULL;
                    END RECORD;
          BEGIN
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 2");
               DECLARE
                    T : TEST := NEW T_INT7(6);
               BEGIN
                    IF EQUAL(T.D7, T.D7) THEN
                         COMMENT ("DON'T OPTIMIZE T.D7");
                    END IF;
               END;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 2");
     END;
     RESULT;
END C38107A;
