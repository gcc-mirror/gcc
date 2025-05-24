-- C35503K.ADA

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
--     CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.
--     PWN 11/30/94 REMOVED ATTRIBUTE TESTS ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C35503K IS

BEGIN
     TEST ("C35503K", "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
                      "INTEGER TYPE" );

     DECLARE
          TYPE INT IS RANGE -6 .. 6;
          SUBTYPE SINT IS INT RANGE -4 .. 4;

          PROCEDURE P (I : INTEGER; STR : STRING) IS
          BEGIN
               BEGIN
                    IF INTEGER'POS (I) /= I THEN
                         FAILED ( "WRONG POS FOR " & STR);
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR POS OF " &
                                   STR);
               END;
               BEGIN
                    IF INTEGER'VAL (I) /= I THEN
                         FAILED ( "WRONG VAL FOR " & STR);
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR VAL OF " &
                                   STR);
               END;
          END P;

     BEGIN
          P ( INTEGER'FIRST, "INTEGER'FIRST");
          P ( INTEGER'LAST,  "INTEGER'LAST");
          P ( 0, "'0'");

          FOR I IN INT'FIRST .. INT'LAST LOOP
               BEGIN
                    IF SINT'POS (I) /= I THEN
                         FAILED ( "WRONG POS FOR "
                                   & INT'IMAGE (I));
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR POS OF "
                                  & INT'IMAGE (I));
               END;
               BEGIN
                    IF SINT'VAL (I) /= I THEN
                         FAILED ( "WRONG VAL FOR "
                                  & INT'IMAGE (I));
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR VAL OF "
                                  & INT'IMAGE (I));
               END;
          END LOOP;

          BEGIN
               IF INT'VAL (INTEGER'(0)) /= 0 THEN
                    FAILED ( "WRONG VAL FOR INT WITH INTEGER" );
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED FOR VAL OF " &
                             "INT WITH INTEGER" );
          END;

          BEGIN
               IF INTEGER'VAL (INT'(0)) /= 0 THEN
                    FAILED ( "WRONG VAL FOR INTEGER WITH INT" );
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED FOR VAL OF " &
                             "INTEGER WITH INT" );
          END;
     END;

     RESULT;
END C35503K;
