-- C35503G.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULT WHEN THE
--     PREFIX IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35503G IS

BEGIN
     TEST ("C35503G", "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
                      "CORRECT RESULT WHEN THE PREFIX IS AN " &
                      "INTEGER TYPE" );

     DECLARE
          TYPE INT IS RANGE -6 .. 6;
          SUBTYPE SINT IS INT RANGE -4 .. 4;

     BEGIN

          FOR I IN INT'FIRST + 1 .. INT'LAST LOOP
               BEGIN
                    IF SINT'PRED (I) /= I - 1 THEN
                         FAILED ( "WRONG SINT'PRED FOR " &
                                  INT'IMAGE (I));
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR " &
                                  "SINT'PRED OF " &
                                  INT'IMAGE (I));
               END;
          END LOOP;

          FOR I IN INT'FIRST .. INT'LAST - 1 LOOP
               BEGIN
                    IF SINT'SUCC (I) /= I + 1 THEN
                         FAILED ( "WRONG SINT'SUCC FOR " &
                                  INT'IMAGE (I));
                         END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR " &
                                  "SINT'SUCC OF " &
                                  INT'IMAGE (I));
               END;
          END LOOP;

     END;

     DECLARE
          SUBTYPE INTRANGE IS INTEGER RANGE IDENT_INT(-6) ..
                                                          IDENT_INT(6);
          SUBTYPE SINTEGER IS INTEGER RANGE IDENT_INT(-4) ..
                                                          IDENT_INT(4);

     BEGIN
          FOR I IN INTRANGE LOOP
               BEGIN
                    IF SINTEGER'PRED (I) /= I - IDENT_INT(1) THEN
                         FAILED ( "WRONG SINTEGER'PRED FOR " &
                                  INTEGER'IMAGE (I));
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR " &
                                  "SINTEGER'PRED OF " &
                                  INTEGER'IMAGE (I));
               END;
               BEGIN
                    IF SINTEGER'SUCC (I) /= I + IDENT_INT(1) THEN
                         FAILED ( "WRONG SINTEGER'SUCC FOR " &
                                  INTEGER'IMAGE (I));
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED FOR " &
                                  "SINTEGER'SUCC OF " &
                                  INTEGER'IMAGE (I));
               END;
          END LOOP;

     END;

     RESULT;
END C35503G;
