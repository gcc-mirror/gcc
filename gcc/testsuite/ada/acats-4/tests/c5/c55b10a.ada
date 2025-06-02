-- C55B10A.ADA

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
--     CHECK THAT, IN 'FOR I IN L .. R LOOP', IF EITHER L OR R IS AN
--     OVERLOADED ENUMERATION LITERAL, THE OVERLOADING IS CORRECTLY
--     RESOLVED AND THE LOOP PARAMETER HAS THE APPROPRIATE TYPE.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C55B10A IS

     TYPE ENUM IS (ALPH, BET, NEITHER);

     GLOBAL : ENUM := NEITHER;

     TYPE ALPHA IS (A, B, C, D, E);
     TYPE BETA IS (G, F, E, D, C);

     PROCEDURE VAR(DEC : ALPHA) IS
     BEGIN
          IF EQUAL(3, 3) THEN
               GLOBAL := ALPH;
          END IF;
     END;

     PROCEDURE VAR(DEC : BETA) IS
     BEGIN
          IF EQUAL(3, 3) THEN
               GLOBAL := BET;
          END IF;
     END;

BEGIN
     TEST("C55B10A", "CHECK THAT, IN 'FOR I IN L .. R LOOP', IF " &
                     "EITHER L OR R IS AN OVERLOADED ENUMERATION " &
                     "LITERAL, THE OVERLOADING IS CORRECTLY RESOLVED " &
                     "AND THE LOOP PARAMETER HAS THE APPROPRIATE TYPE");

     FOR I IN A .. E LOOP
          VAR(I);

          IF GLOBAL /= ALPH THEN
               FAILED("WRONG TYPE FOR ALPHA");
          END IF;
     END LOOP;

     FOR I IN G .. E LOOP
          VAR(I);

          IF GLOBAL /= BET THEN
               FAILED("WRONG TYPE FOR BETA");
          END IF;
     END LOOP;

     RESULT;
END C55B10A;
