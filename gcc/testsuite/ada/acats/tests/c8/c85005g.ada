-- C85005G.ADA

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
--     CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY THE TYPE MARK USED
--     IN THE RENAMING DECLARATION IS IGNORED, AND THE SUBTYPE
--     CONSTRAINT ASSOCIATED WITH THE RENAMED VARIABLE IS USED INSTEAD.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE C85005G IS

     SUBTYPE INT IS INTEGER RANGE -100 .. 100;

     I : INTEGER := IDENT_INT(INTEGER'LAST);
     J : INT := IDENT_INT(INT'LAST);

     DG1 : INTEGER := IDENT_INT(INTEGER'LAST);
     DG2 : INT := IDENT_INT(INT'LAST);

     XI : INT RENAMES I;
     XJ : INTEGER RENAMES J;

     GENERIC
          G1 : IN OUT INT;
          G2 : IN OUT INTEGER;
     PROCEDURE GEN;

     PROCEDURE GEN IS
          XG1 : INT RENAMES G1;
          XG2 : INTEGER RENAMES G2;
     BEGIN
          IF XG1 /= INTEGER'LAST THEN
               FAILED("INCORRECT VALUE OF RENAMING VARIABLE - G1");
          END IF;

          XG1 := IDENT_INT(INTEGER'FIRST);

          IF XG1 /= INTEGER'FIRST THEN
               FAILED("INCORRECT VALUE OF RENAMING VARIABLE - G2");
          END IF;

          IF XG2 /= INT'LAST THEN
               FAILED("INCORRECT VALUE OF RENAMING VARIABLE - G3");
          END IF;

          XG2 := IDENT_INT(INT'FIRST);

          IF XG2 /= INT'FIRST THEN
               FAILED("INCORRECT VALUE OF RENAMING VARIABLE - G4");
          END IF;

          BEGIN
               XG2 := IDENT_INT(INTEGER'LAST);
               FAILED ("NO EXCEPTION RAISED BY XG2 := INTEGER'LAST");
               IF NOT EQUAL(XG2,XG2) THEN
                    COMMENT ("DON'T OPTIMIZE XG2");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION (G)");
          END;
     END GEN;

     PROCEDURE PROC IS NEW GEN(DG1, DG2);

BEGIN
     TEST ("C85005G", "CHECK THAT ANY SUBTYPE CONSTRAINT IMPOSED BY " &
                      "THE TYPE MARK USED IN THE RENAMING " &
                      "DECLARATION IS IGNORED, AND THE SUBTYPE " &
                      "CONSTRAINT ASSOCIATED WITH THE RENAMED " &
                      "VARIABLE IS USED INSTEAD");

     IF XI /= INTEGER'LAST THEN
          FAILED("INCORRECT VALUE OF RENAMING VARIABLE - 1");
     END IF;

     XI := IDENT_INT(INTEGER'FIRST);

     IF XI /= INTEGER'FIRST THEN
          FAILED("INCORRECT VALUE OF RENAMING VARIABLE - 2");
     END IF;

     IF XJ /= INT'LAST THEN
          FAILED("INCORRECT VALUE OF RENAMING VARIABLE - 3");
     END IF;

     XJ := IDENT_INT(INT'FIRST);

     IF XJ /= INT'FIRST THEN
          FAILED("INCORRECT VALUE OF RENAMING VARIABLE - 4");
     END IF;

     BEGIN
          XJ := IDENT_INT(INTEGER'LAST);
          FAILED ("NO EXCEPTION RAISED BY XJ := INTEGER'LAST");
          IF NOT EQUAL(XJ,XJ) THEN
               COMMENT ("DON'T OPTIMIZE XJ");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
                NULL;
          WHEN OTHERS =>
                FAILED ("UNEXPECTED EXCEPTION - 1");
     END;

     PROC;

     RESULT;
EXCEPTION
     WHEN OTHERS =>
          FAILED("UNEXPECTED EXCEPTION - 2");
          RESULT;
END C85005G;
