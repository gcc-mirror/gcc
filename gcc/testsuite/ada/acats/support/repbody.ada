-- REPBODY.ADA
--
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
--
-- HISTORY:
--      DCB 04/27/80
--      JRK 6/10/80
--      JRK 11/12/80
--      JRK 8/6/81
--      JRK 10/27/82
--      JRK 6/1/84
--      JRK 11/18/85  ADDED PRAGMA ELABORATE.
--      PWB 07/29/87  ADDED STATUS ACTION_REQUIRED AND
--                    PROCEDURE SPECIAL_ACTION.
--      TBN 08/20/87  ADDED FUNCTION LEGAL_FILE_NAME.
--      BCB 05/17/90  MODIFIED TO ALLOW OUTPUT TO DIRECT_IO FILE.
--                    ADDED TIME-STAMP.
--      LDC 05/17/90  REMOVED OUTPUT TO DIRECT_IO FILE.
--      WMC 08/11/92  UPDATED ACVC VERSION STRING TO "9X BASIC".
--      DTN 07/05/92  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0 JULY 6 1993 DRAFT".
--      WMC 01/24/94  MODIFIED LEGAL_FILE_NAME TO ALLOW FIVE POSSIBLE
--                    FILE NAMES (INCREASED RANGE OF TYPE FILE_NUM TO 1..5).
--      WMC 11/06/94  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0 NOVEMBER 6 1994 DRAFT".
--      DTN 12/04/94  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0".
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_CHAR.
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_STR.
--      DTN 11/21/95  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0.1".
--      DTN 12/14/95  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.1".
--      EDS 12/17/97  UPDATED ACVC VERSION STRING TO
--                    "2.2".
--      RLB  3/16/00  UPDATED ACATS VERSION STRING TO "2.3".
--                    CHANGED VARIOUS STRINGS TO READ "ACATS".
--      RLB  3/22/01  UPDATED ACATS VERSION STRING TO "2.4".
--      RLB  3/29/01  UPDATED ACATS VERSION STRING TO "2.5".

WITH TEXT_IO, CALENDAR;
USE TEXT_IO, CALENDAR;
PRAGMA ELABORATE (TEXT_IO, CALENDAR);

PACKAGE BODY REPORT IS

     TYPE STATUS IS (PASS, FAIL, DOES_NOT_APPLY, ACTION_REQUIRED,
                     UNKNOWN);

     TYPE TIME_INTEGER IS RANGE 0 .. 86_400;

     TEST_STATUS : STATUS := FAIL;

     MAX_NAME_LEN : CONSTANT := 15;     -- MAXIMUM TEST NAME LENGTH.
     TEST_NAME : STRING (1..MAX_NAME_LEN);

     NO_NAME : CONSTANT STRING (1..7) := "NO_NAME";
     TEST_NAME_LEN : INTEGER RANGE 0..MAX_NAME_LEN := 0;



     ACATS_VERSION : CONSTANT STRING := "2.5";
                                       -- VERSION OF ACATS BEING RUN (X.XX).

     PROCEDURE PUT_MSG (MSG : STRING) IS
          -- WRITE MESSAGE.  LONG MESSAGES ARE FOLDED (AND INDENTED).
          MAX_LEN : CONSTANT INTEGER RANGE 50..150 := 72;  -- MAXIMUM
                                        -- OUTPUT LINE LENGTH.
          INDENT : CONSTANT INTEGER := TEST_NAME_LEN + 9;  -- AMOUNT TO
                                        -- INDENT CONTINUATION LINES.
          I : INTEGER := 0;             -- CURRENT INDENTATION.
          M : INTEGER := MSG'FIRST;     -- START OF MESSAGE SLICE.
          N : INTEGER;                  -- END OF MESSAGE SLICE.
     BEGIN
          LOOP
               IF I + (MSG'LAST-M+1) > MAX_LEN THEN
                    N := M + (MAX_LEN-I) - 1;
                    IF MSG (N) /= ' ' THEN
                         WHILE N >= M AND THEN MSG (N+1) /= ' ' LOOP
                              N := N - 1;
                         END LOOP;
                         IF N < M THEN
                              N := M + (MAX_LEN-I) - 1;
                         END IF;
                    END IF;
               ELSE N := MSG'LAST;
               END IF;
               SET_COL (STANDARD_OUTPUT, TEXT_IO.COUNT (I+1));
               PUT_LINE (STANDARD_OUTPUT, MSG (M..N));
               I := INDENT;
               M := N + 1;
               WHILE M <= MSG'LAST AND THEN MSG (M) = ' ' LOOP
                    M := M + 1;
               END LOOP;
               EXIT WHEN M > MSG'LAST;
          END LOOP;
     END PUT_MSG;

     FUNCTION TIME_STAMP RETURN STRING IS
          TIME_NOW : CALENDAR.TIME;
          YEAR,
          MONTH,
          DAY,
          HOUR,
          MINUTE,
          SECOND : TIME_INTEGER := 1;

          FUNCTION CONVERT (NUMBER : TIME_INTEGER) RETURN STRING IS
               STR : STRING (1..2) := (OTHERS => '0');
               DEC_DIGIT : CONSTANT STRING := "0123456789";
               NUM : TIME_INTEGER := NUMBER;
          BEGIN
               IF NUM = 0 THEN
                    RETURN STR;
               ELSE
                    NUM := NUM MOD 100;
                    STR (2) := DEC_DIGIT (INTEGER (NUM MOD 10 + 1));
                    NUM := NUM / 10;
                    STR (1) := DEC_DIGIT (INTEGER (NUM + 1));
                    RETURN STR;
               END IF;
          END CONVERT;
     BEGIN
          TIME_NOW := CALENDAR.CLOCK;
          SPLIT (TIME_NOW, YEAR_NUMBER (YEAR), MONTH_NUMBER (MONTH),
                  DAY_NUMBER (DAY), DAY_DURATION (SECOND));
          HOUR := SECOND / 3600;
          SECOND := SECOND MOD 3600;
          MINUTE := SECOND / 60;
          SECOND := SECOND MOD 60;
          RETURN (CONVERT (TIME_INTEGER (YEAR)) & "-" &
                  CONVERT (TIME_INTEGER (MONTH)) & "-" &
                  CONVERT (TIME_INTEGER (DAY)) & " " &
                  CONVERT (TIME_INTEGER (HOUR)) & ":" &
                  CONVERT (TIME_INTEGER (MINUTE)) & ":" &
                  CONVERT (TIME_INTEGER (SECOND)));
     END TIME_STAMP;

     PROCEDURE TEST (NAME : STRING; DESCR : STRING) IS
     BEGIN
          TEST_STATUS := PASS;
          IF NAME'LENGTH <= MAX_NAME_LEN THEN
               TEST_NAME_LEN := NAME'LENGTH;
          ELSE TEST_NAME_LEN := MAX_NAME_LEN;
          END IF;
          TEST_NAME (1..TEST_NAME_LEN) :=
                    NAME (NAME'FIRST .. NAME'FIRST+TEST_NAME_LEN-1);

          PUT_MSG ("");
          PUT_MSG (",.,. " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   "ACATS " & ACATS_VERSION & " " & TIME_STAMP);
          PUT_MSG ("---- " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   DESCR & ".");
     END TEST;

     PROCEDURE COMMENT (DESCR : STRING) IS
     BEGIN
          PUT_MSG ("   - " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   DESCR & ".");
     END COMMENT;

     PROCEDURE FAILED (DESCR : STRING) IS
     BEGIN
          TEST_STATUS := FAIL;
          PUT_MSG ("   * " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   DESCR & ".");
     END FAILED;

     PROCEDURE NOT_APPLICABLE (DESCR : STRING) IS
     BEGIN
          IF TEST_STATUS = PASS OR TEST_STATUS = ACTION_REQUIRED THEN
               TEST_STATUS := DOES_NOT_APPLY;
          END IF;
          PUT_MSG ("   + " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   DESCR & ".");
     END NOT_APPLICABLE;

     PROCEDURE SPECIAL_ACTION (DESCR : STRING) IS
     BEGIN
          IF TEST_STATUS = PASS THEN
               TEST_STATUS := ACTION_REQUIRED;
          END IF;
          PUT_MSG ("   ! " & TEST_NAME (1..TEST_NAME_LEN) & " " &
                   DESCR & ".");
     END SPECIAL_ACTION;

     PROCEDURE RESULT IS
     BEGIN
          CASE TEST_STATUS IS
          WHEN PASS =>
               PUT_MSG ("==== " & TEST_NAME (1..TEST_NAME_LEN) &
                        " PASSED ============================.");
          WHEN DOES_NOT_APPLY =>
               PUT_MSG ("++++ " & TEST_NAME (1..TEST_NAME_LEN) &
                        " NOT-APPLICABLE ++++++++++++++++++++.");
          WHEN ACTION_REQUIRED =>
               PUT_MSG ("!!!! " & TEST_NAME (1..TEST_NAME_LEN) &
                        " TENTATIVELY PASSED !!!!!!!!!!!!!!!!.");
               PUT_MSG ("!!!! " & (1..TEST_NAME_LEN => ' ') &
                        " SEE '!' COMMENTS FOR SPECIAL NOTES!!");
          WHEN OTHERS =>
               PUT_MSG ("**** " & TEST_NAME (1..TEST_NAME_LEN) &
                        " FAILED ****************************.");
          END CASE;
          TEST_STATUS := FAIL;
          TEST_NAME_LEN := NO_NAME'LENGTH;
          TEST_NAME (1..TEST_NAME_LEN) := NO_NAME;
     END RESULT;

     FUNCTION IDENT_INT (X : INTEGER) RETURN INTEGER IS
     BEGIN
          IF EQUAL (X, X) THEN          -- ALWAYS EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN 0;                     -- NEVER EXECUTED.
     END IDENT_INT;

     FUNCTION IDENT_CHAR (X : CHARACTER) RETURN CHARACTER IS
     BEGIN
          IF EQUAL (CHARACTER'POS(X), CHARACTER'POS(X)) THEN  -- ALWAYS
                                        -- EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN '0';                   -- NEVER EXECUTED.
     END IDENT_CHAR;

     FUNCTION IDENT_WIDE_CHAR (X : WIDE_CHARACTER) RETURN WIDE_CHARACTER IS
     BEGIN
          IF EQUAL (WIDE_CHARACTER'POS(X), WIDE_CHARACTER'POS(X)) THEN
                                        -- ALWAYS EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN '0';                   -- NEVER EXECUTED.
     END IDENT_WIDE_CHAR;

     FUNCTION IDENT_BOOL (X : BOOLEAN) RETURN BOOLEAN IS
     BEGIN
          IF EQUAL (BOOLEAN'POS(X), BOOLEAN'POS(X)) THEN  -- ALWAYS
                                        -- EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN FALSE;                 -- NEVER EXECUTED.
     END IDENT_BOOL;

     FUNCTION IDENT_STR (X : STRING) RETURN STRING IS
     BEGIN
          IF EQUAL (X'LENGTH, X'LENGTH) THEN  -- ALWAYS EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN "";                    -- NEVER EXECUTED.
     END IDENT_STR;

     FUNCTION IDENT_WIDE_STR (X : WIDE_STRING) RETURN WIDE_STRING IS
     BEGIN
          IF EQUAL (X'LENGTH, X'LENGTH) THEN  -- ALWAYS EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
          RETURN "";                    -- NEVER EXECUTED.
     END IDENT_WIDE_STR;

     FUNCTION EQUAL (X, Y : INTEGER) RETURN BOOLEAN IS
          REC_LIMIT : CONSTANT INTEGER RANGE 1..100 := 3;  -- RECURSION
                                        -- LIMIT.
          Z : BOOLEAN;                  -- RESULT.
     BEGIN
          IF X < 0 THEN
               IF Y < 0 THEN
                    Z := EQUAL (-X, -Y);
               ELSE Z := FALSE;
               END IF;
          ELSIF X > REC_LIMIT THEN
               Z := EQUAL (REC_LIMIT, Y-X+REC_LIMIT);
          ELSIF X > 0 THEN
               Z := EQUAL (X-1, Y-1);
          ELSE Z := Y = 0;
          END IF;
          RETURN Z;
     EXCEPTION
          WHEN OTHERS =>
               RETURN X = Y;
     END EQUAL;

     FUNCTION LEGAL_FILE_NAME (X : FILE_NUM := 1;
                               NAM : STRING := "")
                              RETURN STRING IS
          SUFFIX : STRING (2..6);
     BEGIN
          IF NAM = "" THEN
               SUFFIX := TEST_NAME(3..7);
          ELSE
               SUFFIX := NAM(3..7);
          END IF;

          CASE X IS
               WHEN 1 => RETURN ('X' & SUFFIX);
               WHEN 2 => RETURN ('Y' & SUFFIX);
               WHEN 3 => RETURN ('Z' & SUFFIX);
               WHEN 4 => RETURN ('V' & SUFFIX);
               WHEN 5 => RETURN ('W' & SUFFIX);
          END CASE;
     END LEGAL_FILE_NAME;

BEGIN

     TEST_NAME_LEN := NO_NAME'LENGTH;
     TEST_NAME (1..TEST_NAME_LEN) := NO_NAME;

END REPORT;
