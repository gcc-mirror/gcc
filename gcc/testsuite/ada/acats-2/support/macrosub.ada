-- MACROSUB.ADA
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
-----------------------------------------------------------------------
--                                                                   --
--   THIS PROGRAM IS CALLED MACROSUB.  IT IS USED TO REPLACE THE     --
--   MACROS IN THE ACVC TEST SUITE WITH THEIR PROPER VALUES.  THE    --
--   STEPS LISTED BELOW SHOULD BE FOLLOWED TO ENSURE PROPER RUNNING  --
--   OF THE MACROSUB PROGRAM:                                        --
--                                                                   --
--           1) Edit the file MACRO.DFS (included with the testtape) --
--              and insert your macro values.  The macros which use  --
--              the value of MAX_IN_LEN are calculated automatically --
--              and do not need to be entered.                       --
--                                                                   --
--           2) Create a file called TSTTESTS.DAT which includes all --
--              of the .TST test file names and their directory      --
--              specifications, if necessary.  If a different name   --
--              other than TSTTESTS.DAT is used, this name must be   --
--              substituted in the MACROSUB.ADA file.                --
--                                                                   --
--           3) Compile and link MACROSUB.                           --
--                                                                   --
--           4) Run the MACROSUB program.                            --
--                                                                   --
--   WHEN THE PROGRAM FINISHES RUNNING, THE MACROS WILL HAVE BEEN    --
--   REPLACED WITH THE APPROPRIATE VALUES FROM MACRO.DFS.            --
--                                                                   --
--                                                                   --
--                                                                   --
-- HISTORY:                                                          --
--     BCB 04/17/90  CHANGED MODE OF CALC_MAX_VALS TO OUT.  CHANGED  --
--                   VALUE OF MAX_VAL_LENGTH FROM 512 TO 400.  ADDED --
--                   EXCEPTION HANDLER SO PROGRAM DOES NOT CRASH IF  --
--                   AN EXCEPTION IS RAISED.  ADDED MESSAGES TO      --
--                   REPORT PROGRESS OF PROGRAM.  CHANGED PROGRAM SO --
--                   IT DOES NOT ABORT IF A FILE CANNOT BE FOUND.    --
--                   MODIFIED PROGRAM SO IT ACCEPTS FILENAMES WITH   --
--                   VERSION NUMBERS.                                --
-----------------------------------------------------------------------

WITH TEXT_IO;
USE TEXT_IO;

PACKAGE DEFS IS

-----------------------------------------------------------------------
--                                                                   --
--   THIS PACKAGE IS USED BY MACROSUB.ADA, PARSEMAC.ADA, AND BY      --
--   GETSUBS.ADA.  THE PACKAGE CONTAINS VARIABLE DECLARATIONS WHICH  --
--   NEED TO BE KNOWN BY ALL OF THE PROCEDURES AND PACKAGES WHICH    --
--   MAKE UP THE PROGRAM.                                            --
--                                                                   --
-----------------------------------------------------------------------

     MAX_VAL_LENGTH : CONSTANT INTEGER := 400;

     SUBTYPE VAL_STRING IS STRING (1..MAX_VAL_LENGTH);

     TYPE REC_TYPE IS RECORD
          MACRO_NAME : STRING (1..80);
          NAME_LENGTH, VALUE_LENGTH : INTEGER;
          MACRO_VALUE : VAL_STRING;
     END RECORD;

     TYPE TABLE_TYPE IS ARRAY (1..100) OF REC_TYPE;

     SYMBOL_TABLE : TABLE_TYPE;

     NUM_MACROS : INTEGER;

END DEFS;

WITH TEXT_IO;
USE TEXT_IO;
WITH DEFS;
USE DEFS;

PACKAGE GETSUBS IS

------------------------------------------------------------------------
--                                                                    --
--  THIS PACKAGE IS USED BY MACROSUB.ADA FOR READING FROM MACRO.DFS   --
--  THE VALUES FOR THE MACRO SUBSTITUTIONS FOR A TEST TAPE.           --
--                                                                    --
------------------------------------------------------------------------

     MAC_FILE, LINE_LEN : EXCEPTION;

     PROCEDURE CALC_MAX_VALS(INDEX, LENGTH, MAX_IN_LEN : IN INTEGER;
                             CALCULATED : OUT BOOLEAN);

     PROCEDURE FILL_TABLE;

END GETSUBS;

PACKAGE BODY GETSUBS IS

-----------------------------------------------------------------------
--                                                                   --
--    PROCEDURE CALC_MAX_VALS CALCULATES THE VALUE FOR THE MACRO     --
--    READ FROM MACRO.DFS IF ITS LENGTH IS EQUAL OR NEARLY EQUAL TO  --
--    MAX_IN_LEN.  IT THEN RETURNS A FLAG SET TO TRUE IF A VALUE WAS --
--    CALCULATED, FALSE IF ONE WAS NOT.                              --
--                                                                   --
-----------------------------------------------------------------------

     PROCEDURE CALC_MAX_VALS(INDEX, LENGTH, MAX_IN_LEN : IN INTEGER;
                             CALCULATED : OUT BOOLEAN) IS

     BEGIN

          IF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) = "BIG_ID1"
               THEN SYMBOL_TABLE (INDEX).MACRO_VALUE (1..MAX_IN_LEN) :=
               (1..(MAX_IN_LEN-1) => 'A') & "1";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_ID2" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN) := (1..(MAX_IN_LEN-1) => 'A') & "2";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_ID3" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN) := (1..(MAX_IN_LEN + 1)/2 => 'A') & "3" &
               ((MAX_IN_LEN + 1)/2 + 2..MAX_IN_LEN => 'A');
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_ID4" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN) := (1..(MAX_IN_LEN + 1)/2 => 'A') & "4" &
               ((MAX_IN_LEN + 1)/2 + 2..MAX_IN_LEN => 'A');
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_STRING1" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..(MAX_IN_LEN + 1)/2 + 2) :=
                          '"' & (1..(MAX_IN_LEN + 1)/2 => 'A') & '"';
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_STRING2" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN - (MAX_IN_LEN + 1)/2 + 2) :=
               '"' & (2..MAX_IN_LEN - (MAX_IN_LEN + 1)/2 => 'A') &
               '1' & '"';
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "MAX_STRING_LITERAL" THEN SYMBOL_TABLE (INDEX).
               MACRO_VALUE (1..MAX_IN_LEN) := '"' &
               (1..MAX_IN_LEN-2 => 'A') & '"';
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_INT_LIT" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN)  := (1..MAX_IN_LEN-3 => '0') & "298";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BIG_REAL_LIT" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN) := (1..MAX_IN_LEN-5 => '0') & "690.0";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "MAX_LEN_INT_BASED_LITERAL" THEN
               SYMBOL_TABLE (INDEX).
               MACRO_VALUE (1..MAX_IN_LEN) := "2:" &
               (1..MAX_IN_LEN - 5 => '0') & "11:";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "MAX_LEN_REAL_BASED_LITERAL" THEN SYMBOL_TABLE (INDEX).
               MACRO_VALUE (1..MAX_IN_LEN) := "16:" &
               (1..MAX_IN_LEN - 7 => '0') & "F.E:";
               CALCULATED := TRUE;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BLANKS" THEN SYMBOL_TABLE (INDEX).MACRO_VALUE
               (1..MAX_IN_LEN-20) := (1..MAX_IN_LEN-20 => ' ');
               CALCULATED := TRUE;
          ELSE
               CALCULATED := FALSE;
          END IF;
          IF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
               "BLANKS" THEN SYMBOL_TABLE (INDEX).VALUE_LENGTH :=
               MAX_IN_LEN - 20;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
                "BIG_STRING1" THEN
               SYMBOL_TABLE (INDEX).VALUE_LENGTH :=
                           (MAX_IN_LEN + 1)/2 + 2;
          ELSIF SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) =
                "BIG_STRING2" THEN
               SYMBOL_TABLE (INDEX).VALUE_LENGTH :=
                      MAX_IN_LEN - (MAX_IN_LEN + 1)/2 + 2;
          ELSE SYMBOL_TABLE (INDEX).VALUE_LENGTH := MAX_IN_LEN;
          END IF;
     END CALC_MAX_VALS;

-----------------------------------------------------------------------
--                                                                   --
--    PROCEDURE FILL_TABLE READS THE MACRO NAMES AND MACRO VALUES IN --
--    FROM MACRO.DFS AND STORES THEM IN THE SYMBOL TABLE.  PROCEDURE --
--    CALC_MAX_VALS IS CALLED TO DETERMINE IF THE MACRO VALUE SHOULD --
--    BE CALCULATED OR READ FROM MACRO.DFS.                          --
--                                                                   --
-----------------------------------------------------------------------

     PROCEDURE FILL_TABLE IS

          INFILE1 : FILE_TYPE;
          MACRO_FILE : CONSTANT STRING := "MACRO.DFS";
          A_LINE : VAL_STRING;
          I, INDEX, LENGTH, HOLD, A_LENGTH, NAME : INTEGER;
          MAX_IN_LEN : INTEGER := 1;
          CALCULATED : BOOLEAN;

     BEGIN
          INDEX := 1;
          BEGIN
               OPEN (INFILE1, IN_FILE, MACRO_FILE);
          EXCEPTION
               WHEN NAME_ERROR =>
                    PUT_LINE ("** ERROR: MACRO FILE " & MACRO_FILE &
                              " NOT FOUND.");
                    RAISE MAC_FILE;
          END;
          WHILE NOT END_OF_FILE (INFILE1) LOOP
               GET_LINE (INFILE1, A_LINE, A_LENGTH);
               IF A_LENGTH > 0 AND A_LINE (1..2) /= "--" AND
                  A_LINE (1) /= ' ' AND A_LINE (1) /= ASCII.HT THEN
                    I := 1;
                    WHILE I <= A_LENGTH AND THEN
                           ((A_LINE (I) IN 'A'..'Z') OR
                            (A_LINE (I) IN '0'..'9') OR
                            A_LINE (I) = '_') LOOP
                         I := I + 1;
                    END LOOP;
                    I := I - 1;
                    LENGTH := I;
                    BEGIN
                         SYMBOL_TABLE (INDEX).MACRO_NAME (1..LENGTH) :=
                              A_LINE (1..I);
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              PUT_LINE ("** ERROR: LINE LENGTH IS " &
                                        "GREATER THAN MAX_VAL_LENGTH.");
                              RAISE LINE_LEN;
                    END;
                    SYMBOL_TABLE (INDEX).NAME_LENGTH := I;
                    CALC_MAX_VALS (INDEX, LENGTH, MAX_IN_LEN,
                                   CALCULATED);
                    IF NOT CALCULATED THEN
                         I := I + 1;
                         WHILE A_LINE (I) = ' ' OR A_LINE (I) =
                           ASCII.HT LOOP
                              I := I + 1;
                              IF SYMBOL_TABLE (INDEX).MACRO_NAME
                                   (1..LENGTH) = "BLANKS" THEN
                                   EXIT;
                              END IF;
                         END LOOP;
                         HOLD := I;

-- MACRO VALUE BEGINS AT POSITION HOLD.
-- NOW FIND WHERE IT ENDS BY STARTING AT THE END OF THE INPUT
-- LINE AND SEARCHING BACKWARD FOR A NON-BLANK.

                         I := A_LENGTH;
                         WHILE I > HOLD AND THEN (A_LINE (I) = ' '
                            OR A_LINE(I) = ASCII.HT) LOOP
                              I := I - 1;
                         END LOOP;
                         LENGTH := I - HOLD + 1;
                         SYMBOL_TABLE (INDEX).MACRO_VALUE (1..LENGTH)
                              := A_LINE (HOLD..I);
                         SYMBOL_TABLE (INDEX).VALUE_LENGTH := LENGTH;
                         NAME := SYMBOL_TABLE (INDEX).NAME_LENGTH;
                         IF SYMBOL_TABLE (INDEX).MACRO_NAME (1..NAME) =
                              "MAX_IN_LEN" THEN MAX_IN_LEN :=
                              INTEGER'VALUE (SYMBOL_TABLE (INDEX).
                              MACRO_VALUE (1..LENGTH));
                         END IF;
                    END IF;
                    INDEX := INDEX + 1;
               END IF;
          END LOOP;
          NUM_MACROS := INDEX - 1;
          CLOSE (INFILE1);
     END FILL_TABLE;

BEGIN
     NULL;
END GETSUBS;

WITH TEXT_IO;
USE TEXT_IO;
WITH DEFS;
USE DEFS;

PACKAGE PARSEMAC IS

------------------------------------------------------------------------
--                                                                    --
--   THIS PACKAGE IS USED BY MACROSUB.ADA FOR FINDING A MACRO TO      --
--   SUBSTITUTE.  MACRO SUBSTITUTIONS ARE MADE IN *.TST TESTS IN THE  --
--   ACVC TEST SUITE.  THIS PROCEDURE IS CURRENTLY SET UP FOR ACVC    --
--   VERSION 1.10.                                                    --
--                                                                    --
------------------------------------------------------------------------

     PROCEDURE LOOK_FOR_MACRO (A_LINE : IN STRING;
                               A_LENGTH : IN INTEGER;
                               PTR : IN OUT INTEGER;
                               MACRO : OUT STRING;
                               MACRO_LEN : IN OUT INTEGER);


     PROCEDURE WHICH_MACRO (MACRO : IN STRING;
                            MACRO_LEN : IN INTEGER;
                            TEMP_MACRO : OUT STRING;
                            TEMP_MACRO_LEN : IN OUT INTEGER);

END PARSEMAC;

PACKAGE BODY PARSEMAC IS

-----------------------------------------------------------------------
--    PROCEDURE LOOK_FOR_MACRO LOOKS FOR A DOLLAR SIGN WHICH SIGNALS --
--    THE START OF A MACRO IN THE *.TST FILES.  IT THEN COUNTS       --
--    CHARACTERS UNTIL A <LETTER>, <NUMBER>, OR <_> IS NOT FOUND.    --
--    RETURN PARAMETERS SEND THE BEGINNING POINTER AND LENGTH OF THE --
--    MACRO BACK TO THE MAIN PROGRAM.  ALSO RETURNED IS THE MACRO    --
--    STRING.                                                        --
-----------------------------------------------------------------------

     PROCEDURE LOOK_FOR_MACRO (A_LINE : IN STRING;
                               A_LENGTH : IN INTEGER;
                               PTR : IN OUT INTEGER;
                               MACRO : OUT STRING;
                               MACRO_LEN : IN OUT INTEGER) IS

     II, J : INTEGER := INTEGER'LAST;

     BEGIN
          FOR I IN PTR..A_LENGTH LOOP
               IF A_LINE (I) = '$' THEN
                    II := I+1;
                    EXIT;
               END IF;
               II := I;
          END LOOP;
          IF II < A_LENGTH THEN    -- DOLLAR SIGN IS FOUND.
               J := II;
               WHILE J <= A_LENGTH AND THEN ((A_LINE(J) IN 'A'..'Z') OR
                      (A_LINE(J) IN '0'..'9') OR
                       A_LINE(J) = '_') LOOP
                       J := J+1;
               END LOOP;
               J := J-1;
               MACRO_LEN := (J-II+1);
               MACRO (1..MACRO_LEN) := A_LINE (II .. J);
                             -- DON'T INCLUDE THE DOLLAR SIGN
               PTR := J+1;
          ELSE
               MACRO_LEN := 0;
          END IF;
     RETURN;
     END LOOK_FOR_MACRO;

------------------------------------------------------------------------
--    PROCEDURE WHICH_MACRO COMPARES THE INPUT MACRO STRING TO A      --
--    VALUE READ FROM MACRO.DFS AND STORED IN THE SYMBOL TABLE AND    --
--    RETURNS THE MACRO SUBSTITUTION STRING BACK TO THE MAIN PROGRAM. --
------------------------------------------------------------------------

     PROCEDURE WHICH_MACRO (MACRO : IN STRING;
                            MACRO_LEN : IN INTEGER;
                            TEMP_MACRO : OUT STRING;
                            TEMP_MACRO_LEN : IN OUT INTEGER) IS

     BEGIN
          FOR INDEX IN 1 .. NUM_MACROS LOOP
               IF MACRO (1..MACRO_LEN) =
                  SYMBOL_TABLE (INDEX).MACRO_NAME
                      (1..SYMBOL_TABLE (INDEX).NAME_LENGTH) THEN
                    TEMP_MACRO_LEN :=
                      SYMBOL_TABLE (INDEX).VALUE_LENGTH;
                    TEMP_MACRO (1..TEMP_MACRO_LEN) :=
                      SYMBOL_TABLE (INDEX).MACRO_VALUE
                        (1..TEMP_MACRO_LEN);
                    EXIT;
               END IF;
               IF INDEX = NUM_MACROS THEN
                    PUT_LINE ("** ERROR: MACRO " & MACRO (1..MACRO_LEN)
                              & " NOT FOUND.  UPDATE PROGRAM.");
                    TEMP_MACRO_LEN := MACRO_LEN;
                    TEMP_MACRO (1..TEMP_MACRO_LEN) :=
                       MACRO (1..MACRO_LEN);
               END IF;
          END LOOP;

     END WHICH_MACRO;

BEGIN
     NULL;
END PARSEMAC;

WITH TEXT_IO, GETSUBS, PARSEMAC, DEFS;
USE TEXT_IO, GETSUBS, PARSEMAC, DEFS;

PROCEDURE MACROSUB IS

------------------------------------------------------------------------
--                                                                    --
--    MACROSUB IS THE MAIN PROGRAM THAT CALLS PROCEDURES IN TWO       --
--    PACKAGES, GETSUBS AND PARSEMAC.  THIS PROGRAM IS USED TO MAKE   --
--    THE MACRO SUBSTITUTIONS FOR TST TESTS IN THE ACVC TEST SUITE.   --
--                                                                    --
------------------------------------------------------------------------

     INFILE1, INFILE2, OUTFILE1     : FILE_TYPE;
     FNAME, MACRO                   : VAL_STRING;
     LENGTH, A_LENGTH, PTR,
     TEMP_MACRO_LENGTH, MACRO_LEN, FILE_COUNT : INTEGER := 0;
     A_LINE, TEMP_MACRO, TEMP_LINE, NEW_LINE : VAL_STRING;
     END_OF_LINE_SEARCH, FLAG       : BOOLEAN := FALSE;
     TESTS_FILE                     : CONSTANT STRING := "TSTTESTS.DAT";
     TSTTESTS,FILE_CRE              : EXCEPTION;

BEGIN
     PUT_LINE ("BEGINNING MACRO SUBSTITUTIONS.");
     FILL_TABLE;
     BEGIN
          OPEN (INFILE2, IN_FILE, TESTS_FILE);
     EXCEPTION
          WHEN NAME_ERROR =>
               PUT_LINE ("** ERROR: ERROR DURING OPENING OF " &
                         "TSTTESTS.DAT");
               RAISE TSTTESTS;
     END;
     WHILE NOT END_OF_FILE (INFILE2) LOOP
          GET_LINE (INFILE2, FNAME, LENGTH);
          FILE_COUNT := FILE_COUNT + 1;
          BEGIN
               OPEN (INFILE1, IN_FILE, FNAME(1..LENGTH));
          EXCEPTION
               WHEN NAME_ERROR =>
                    PUT_LINE ("** ERROR: ERROR DURING OPENING OF " &
                              FNAME(1..LENGTH) & ".");
                    FLAG := TRUE;
          END;
          IF NOT FLAG THEN
               PUT_LINE ("WORKING ON " & FNAME(1..LENGTH));
               IF FILE_COUNT = 70 THEN
                    PUT_LINE ("MACRO SUBSTITUTIONS HALF COMPLETED.");
               END IF;
               FOR I IN REVERSE 1 .. LENGTH LOOP
                    IF FNAME(I) = ';' THEN
                         LENGTH := I - 1;
                         EXIT;
                    END IF;
               END LOOP;
               IF FNAME (LENGTH-2..LENGTH) = "TST" THEN
                    FNAME (LENGTH-2..LENGTH) := "ADT";
               ELSIF FNAME (LENGTH-2..LENGTH) = "tst" THEN
                    FNAME (LENGTH-2..LENGTH) := "adt";
               END IF;
               BEGIN
                    CREATE (OUTFILE1, OUT_FILE, FNAME (1..LENGTH));
               EXCEPTION
                    WHEN OTHERS =>
                         PUT_LINE ("** ERROR: EXCEPTION RAISED DURING" &
                                   " ATTEMPTED CREATION OF " &
                                   FNAME(1..LENGTH) & ".");
                         RAISE FILE_CRE;
               END;
               WHILE NOT END_OF_FILE (INFILE1) LOOP
                    GET_LINE (INFILE1, A_LINE, A_LENGTH);
                    IF A_LENGTH > 0 AND A_LINE(1..2) /= "--" THEN
                         END_OF_LINE_SEARCH := FALSE;
                         PTR := 1;
                         WHILE NOT END_OF_LINE_SEARCH LOOP
                              LOOK_FOR_MACRO (A_LINE, A_LENGTH, PTR,
                                              MACRO, MACRO_LEN);
                              IF MACRO_LEN = 0 THEN
                                   END_OF_LINE_SEARCH := TRUE;
                              ELSE --  SEE WHICH MACRO IT IS
                                   WHICH_MACRO (MACRO, MACRO_LEN,
                                       TEMP_MACRO, TEMP_MACRO_LENGTH);
                              END IF;
                              IF NOT END_OF_LINE_SEARCH THEN
                                   IF PTR-MACRO_LEN-2 > 0 THEN
                                 -- IF MACRO IS NOT FIRST ON THE LINE
                                        NEW_LINE (1..PTR-MACRO_LEN-2)
                                         := A_LINE(1..PTR-MACRO_LEN -2);
                                 -- THE OLD LINE UNTIL THE DOLLAR SIGN
                                   END IF;
                                   NEW_LINE(PTR-MACRO_LEN-1 ..
                                            TEMP_MACRO_LENGTH +
                                            (PTR-MACRO_LEN) - 2) :=
                                       TEMP_MACRO(1..TEMP_MACRO_LENGTH);
                                   IF PTR <= A_LENGTH THEN
                                 -- IF MACRO IS NOT LAST ON THE LINE
                                        NEW_LINE (TEMP_MACRO_LENGTH +
                                             PTR-MACRO_LEN - 1 ..
                                             TEMP_MACRO_LENGTH - 1 +
                                             A_LENGTH - MACRO_LEN) :=
                                             A_LINE (PTR..A_LENGTH);
                                   ELSE
                                        END_OF_LINE_SEARCH := TRUE;
                                   END IF;
                                   A_LENGTH := A_LENGTH +
                                               TEMP_MACRO_LENGTH -
                                               MACRO_LEN - 1;
                                   A_LINE (1..A_LENGTH) :=
                                          NEW_LINE (1..A_LENGTH);
                                   PTR := PTR - MACRO_LEN +
                                          TEMP_MACRO_LENGTH - 1;
                              END IF;
                         END LOOP;
                    END IF;
                    PUT_LINE (OUTFILE1, A_LINE (1..A_LENGTH));
               END LOOP;
               CLOSE (OUTFILE1);
               CLOSE (INFILE1);
          ELSE
               FLAG := FALSE;
          END IF;
     END LOOP;
     CLOSE (INFILE2);
     PUT_LINE ("MACRO SUBSTITUTIONS COMPLETED.");
EXCEPTION
     WHEN MAC_FILE | LINE_LEN | TSTTESTS | FILE_CRE =>
          NULL;
     WHEN OTHERS =>
          PUT_LINE ("UNEXPECTED EXCEPTION RAISED");
END MACROSUB;
