-- C36204B.ADA

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
--     CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES WITH
--     ACCESS VALUES AND FUNCTION CALLS AS THE PREFIXES.

-- HISTORY:
--     L.BROWN   08/05/86
--     DWC  07/24/87  DELETED BLANK AT END OF TEST DESCRIPTION.

WITH REPORT; USE REPORT;

PROCEDURE C36204B IS

     BEGIN
          TEST("C36204B", "ARRAY ATTRIBUTES RETURN CORRECT VALUES " &
                          "FOR ACCESS VALUES AND FUNCTION CALLS AS " &
                          "PREFIXES");
          DECLARE
               TYPE ARR1 IS ARRAY (INTEGER RANGE IDENT_INT(1) ..
                                   IDENT_INT(10)) OF INTEGER ;
               TYPE ARR2 IS ARRAY (BOOLEAN,
                                   INTEGER RANGE IDENT_INT(1) ..
                                   IDENT_INT(3)) OF INTEGER ;

               TYPE PTR1 IS ACCESS ARR1;
               TYPE PTR2 IS ACCESS ARR2;

               PT1 : PTR1 := NEW ARR1'(ARR1'RANGE => 0);
               PT2 : PTR2 := NEW ARR2'(ARR2'RANGE(1) =>
                                      (ARR2'RANGE(2) => 0));
               SUBTYPE ARR1_RANGE IS INTEGER RANGE PT1'RANGE;
          BEGIN
               IF PT1'FIRST /= IDENT_INT(1) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 1");
               END IF;

               IF PT2'FIRST(2) /= IDENT_INT(1)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 1");
               END IF;

               IF ARR1_RANGE'FIRST /= IDENT_INT(1) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 2");
               END IF;

               IF PT1'LAST /= IDENT_INT(10) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 3");
               END IF;

               IF PT2'LAST(2) /= IDENT_INT(3)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 2");
               END IF;

               IF ARR1_RANGE'LAST /= IDENT_INT(10) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 4");
               END IF;

               IF PT1'LENGTH /= IDENT_INT(10) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 5");
               END IF;

               IF PT2'LENGTH(2) /= IDENT_INT(3)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING ACCESS TYPES AS PREFIXES 3");
               END IF;

          END;

          DECLARE

               TYPE UNCON IS ARRAY (INTEGER RANGE <>) OF INTEGER ;
               TYPE UNCON2 IS ARRAY (INTEGER RANGE <>,
                                     INTEGER RANGE <>) OF INTEGER ;

               ARY1 : STRING(IDENT_INT(5) .. IDENT_INT(8));
               F : INTEGER := IDENT_INT(1);
               L : INTEGER := IDENT_INT(3);

               FUNCTION FUN( LO,HI : INTEGER ) RETURN UNCON IS
                    ARR : UNCON(IDENT_INT(LO) .. IDENT_INT(HI));
                    BEGIN
                         ARR := (ARR'RANGE => 0);
                         RETURN ARR;
                    END FUN;

               FUNCTION FUN2( LO,HI : INTEGER ) RETURN UNCON2 IS
                    AR2 : UNCON2(IDENT_INT(LO) .. IDENT_INT(HI),
                                 IDENT_INT(LO) .. IDENT_INT(HI));
                    BEGIN
                         AR2 := (AR2'RANGE(1) =>(AR2'RANGE(2) => 0));
                         RETURN AR2;
                    END FUN2;
          BEGIN

               ARY1 := (ARY1'RANGE => 'A');

               IF FUN(F,L)'FIRST /= IDENT_INT(1)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 1");
               END IF;

               IF FUN2(F,L)'FIRST(2) /= IDENT_INT(1)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 1");
               END IF;

               IF "&"(ARY1,"XX")'FIRST /= IDENT_INT(5)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 2");
               END IF;

               IF FUN(F,L)'LAST /= IDENT_INT(3)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 3");
               END IF;

               IF FUN2(F,L)'LAST(2) /= IDENT_INT(3)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 2");
               END IF;

               IF "&"(ARY1,"YY")'LAST /= IDENT_INT(10)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 4");
               END IF;

               IF FUN(F,L)'LENGTH /= IDENT_INT(3) THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 5");
               END IF;

               IF FUN2(F,L)'LENGTH(2) /= IDENT_INT(3)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 3");
               END IF;

               IF "&"(ARY1,"XX")'LENGTH /= IDENT_INT(6)  THEN
                    FAILED("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
                           "ARRAY USING FUNCTION RESULTS AS " &
                           "PREFIXES 6");
               END IF;

               DECLARE

                    SUBTYPE SMIN IS INTEGER RANGE FUN(F,L)'RANGE;
                    SUBTYPE SMIN2 IS INTEGER RANGE FUN2(F,L)'RANGE(2);
                    SUBTYPE SMIN3 IS INTEGER RANGE "&"(ARY1,"YY")'RANGE;

               BEGIN
                    IF SMIN'FIRST /= IDENT_INT(1)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "ONE-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 7");
                    END IF;

                    IF SMIN2'FIRST /= IDENT_INT(1)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "TWO-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 4");
                    END IF;

                    IF SMIN3'FIRST /= IDENT_INT(5)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "ONE-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 8");
                    END IF;

                    IF SMIN'LAST /= IDENT_INT(3)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "ONE-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 9");
                    END IF;

                    IF SMIN2'LAST /= IDENT_INT(3)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "TWO-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 5");
                    END IF;

                    IF SMIN3'LAST /= IDENT_INT(10)  THEN
                         FAILED("INCORRECT ATTRIBUTE VALUE FOR " &
                                "ONE-DIM ARRAY USING FUNCTION " &
                                "RESULTS AS PREFIXES 10");
                    END IF;

               END;

          END;

     RESULT;

     END C36204B;
