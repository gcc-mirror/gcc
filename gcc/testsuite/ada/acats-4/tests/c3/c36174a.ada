-- C36174A.ADA

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
-- CHECK THAT INDEX_CONSTRAINTS MAY BE OMITTED FOR CONSTANTS.

-- DAT 2/9/81
-- JBG 12/8/83


WITH REPORT;
PROCEDURE C36174A IS

     USE REPORT;

     S0 : CONSTANT STRING := "";
     S1 : CONSTANT STRING := S0;
     S2 : CONSTANT STRING := (1 .. 0 => 'Z');
     S3 : CONSTANT STRING := ('A', 'B', 'C');
     S4 : CONSTANT STRING := S3 & "ABC" & S3 & S2 & "Z";
     S9 : CONSTANT STRING := S0 & S1 & S2 & S3(3..1);

     TYPE A4 IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>,
          INTEGER RANGE <>, INTEGER RANGE <>) OF STRING (1 .. 0);
     C4 : CONSTANT A4 :=
          (-6 .. -4 =>
               (4 .. 5 =>
                    (-4 .. -5 =>
                         (1000 .. 2000 =>
                              S9))));
     S10 : CONSTANT STRING := (10 .. 9 => 'Q');

     TYPE I_12 IS NEW INTEGER RANGE 10 .. 12;
     TYPE A_12 IS ARRAY (I_12 RANGE <>, I_12 RANGE <>) OF I_12;
     A12 : CONSTANT A_12 :=
          (11 .. 12 => (10 .. 10 => 10));
     B12 : CONSTANT A_12 :=
          (11 => (10 | 12 => 10, 11 => 11),
           10 => (10 | 12 | 11 => 12));

     N6 : CONSTANT INTEGER := IDENT_INT (6);
     S6 : CONSTANT STRING := (N6 .. N6 + 6 => 'Z');
     S7 : CONSTANT STRING := S6 (N6 .. N6 + IDENT_INT (-1));

BEGIN
     TEST ("C36174A", "INDEX_CONSTRAINTS MAY BE OMITTED FOR CONSTANTS");

     IF S0'FIRST /= 1 OR S0'LAST /= 0
     OR S1'FIRST /= 1 OR S1'LAST /= 0
     OR S2'FIRST /= 1 OR S2'LAST /= 0
     OR S3'FIRST /= 1 OR S3'LAST /= 3
     THEN
          FAILED ("INVALID STRING CONSTANT BOUNDS 1");
     END IF;

     IF S4'FIRST /= 1 OR S4'LAST /= 10 THEN
          FAILED ("INVALID STRING CONSTANT BOUNDS 2");
     END IF;

     IF S9'FIRST /= 3 OR S9'LAST /= 1 THEN
          FAILED ("INVALID STRING CONSTANT BOUNDS 3");
     END IF;

     IF C4'FIRST(1) /= -6 OR C4'LAST(1) /= -4
     OR C4'FIRST(2) /= 4 OR C4'LAST(2) /= 5
     OR C4'FIRST(3) /= -4 OR C4'LAST(3) /= -5
     OR C4'FIRST(4) /= 1000 OR C4'LAST(4) /= 2000
     THEN
          FAILED ("INVALID ARRAY CONSTANT BOUNDS");
     END IF;

     IF S10'FIRST /= 10 OR S10'LAST /= 9
     THEN
          FAILED ("INVALID STRING CONSTANT BOUNDS 10");
     END IF;

     IF A12'FIRST /= 11 OR A12'LAST /= 12
     OR A12'FIRST(2) /= 10 OR A12'LAST(2) /= 10
     THEN FAILED ("INVALID ARRAY CONSTANT BOUNDS 2");
     END IF;

     IF B12'FIRST /= 10 OR B12'LAST /= 11
     OR B12'FIRST(2) /= 10 OR B12'LAST(2) /= 12
     THEN
          FAILED ("INVALID ARRAY CONSTANT BOUNDS 3");
     END IF;

     IF S6'FIRST /= 6 OR S6'LAST /= 12 OR S6'LENGTH /= 7
     THEN
          FAILED ("INVALID STRING CONSTANT  BOUNDS 12");
     END IF;

     IF S7'FIRST /= 6 OR S7'LAST /= 5 THEN
          FAILED ("INVALID STRING CONSTANT BOUNDS 13");
     END IF;

     RESULT;
END C36174A;
