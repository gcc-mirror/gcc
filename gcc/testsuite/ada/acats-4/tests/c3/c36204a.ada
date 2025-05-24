-- C36204A.ADA

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
-- CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES.
-- BOTH ARRAY OBJECTS AND TYPES ARE CHECKED.

-- DAT 2/12/81
-- SPS 11/1/82
-- WMC 03/16/92  CREATED TYPE RANGE CHECK FOR AE_TYPE.

WITH REPORT;
PROCEDURE C36204A IS

     USE REPORT;

BEGIN
     TEST ("C36204A", "ARRAY ATTRIBUTES RETURN CORRECT VALUES");

     DECLARE
          A1 : ARRAY (BOOLEAN,
                      INTEGER RANGE IDENT_INT(1)..IDENT_INT(10))
               OF STRING(IDENT_INT(5)..IDENT_INT(7));
          TYPE NI IS  RANGE -3 .. 3;
          N : NI := NI(IDENT_INT(2));
          SUBTYPE SNI IS NI RANGE -N .. N;
          TYPE AA IS ARRAY (NI, SNI, BOOLEAN)
               OF NI;
          A1_1_1 : BOOLEAN := A1'FIRST;
          A1_1_2 : BOOLEAN := A1'LAST(1);
          A1_2_1 : INTEGER RANGE A1'RANGE(2) := A1'FIRST(2);  -- 1
          A1_2_2 : INTEGER RANGE A1'RANGE(2) := A1'LAST(2);   -- 10
          SUBTYPE AE_TYPE IS INTEGER RANGE A1(TRUE,5)'RANGE;    -- RANGE 5..7
          A2 : AA;
          A4 : ARRAY (A1_1_1 .. A1_1_2, A1_2_1 .. A1_2_2) OF
               STRING (IDENT_INT(1)..IDENT_INT(3));

          I : INTEGER;
          B : BOOLEAN;
     BEGIN
          IF A4'FIRST /= IDENT_BOOL(FALSE)
             OR A4'LAST /= IDENT_BOOL(TRUE)
             OR A4'FIRST(2) /= INTEGER'(1)
             OR A4'LAST(2) /= INTEGER'(10)
          THEN
               FAILED ("INCORRECT 'FIRST OR 'LAST  - 1");
          END IF;
      
          IF A4'LENGTH /= INTEGER'(2)
             OR A4'LENGTH /= NI'(2)
             OR A4'LENGTH(1) /= N
             OR A4'LENGTH(2) /= A4'LAST(2)
          THEN
               FAILED ("INCORRECT 'LENGTH - 1");
          END IF;

          A4 := (BOOLEAN => (1 .. 10 => "XYZ"));
          FOR L1 IN A1'RANGE(1) LOOP
               FOR L2 IN A4'RANGE(2) LOOP
                    A1(L1,L2) := A4(L1,L2);
               END LOOP;
          END LOOP;

          IF AA'FIRST(1) /= NI'(-3)
             OR AA'LAST(1) /= N + 1
             OR AA'FIRST(2) /= -N
             OR AA'LAST(2) /= N
             OR AA'FIRST(3) /= IDENT_BOOL(FALSE)
             OR AA'LAST(3) /= IDENT_BOOL(TRUE)
          THEN
               FAILED ("INCORRECT 'FIRST OR 'LAST - 2");
          END IF;

          IF N NOT IN AA'RANGE(2)
             OR IDENT_BOOL(FALSE) NOT IN AA'RANGE(3)
             OR N + 1 NOT IN AA'RANGE
             OR N + 1 IN AA'RANGE(2)
          THEN
               FAILED ("INCORRECT 'RANGE - 1");
          END IF;

          IF AA'LENGTH /= INTEGER'(7)
             OR AA'LENGTH(2) - 3 /= N
             OR AA'LENGTH(3) /= 2
          THEN
               FAILED ("INCORRECT 'LENGTH - 2");
          END IF;

          IF A2'FIRST(1) /= NI'(-3)
             OR A2'LAST(1) /= N + 1
             OR A2'FIRST(2) /= -N
             OR A2'LAST(2) /= N
             OR A2'FIRST(3) /= IDENT_BOOL(FALSE)
             OR A2'LAST(3) /= IDENT_BOOL(TRUE)
          THEN 
               FAILED ("INCORRECT 'FIRST OR 'LAST - 3");
          END IF;

          IF N NOT IN A2'RANGE(2)
             OR IDENT_BOOL(FALSE) NOT IN A2'RANGE(3)
             OR N + 1 NOT IN A2'RANGE
             OR N + 1 IN A2'RANGE(2)
          THEN
               FAILED ("INCORRECT 'RANGE - 2");
          END IF;

          IF A2'LENGTH /= INTEGER'(7)
             OR A2'LENGTH(2) - 3 /= INTEGER(N)
             OR A2'LENGTH(3) /= 2
          THEN
               FAILED ("INCORRECT 'LENGTH - 3");
          END IF;

          IF (AE_TYPE'FIRST /= 5) OR (AE_TYPE'LAST /= 7) THEN
             FAILED ("INCORRECT TYPE RANGE DEFINED FOR AE_TYPE");
          END IF;
     EXCEPTION
          WHEN OTHERS => FAILED  ("EXCEPTION RAISED ?");
     END;

     RESULT;
END C36204A;
