-- C41401A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE PREFIX OF THE FOLLOWING
-- ATTRIBUTES HAS THE VALUE NULL:
--     A) 'CALLABLE AND 'TERMINATED FOR A TASK TYPE.
--     B) 'FIRST, 'FIRST(N), 'LAST, 'LAST(N), 'LENGTH, 'LENGTH(N),
--        'RANGE, AND 'RANGE(N) FOR AN ARRAY TYPE.

-- TBN  10/2/86
-- EDS  07/14/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C41401A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TYPE ACC_TT IS ACCESS TT;

     TYPE NULL_ARR1 IS ARRAY (2 .. 1) OF INTEGER;
     TYPE ARRAY1 IS ARRAY (INT RANGE <>) OF INTEGER;
     TYPE NULL_ARR2 IS ARRAY (3 .. 1, 2 .. 1) OF INTEGER;
     TYPE ARRAY2 IS ARRAY (INT RANGE <>, INT RANGE <>) OF INTEGER;
     TYPE ACC_NULL1 IS ACCESS NULL_ARR1;
     TYPE ACC_ARR1 IS ACCESS ARRAY1;
     TYPE ACC_NULL2 IS ACCESS NULL_ARR2;
     TYPE ACC_ARR2 IS ACCESS ARRAY2;

     PTR_TT : ACC_TT;
     PTR_ARA1: ACC_NULL1;
     PTR_ARA2 : ACC_ARR1 (1 .. 4);
     PTR_ARA3 : ACC_NULL2;
     PTR_ARA4 : ACC_ARR2 (1 .. 2, 2 .. 4);
     BOOL_VAR : BOOLEAN := FALSE;
     INT_VAR : INTEGER := 1;

     TASK BODY TT IS
     BEGIN
          ACCEPT E;
     END TT;

BEGIN
     TEST ("C41401A", "CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE " &
                      "PREFIX HAS A VALUE OF NULL FOR THE FOLLOWING " &
                      "ATTRIBUTES: 'CALLABLE, 'TERMINATED, 'FIRST, " &
                      "'LAST, 'LENGTH, AND 'RANGE");

     BEGIN
          IF EQUAL (3, 2) THEN
               PTR_TT := NEW TT;
          END IF;
          BOOL_VAR := IDENT_BOOL(PTR_TT'CALLABLE);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 1 " & BOOLEAN'IMAGE(BOOL_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

     BEGIN
          IF EQUAL (1, 3) THEN
               PTR_TT := NEW TT;
          END IF;
          BOOL_VAR := IDENT_BOOL(PTR_TT'TERMINATED);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 3 " & BOOLEAN'IMAGE(BOOL_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA1'FIRST);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 5 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 6");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA2'LAST);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 7 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 8");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA1'LENGTH);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 9 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 10");
     END;

     BEGIN
          DECLARE
               A : ARRAY1 (PTR_ARA2'RANGE);
          BEGIN
               A (1) := IDENT_INT(1);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 11 " & 
                       INTEGER'IMAGE(A(1)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - 11 ");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 12");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA3'FIRST(2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 13 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 14");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA4'LAST(2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 15 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 16");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA3'LENGTH(2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 17 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 18");
     END;

     BEGIN
          DECLARE
               A : ARRAY1 (PTR_ARA4'RANGE(2));
          BEGIN
               A (1) := IDENT_INT(1);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 19 " & 
                       INTEGER'IMAGE(A(1)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("CONSTRAINT_ERROR NOT RAISED - 19 ");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 20");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA4'LAST(1));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 21 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 22");
     END;

     BEGIN
          INT_VAR := IDENT_INT(PTR_ARA3'LENGTH(1));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 23 " & INTEGER'IMAGE(INT_VAR));
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 24");
     END;

     RESULT;
END C41401A;
