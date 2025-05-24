-- C41104A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF AN EXPRESSION GIVES AN INDEX
-- VALUE OUTSIDE THE RANGE SPECIFIED FOR THE INDEX FOR ARRAYS AND ACCESS
-- TYPES.

-- TBN  9/12/86
-- EDS  8/03/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C41104A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 5;
     SUBTYPE BOOL IS BOOLEAN RANGE TRUE .. TRUE;
     SUBTYPE CHAR IS CHARACTER RANGE 'W' .. 'Z';
     TYPE ARRAY1 IS ARRAY (INT RANGE <>) OF INTEGER;
     TYPE ARRAY2 IS ARRAY (3 .. 1) OF INTEGER;
     TYPE ARRAY3 IS ARRAY (BOOL RANGE <>) OF INTEGER;
     TYPE ARRAY4 IS ARRAY (CHAR RANGE <>) OF INTEGER;

     TYPE REC (D : INT) IS
          RECORD
               A : ARRAY1 (1 .. D);
          END RECORD;

     TYPE B_REC (D : BOOL) IS
          RECORD
               A : ARRAY3 (TRUE .. D);
          END RECORD;

     TYPE NULL_REC (D : INT) IS
          RECORD
               A : ARRAY1 (D .. 1);
          END RECORD;

     TYPE NULL_CREC (D : CHAR) IS
          RECORD
               A : ARRAY4 (D .. 'W');
          END RECORD;

BEGIN
     TEST ("C41104A", "CHECK THAT CONSTRAINT_ERROR IS RAISED IF AN " &
                      "EXPRESSION GIVES AN INDEX VALUE OUTSIDE THE " &
                      "RANGE SPECIFIED FOR THE INDEX FOR ARRAYS AND " &
                      "ACCESS TYPES");

     DECLARE
          ARA1 : ARRAY1 (1 .. 5) := (1, 2, 3, 4, 5);
     BEGIN
          ARA1 (IDENT_INT(0)) := 1;

          BEGIN
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - " &
                        INTEGER'IMAGE(ARA1 (1)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 1");
     END;
------------------------------------------------------------------------
     DECLARE
          TYPE ACC_ARRAY IS ACCESS ARRAY3 (TRUE .. TRUE);
          ACC_ARA : ACC_ARRAY := NEW ARRAY3'(TRUE => 2);
     BEGIN
          ACC_ARA (IDENT_BOOL(FALSE)) := 2;

          BEGIN

               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - " &
                        INTEGER'IMAGE(ACC_ARA (TRUE)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 2");
     END;
------------------------------------------------------------------------
     DECLARE
          ARA2 : ARRAY4 ('Z' .. 'Y');
     BEGIN
          ARA2 (IDENT_CHAR('Y')) := 3;

          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 3");

          BEGIN
               COMMENT ("ARA2 (Y) IS " & INTEGER'IMAGE(ARA2 ('Y')));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 3");
     END;
------------------------------------------------------------------------
     DECLARE
          TYPE ACC_ARRAY IS ACCESS ARRAY2;
          ACC_ARA : ACC_ARRAY := NEW ARRAY2;
     BEGIN
          ACC_ARA (IDENT_INT(4)) := 4;

          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 4");

          BEGIN
               COMMENT ("ACC_ARA (4) IS " & INTEGER'IMAGE(ACC_ARA (4)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 4");
     END;
------------------------------------------------------------------------
     DECLARE
          REC1 : B_REC (TRUE) := (TRUE, A => (TRUE => 5));
     BEGIN
          REC1.A (IDENT_BOOL (FALSE)) := 1;

          BEGIN
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - " &
                       INTEGER'IMAGE(REC1.A (TRUE)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 5");
     END;
------------------------------------------------------------------------
     DECLARE
          TYPE ACC_REC IS ACCESS REC (3);
          ACC_REC1 : ACC_REC := NEW REC'(3, (4, 5, 6));
     BEGIN
          ACC_REC1.A (IDENT_INT(4)) := 4;

          BEGIN
               FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - " &
                        INTEGER'IMAGE(ACC_REC1.A (3)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 6");
     END;
------------------------------------------------------------------------
     DECLARE
          REC1 : NULL_REC (2);
     BEGIN
          REC1.A (IDENT_INT(2)) := 1;

          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 7");

          BEGIN
               COMMENT ("REC1.A (2) IS " & INTEGER'IMAGE(REC1.A (2)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 7");
     END;
------------------------------------------------------------------------
     DECLARE
          TYPE ACC_REC IS ACCESS NULL_CREC ('Z');
          ACC_REC1 : ACC_REC := NEW NULL_CREC ('Z');
     BEGIN
          ACC_REC1.A (IDENT_CHAR('A')) := 4;

          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 8");
          BEGIN
               COMMENT ("ACC_REC1.A (A) IS " &
                         INTEGER'IMAGE(ACC_REC1.A ('A')));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION ON ATTEMPT TO USE OBJECT");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - 8");
     END;
------------------------------------------------------------------------

     RESULT;
END C41104A;
