-- C52008B.ADA

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
--     CHECK THAT A RECORD VARIABLE DECLARED WITH A SPECIFIED
--     DISCRIMINANT CONSTRAINT CANNOT HAVE A DISCRIMINANT VALUE ALTERED
--     BY ASSIGNMENT.  ASSIGNING AN ENTIRE RECORD VALUE WITH A
--     DIFFERENT DISCRIMINANT VALUE SHOULD RAISE CONSTRAINT_ERROR AND
--     LEAVE THE TARGET VARIABLE UNALTERED.  THIS TEST USES NON-STATIC
--     DISCRIMINANT VALUES.

-- HISTORY:
--     ASL  6/25/81  CREATED ORIGINAL TEST
--     JRK 11/18/82
--     RJW  8/17/89  ADDED SUBTYPE 'SUBINT'.

WITH REPORT;
PROCEDURE C52008B IS

     USE REPORT;

     TYPE REC1(D1,D2 : INTEGER) IS
          RECORD
               COMP1 : STRING(D1..D2);
          END RECORD;

     TYPE AR_REC1 IS ARRAY (NATURAL RANGE <>) OF REC1(IDENT_INT(3),
                                                      IDENT_INT(5));

     SUBTYPE SUBINT IS INTEGER RANGE -128 .. 127;

     TYPE REC2(D1,D2,D3,D4 : SUBINT := 0) IS
          RECORD
               COMP1 : STRING(1..D1);
               COMP2 : STRING(D2..D3);
               COMP5 : AR_REC1(1..D4);
               COMP6 : REC1(D3,D4);
          END RECORD;

     STR : STRING(IDENT_INT(3)..IDENT_INT(5)) := "ZZZ";

     R1A : REC1(IDENT_INT(3),IDENT_INT(5)) := (3,5,STR);
     R1C : REC1(5,6) := (5,6,COMP1 => (5..6 => 'K'));

     Q,R : REC2(IDENT_INT(2),IDENT_INT(3),IDENT_INT(5),IDENT_INT(6));
     TEMP : REC2(2,3,5,6);

     W : REC2(1,4,6,8);
     OK : BOOLEAN := FALSE;


BEGIN

     TEST ("C52008B", "CANNOT ASSIGN RECORD VARIABLE WITH SPECIFIED " &
                      "DISCRIMINANT VALUE A VALUE WITH A DIFFERENT " &
                      "(DYNAMIC) DISCRIMINANT VALUE");

     BEGIN
          R1A := (IDENT_INT(3),5,"XYZ");

          R := (IDENT_INT(2),IDENT_INT(3),IDENT_INT(5),IDENT_INT(6),
                "AB",
                STR,
                (1..6 => R1A),
                R1C);

          TEMP := R;
          Q := TEMP;
          R.COMP1 := "YY";
          OK := TRUE;
          W := R;
          FAILED ("ASSIGNMENT MADE USING INCORRECT DISCRIMINANT " &
                  "VALUES");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT OK
               OR Q /= TEMP
               OR R = TEMP
               OR R = Q
               OR W.D4 /= 8 THEN
                    FAILED ("LEGITIMATE ASSIGNMENT FAILED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION");
     END;

     RESULT;

END C52008B;
