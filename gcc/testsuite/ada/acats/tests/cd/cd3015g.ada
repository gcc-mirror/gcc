-- CD3015G.ADA

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
--     CHECK THAT A DERIVED ENUMERATION TYPE WITH A REPRESENTATION
--     CLAUSE CAN BE USED CORRECTLY IN ORDERING RELATIONS, INDEXING
--     ARRAYS, AND IN GENERIC INSTANTIATIONS WHEN THERE IS AN
--     ENUMERATION CLAUSE FOR THE PARENT.

-- HISTORY
--     DHH 09/30/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     BCB 03/08/90  REVISED WORDING IN HEADER COMMENT AND IN CALL TO
--                   REPORT.TEST.  ADDED CHECK FOR NON-CONTIGUOUS CODES.
--                   REVISED CHECK FOR ARRAY INDEXING.
--     THS 09/18/90  REVISED WORDING IN HEADER COMMENT AND FIXED FAILURE
--                   ERROR MESSAGE.

WITH REPORT; USE REPORT;
PROCEDURE CD3015G IS

BEGIN

     TEST ("CD3015G", "CHECK THAT A DERIVED ENUMERATION TYPE WITH A " &
                      "REPRESENTATION CLAUSE CAN BE USED CORRECTLY " &
                      "IN ORDERING RELATIONS, INDEXING ARRAYS, AND " &
                      "IN GENERIC INSTANTIATIONS WHEN THERE IS AN " &
                      "ENUMERATION CLAUSE FOR THE PARENT");

     DECLARE
          PACKAGE PACK IS

               TYPE MAIN IS (RED,BLUE,YELLOW,'R','B','Y');

               FOR MAIN USE (RED => 1, BLUE => 2, YELLOW => 3, 'R' => 4,
                             'B' => 5, 'Y' => 6);

               TYPE HUE IS NEW MAIN;
               FOR HUE USE (RED => 8, BLUE => 9, YELLOW => 10,
                            'R' => 11, 'B' => 12, 'Y' => 13);

               TYPE HUE1 IS NEW MAIN;
               FOR HUE1 USE (RED => 10, BLUE => 14, YELLOW => 16,
                            'R' => 19, 'B' => 41, 'Y' => 46);

               TYPE BASE1 IS ARRAY(HUE1) OF INTEGER;
               COLOR1,BASIC1 : HUE1;
               BARRAY1 : BASE1;

               TYPE BASE IS ARRAY(HUE) OF INTEGER;
               COLOR,BASIC : HUE;
               BARRAY : BASE;

               GENERIC
                    TYPE ENUM IS (<>);
               PROCEDURE CHANGE(X,Y : IN OUT ENUM);

          END PACK;

          PACKAGE BODY PACK IS

               PROCEDURE CHANGE(X,Y : IN OUT ENUM) IS
                    T : ENUM;
               BEGIN
                    T := X;
                    X := Y;
                    Y := T;
               END CHANGE;

               PROCEDURE PROC IS NEW CHANGE(HUE);
               PROCEDURE PROC1 IS NEW CHANGE(HUE1);

          BEGIN
               BASIC := RED;
               COLOR := HUE'SUCC(BASIC);
               BASIC1 := RED;
               COLOR1 := HUE1'SUCC(BASIC1);
               IF (COLOR < BASIC OR BASIC >= 'R' OR 'Y' <= COLOR OR
                  COLOR > 'B') OR
                  NOT (COLOR1 >= BASIC1 AND BASIC1 < 'R' AND
                  'Y' > COLOR1 AND COLOR1 <= 'B') THEN
                    FAILED("ORDERING RELATIONS ARE INCORRECT");
               END IF;

               PROC(BASIC,COLOR);
               PROC1(BASIC1,COLOR1);

               IF COLOR /= RED OR COLOR1 /= RED THEN
                    FAILED("VALUES OF PARAMETERS TO INSTANCE OF " &
                           "GENERIC UNIT NOT CORRECT AFTER CALL");
               END IF;

               BARRAY := (IDENT_INT(1),IDENT_INT(2),IDENT_INT(3),
                         IDENT_INT(4),IDENT_INT(5),IDENT_INT(6));

               BARRAY1 := (IDENT_INT(1),IDENT_INT(2),IDENT_INT(3),
                         IDENT_INT(4),IDENT_INT(5),IDENT_INT(6));

               IF (BARRAY (RED) /= 1 OR BARRAY (BLUE) /= 2 OR
                   BARRAY (YELLOW) /= 3 OR BARRAY ('R') /= 4 OR
                   BARRAY ('B') /= 5 OR BARRAY ('Y') /= 6) OR
                  NOT (BARRAY1 (RED) = 1 AND BARRAY1 (BLUE) = 2 AND
                   BARRAY1 (YELLOW) = 3 AND BARRAY1 ('R') = 4 AND
                   BARRAY1 ('B') = 5 AND BARRAY1 ('Y') = 6)
                  THEN
                    FAILED("INDEXING ARRAY FAILURE");
               END IF;

          END PACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD3015G;
