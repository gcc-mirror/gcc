-- C43204C.ADA

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
--     CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS CHOICE CAN APPEAR
--     (AND BOUNDS ARE DETERMINED CORRECTLY) AS AN ACTUAL PARAMETER OF
--     A GENERIC INSTANTIATION WHEN THE GENERIC FORMAL PARAMETER IS
--     CONSTRAINED.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43204C IS

     TYPE ARR10 IS ARRAY(IDENT_INT(1)..IDENT_INT(0)) OF INTEGER;
     TYPE ARR11 IS ARRAY(INTEGER RANGE -3..3) OF INTEGER;
     TYPE ARR12 IS ARRAY(IDENT_INT(-3)..IDENT_INT(3)) OF INTEGER;

     TYPE ARR20 IS ARRAY(IDENT_INT(1)..IDENT_INT(0),
                         IDENT_INT(0)..IDENT_INT(-1)) OF INTEGER;
     TYPE ARR21 IS ARRAY(INTEGER RANGE -1..1,
                         INTEGER RANGE -1..1) OF INTEGER;
     TYPE ARR22 IS ARRAY(IDENT_INT(-1)..IDENT_INT(1),
                         IDENT_INT(-1)..IDENT_INT(1)) OF INTEGER;
     TYPE ARR23 IS ARRAY(INTEGER'(-1)..1,
                         IDENT_INT(-1)..IDENT_INT(1)) OF INTEGER;

     GENERIC
          A : ARR10;
     PROCEDURE GPROC10;

     GENERIC
          A : ARR11;
     PROCEDURE GPROC11;

     GENERIC
          A : ARR12;
     PROCEDURE GPROC12;

     GENERIC
          A : ARR20;
     PROCEDURE GPROC20;

     GENERIC
          A : ARR21;
     PROCEDURE GPROC21 (C : INTEGER);

     GENERIC
          A : ARR22;
     PROCEDURE GPROC22;

     GENERIC
          A : ARR23;
     PROCEDURE GPROC23;

     PROCEDURE GPROC10 IS
     BEGIN
          IF A'LENGTH /= IDENT_INT(0) THEN
               FAILED ("PROC10 ARRAY IS NOT NULL");
          END IF;
     END GPROC10;

     PROCEDURE GPROC11 IS
     BEGIN
          IF A'LENGTH /= IDENT_INT(7) OR
             A'FIRST /= IDENT_INT(-3) OR
             A'LAST /= IDENT_INT(3)   THEN
               FAILED ("INCORRECT LENGTH IN PROC11");
          END IF;

          FOR I IN IDENT_INT(-3)..IDENT_INT(3) LOOP
               IF IDENT_INT(A(I)) /= 1 THEN
                    FAILED ("INCORRECT VALUE OF COMPONENT " &
                            INTEGER'IMAGE(I) & ", PROC11");
               END IF;
          END LOOP;
     END GPROC11;

     PROCEDURE GPROC12 IS
     BEGIN
          IF A'LENGTH /= IDENT_INT(7) THEN
               FAILED ("INCORRECT LENGTH IN PROC12");
          END IF;

          FOR I IN IDENT_INT(-3)..IDENT_INT(3) LOOP
               IF IDENT_INT(A(I)) /= 2 THEN
                    FAILED ("INCORRECT VALUE OF COMPONENT " &
                            INTEGER'IMAGE(I) & ", PROC12");
               END IF;
          END LOOP;
     END GPROC12;

     PROCEDURE GPROC20 IS
     BEGIN
          IF A'LENGTH(1) /= IDENT_INT(0) OR
             A'LENGTH(2) /= IDENT_INT(0) THEN
               FAILED ("GPROC20 ARRAY IS NOT NULL");
          END IF;
     END GPROC20;

     PROCEDURE GPROC21 (C : INTEGER) IS
     BEGIN
          FOR I IN INTEGER'(-1)..1 LOOP
               FOR J IN INTEGER'(-1)..1 LOOP
                    IF IDENT_INT(A(I,J)) /= C THEN
                         FAILED ("INCORRECT VALUE OF COMPONENT (" &
                                 INTEGER'IMAGE(I) & "," &
                                 INTEGER'IMAGE(J) & "), GPROC21 CALL " &
                                 "NUMBER" & INTEGER'IMAGE(C));
                    END IF;
               END LOOP;
          END LOOP;
     END GPROC21;

     PROCEDURE GPROC22 IS
     BEGIN
          FOR I IN INTEGER'(-1)..1 LOOP
               FOR J IN INTEGER'(-1)..1 LOOP
                    IF IDENT_INT(A(I,J)) /= 3 THEN
                         FAILED ("INCORRECT VALUE OF COMPONENT (" &
                                 INTEGER'IMAGE(I) & "," &
                                 INTEGER'IMAGE(J) & "), GPROC22");
                    END IF;
               END LOOP;
          END LOOP;
     END GPROC22;

     PROCEDURE GPROC23 IS
     BEGIN
          FOR I IN INTEGER'(-1)..1 LOOP
               FOR J IN INTEGER'(-1)..1 LOOP
                    IF IDENT_INT(A(I,J)) /= 4 THEN
                         FAILED ("INCORRECT VALUE OF COMPONENT (" &
                                 INTEGER'IMAGE(I) & "," &
                                 INTEGER'IMAGE(J) & "), GPROC23");
                    END IF;
               END LOOP;
          END LOOP;
     END GPROC23;

     PROCEDURE PROC11 IS NEW GPROC11((1,1,1, OTHERS => 1));
     PROCEDURE PROC12 IS NEW GPROC12((OTHERS => 2));
     PROCEDURE PROC10 IS NEW GPROC10((OTHERS => 3));

     PROCEDURE PROC21 IS NEW GPROC21(((1,1,1), OTHERS => (1,1,1)));
     PROCEDURE PROC22 IS NEW GPROC21(((2,OTHERS => 2), (2,OTHERS => 2),
                                      (2,2,OTHERS => 2)));
     PROCEDURE PROC23 IS NEW GPROC22((OTHERS => (OTHERS => 3)));
     PROCEDURE PROC24 IS NEW GPROC23((OTHERS => (4,4,4)));
     PROCEDURE PROC20 IS NEW GPROC20((OTHERS => (OTHERS => 5)));

BEGIN
     TEST ("C43204C", "CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS " &
                      "CHOICE CAN APPEAR (AND BOUNDS ARE DETERMINED " &
                      "CORRECTLY) AS AN ACTUAL PARAMETER OF A " &
                      "SUBPROGRAM CALL WHEN THE FORMAL PARAMETER IS " &
                      "CONSTRAINED");

     PROC11;
     PROC12;
     PROC10;

     PROC21(1);
     PROC22(2);
     PROC23;
     PROC24;
     PROC20;

     RESULT;
END C43204C;
