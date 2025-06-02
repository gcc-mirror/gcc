-- C85004B.ADA

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
--     CHECK THAT A RENAMED CONSTANT OBJECT, "IN" PARAMETER OF A
--     SUBPROGRAM OR ENTRY, "IN" FORMAL GENERIC, RECORD DISCRIMINANT,
--     LOOP PARAMETER, DEFERRED CONSTANT, OR RENAMED CONSTANT HAS THE
--     CORRECT VALUE.

-- HISTORY:
--     JET 07/25/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85004B IS

     TYPE A IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
     SUBTYPE P IS POSITIVE RANGE 1 .. 10;

     C1 : CONSTANT INTEGER := 1;
     X1 : INTEGER RENAMES C1;
     X2 : INTEGER RENAMES X1;

     TYPE REC (D : P := 1) IS
          RECORD
               I : A(1..D);
          END RECORD;
     TYPE ACCREC1 IS ACCESS REC;
     TYPE ACCREC2 IS ACCESS REC(10);

     R1 : REC;
     R2 : REC(10);
     AR1 : ACCREC1 := NEW REC;
     AR2 : ACCREC2 := NEW REC(10);

     X3 : P RENAMES R1.D;
     X4 : P RENAMES R2.D;
     X5 : P RENAMES AR1.D;
     X6 : P RENAMES AR2.D;

     C2 : CONSTANT A(1..3) := (1, 2, 3);
     X7 : INTEGER RENAMES C2(1);

     GENERIC
          K1 : IN INTEGER;
     PACKAGE GENPKG IS
          TYPE K IS PRIVATE;
          K2 : CONSTANT K;
     PRIVATE
          TYPE K IS RANGE 1..100;
          K2 : CONSTANT K := 5;
     END GENPKG;

     TASK FOOEY IS
          ENTRY ENT1 (I : IN INTEGER);
     END FOOEY;

     TASK BODY FOOEY IS
     BEGIN
          ACCEPT ENT1 (I : IN INTEGER) DO
               DECLARE
                    TX1 : INTEGER RENAMES I;
               BEGIN
                    IF TX1 /= IDENT_INT(2) THEN
                         FAILED ("INCORRECT VALUE");
                    END IF;
               END;
          END ENT1;
     END FOOEY;

     PACKAGE BODY GENPKG IS
          KX1 : INTEGER RENAMES K1;
          KX2 : K RENAMES K2;
     BEGIN
          IF KX1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF KX1");
          END IF;

          IF KX2 /= K(IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF KX2");
          END IF;
     END GENPKG;

     PROCEDURE PROC (I : IN INTEGER) IS
          PX1 : INTEGER RENAMES I;
     BEGIN
          IF PX1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF PX1");
          END IF;
     END PROC;

     PACKAGE PKG IS NEW GENPKG(4);

BEGIN
     TEST ("C85004B", "CHECK THAT A RENAMED CONSTANT OBJECT, 'IN' " &
           "PARAMETER OF A SUBPROGRAM OR ENTRY, 'IN' FORMAL GENERIC, " &
           "RECORD DISCRIMINANT, LOOP PARAMETER, DEFERRED CONSTANT, " &
           "OR RENAMED CONSTANT HAS THE CORRECT VALUE");

     FOOEY.ENT1(2);

     PROC(3);

     IF X1 /= IDENT_INT(1) THEN
          FAILED ("INCORRECT VALUE OF X1");
     END IF;

     IF X2 /= IDENT_INT(1) THEN
          FAILED ("INCORRECT VALUE OF X2");
     END IF;

     IF X3 /= IDENT_INT(1) THEN
          FAILED ("INCORRECT VALUE OF X3");
     END IF;

     IF X4 /= IDENT_INT(10) THEN
          FAILED ("INCORRECT VALUE OF X4");
     END IF;

     IF X5 /= IDENT_INT(1) THEN
          FAILED ("INCORRECT VALUE OF X5");
     END IF;

     IF X6 /= IDENT_INT(10) THEN
          FAILED ("INCORRECT VALUE OF X6");
     END IF;

     IF X7 /= IDENT_INT(1) THEN
          FAILED ("INCORRECT VALUE OF X7");
     END IF;

     FOR I IN 1..IDENT_INT(2) LOOP
          DECLARE
               X8 : INTEGER RENAMES I;
          BEGIN
               IF X8 /= IDENT_INT(I) THEN
                    FAILED ("INCORRECT VALUE OF X8");
               END IF;
          END;
     END LOOP;

     RESULT;

END C85004B;
