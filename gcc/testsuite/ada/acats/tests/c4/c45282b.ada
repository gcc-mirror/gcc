-- C45282B.ADA

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
-- CHECK THAT IN AND NOT IN ARE EVALUATED CORRECTLY FOR :
--     D) ACCESS TO RECORD, PRIVATE, AND LIMITED PRIVATE TYPES WITH
--        DISCRIMINANTS (WITH AND WITHOUT DEFAULT VALUES), WHERE THE 
--        TYPE MARK DENOTES A CONSTRAINED AND UNCONSTRAINED TYPE;
--     E) ACCESS TO TASK TYPES.

-- TBN  8/8/86

WITH REPORT; USE REPORT;
PROCEDURE C45282B IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 5;

     PACKAGE P IS
          TYPE PRI_REC1 (D : INT) IS PRIVATE;
          TYPE PRI_REC2 (D : INT := 2) IS PRIVATE;
          FUNCTION INIT_PREC1 (A : INT; B : STRING) RETURN PRI_REC1;
          FUNCTION INIT_PREC2 (A : INT; B : STRING) RETURN PRI_REC2;
          TYPE LIM_REC1 (D : INT) IS LIMITED PRIVATE;
          TYPE ACC_LIM1 IS ACCESS LIM_REC1;
          SUBTYPE ACC_SUB_LIM1 IS ACC_LIM1 (2);
          PROCEDURE ASSIGN_LIM1 (A : ACC_LIM1; B : INT; C : STRING);
          TYPE LIM_REC2 (D : INT := 2) IS LIMITED PRIVATE;
          TYPE ACC_LIM2 IS ACCESS LIM_REC2;
          SUBTYPE ACC_SUB_LIM2 IS ACC_LIM2 (2);
          PROCEDURE ASSIGN_LIM2 (A : ACC_LIM2; B : INT; C : STRING);
     PRIVATE
          TYPE PRI_REC1 (D : INT) IS
               RECORD
                    STR : STRING (1 .. D);
               END RECORD;
          TYPE PRI_REC2 (D : INT := 2) IS
               RECORD
                    STR : STRING (1 .. D);
               END RECORD;
          TYPE LIM_REC1 (D : INT) IS
               RECORD
                    STR : STRING (1 .. D);
               END RECORD;
          TYPE LIM_REC2 (D : INT := 2) IS
               RECORD
                    STR : STRING (1 .. D);
               END RECORD;
     END P;

     USE P;

     TYPE DIS_REC1 (D : INT) IS
          RECORD
               STR : STRING (1 .. D);
          END RECORD;
     TYPE DIS_REC2 (D : INT := 5) IS
          RECORD
               STR : STRING (D .. 8);
          END RECORD;

     TYPE ACC1_REC1 IS ACCESS DIS_REC1;
     SUBTYPE ACC2_REC1 IS ACC1_REC1 (2);
     TYPE ACC1_REC2 IS ACCESS DIS_REC2;
     SUBTYPE ACC2_REC2 IS ACC1_REC2 (2);
     REC1 : ACC1_REC1;
     REC2 : ACC2_REC1;
     REC3 : ACC1_REC2;
     REC4 : ACC2_REC2;
     TYPE ACC_PREC1 IS ACCESS PRI_REC1;
     SUBTYPE ACC_SREC1 IS ACC_PREC1 (2);
     REC5 : ACC_PREC1;
     REC6 : ACC_SREC1;
     TYPE ACC_PREC2 IS ACCESS PRI_REC2;
     SUBTYPE ACC_SREC2 IS ACC_PREC2 (2);
     REC7 : ACC_PREC2;
     REC8 : ACC_SREC2;
     REC9 : ACC_LIM1;
     REC10 : ACC_SUB_LIM1;
     REC11 : ACC_LIM2;
     REC12 : ACC_SUB_LIM2;

     TASK TYPE T IS
          ENTRY E (X : INTEGER);
     END T;

     TASK BODY T IS
     BEGIN
          ACCEPT E (X : INTEGER) DO
               IF X /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT VALUE PASSED TO TASK");
               END IF;
          END E;
     END T;

     PACKAGE BODY P IS
          FUNCTION INIT_PREC1 (A : INT; B : STRING) RETURN PRI_REC1 IS
               REC : PRI_REC1 (A);
          BEGIN
               REC := (A, B);
               RETURN (REC);
          END INIT_PREC1;

          FUNCTION INIT_PREC2 (A : INT; B : STRING) RETURN PRI_REC2 IS
               REC : PRI_REC2;
          BEGIN
               REC := (A, B);
               RETURN (REC);
          END INIT_PREC2;

          PROCEDURE ASSIGN_LIM1 (A : ACC_LIM1; B : INT; C : STRING) IS
          BEGIN
               A.ALL := (B, C);
          END ASSIGN_LIM1;

          PROCEDURE ASSIGN_LIM2 (A : ACC_LIM2; B : INT; C : STRING) IS
          BEGIN
               A.ALL := (B, C);
          END ASSIGN_LIM2;
     END P;

BEGIN

     TEST ("C45282B", "CHECK THAT IN AND NOT IN ARE EVALUATED FOR " &
                      "ACCESS TYPES TO RECORD TYPES, PRIVATE TYPES, " &
                      "LIMITED PRIVATE TYPES WITH DISCRIMINANTS, AND " &
                      "TASK TYPES");

-- CASE D
------------------------------------------------------------------------
     IF REC1 NOT IN ACC1_REC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 1");
     END IF;
     IF REC1 IN ACC2_REC1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 2");
     END IF;
     IF REC2 NOT IN ACC1_REC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 3");
     END IF;
     REC1 := NEW DIS_REC1'(5, "12345");
     IF REC1 IN ACC1_REC1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 4");
     END IF;
     IF REC1 IN ACC2_REC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 5");
     END IF;
     REC2 := NEW DIS_REC1'(2, "HI");
     IF REC2 IN ACC1_REC1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 6");
     END IF;

------------------------------------------------------------------------

     IF REC3 IN ACC1_REC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 7");
     END IF;
     IF REC3 NOT IN ACC2_REC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 8");
     END IF;
     IF REC4 IN ACC1_REC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 9");
     END IF;
     REC3 := NEW DIS_REC2'(5, "5678");
     IF REC3 IN ACC1_REC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 10");
     END IF;
     IF REC3 IN ACC2_REC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 11");
     END IF;
     REC4 := NEW DIS_REC2'(2, "2345678");
     IF REC4 IN ACC1_REC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 12");
     END IF;
     IF REC4 NOT IN ACC2_REC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 13");
     END IF;

------------------------------------------------------------------------

     IF REC5 NOT IN ACC_PREC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 14");
     END IF;
     IF REC5 NOT IN ACC_SREC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 15");
     END IF;
     IF REC6 NOT IN ACC_PREC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 16");
     END IF;
     REC5 := NEW PRI_REC1'(INIT_PREC1 (5, "12345"));
     IF REC5 IN ACC_PREC1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 17");
     END IF;
     IF REC5 IN ACC_SREC1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 18");
     END IF;
     REC6 := NEW PRI_REC1'(INIT_PREC1 (2, "HI"));
     IF REC6 IN ACC_PREC1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 19");
     END IF;

------------------------------------------------------------------------

     IF REC7 NOT IN ACC_PREC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 20");
     END IF;
     IF REC7 NOT IN ACC_SREC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 21");
     END IF;
     IF REC8 NOT IN ACC_PREC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 22");
     END IF;
     REC7 := NEW PRI_REC2'(INIT_PREC2 (5, "12345"));
     IF REC7 IN ACC_PREC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 23");
     END IF;
     IF REC7 IN ACC_SREC2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 24");
     END IF;
     REC8 := NEW PRI_REC2'(INIT_PREC2 (2, "HI"));
     IF REC8 IN ACC_PREC2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 25");
     END IF;

------------------------------------------------------------------------

     IF REC9 NOT IN ACC_LIM1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 26");
     END IF;
     IF REC9 NOT IN ACC_SUB_LIM1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 27");
     END IF;
     IF REC10 NOT IN ACC_LIM1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 28");
     END IF;
     REC9 := NEW LIM_REC1 (5);
     ASSIGN_LIM1 (REC9, 5, "12345");
     IF REC9 IN ACC_LIM1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 29");
     END IF;
     IF REC9 IN ACC_SUB_LIM1 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 30");
     END IF;
     REC10 := NEW LIM_REC1 (2);
     ASSIGN_LIM1 (REC10, 2, "12");
     IF REC10 IN ACC_LIM1 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 31");
     END IF;

------------------------------------------------------------------------

     IF REC11 NOT IN ACC_LIM2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 32");
     END IF;
     IF REC11 NOT IN ACC_SUB_LIM2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 33");
     END IF;
     IF REC12 NOT IN ACC_LIM2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 34");
     END IF;
     REC11 := NEW LIM_REC2;
     IF REC11 NOT IN ACC_SUB_LIM2 THEN
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 35");
     END IF;
     ASSIGN_LIM2 (REC11, 2, "12");
     IF REC11 IN ACC_LIM2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 36");
     END IF;
     IF REC11 IN ACC_SUB_LIM2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 37");
     END IF;
     REC12 := NEW LIM_REC2;
     ASSIGN_LIM2 (REC12, 2, "12");
     IF REC12 IN ACC_LIM2 THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 38");
     END IF;

-- CASE E
------------------------------------------------------------------------
     DECLARE
          TYPE ACC_TASK IS ACCESS T;
          T1 : ACC_TASK;
     BEGIN
          IF T1 NOT IN ACC_TASK THEN
               FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 39");
          END IF;
          T1 := NEW T;
          IF T1 IN ACC_TASK THEN
               NULL;
          ELSE
               FAILED ("INCORRECT RESULTS FOR ACCESS TYPES - 38");
          END IF;
          T1.E (1);
     END;

     RESULT;
END C45282B;
