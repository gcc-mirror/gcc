-- C95066A.ADA

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
-- CHECK THAT A STATIC EXPRESSION, CONSTANT NAME, ATTRIBUTE NAME,
-- VARIABLE, DEREFERENCED ACCESS, USER-DEFINED OPERATOR, USER-
-- DEFINED FUNCTION, OR ALLOCATOR CAN BE USED IN THE INITIALIZATION
-- EXPRESSION OF A FORMAL PARAMETER, AND THAT THE APPROPRIATE
-- VALUE IS USED AS A DEFAULT PARAMETER VALUE WHEN THE ENTRY
-- IS CALLED.

-- GLH  6/19/85

WITH REPORT;
PROCEDURE C95066A IS

     USE REPORT;

     TYPE INT IS RANGE 1 .. 10;

     TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;

     TYPE RECTYPE (CONSTRAINT : INTEGER) IS
          RECORD
               A : ARR (0..CONSTRAINT);
          END RECORD;

     C7   : CONSTANT INTEGER  := 7;
     V7   : INTEGER  := 7;

     TYPE  A_INT IS ACCESS INTEGER;
     C_A  : CONSTANT A_INT  := NEW INTEGER'(7);

     SUBTYPE RECTYPE1 IS RECTYPE (2 + 5);
     SUBTYPE RECTYPE2 IS RECTYPE (C7);
     SUBTYPE RECTYPE3 IS RECTYPE (V7);

     FUNCTION "&" (X,Y : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN 10;
     END "&";

     FUNCTION FUNC (X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X;
     END FUNC;

      -- STATIC EXPRESSION.

     TASK T1 IS
          ENTRY E1 (REC : RECTYPE1 := (3+4,(0,1,2,3,4,5,6,7)));
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1 (REC : RECTYPE1 := (3+4,(0,1,2,3,4,5,6,7))) DO
               IF (REC /= (7,(0,1,2,3,4,5,6,7))) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E1 PARAMETER");
               END IF;
          END E1;
     END T1;

     -- CONSTANT NAME.

     TASK T2 IS
          ENTRY E2 (REC : RECTYPE2 := (C7,(0,1,2,3,4,5,6,7)));
     END T2;

     TASK BODY T2 IS
     BEGIN
          ACCEPT E2 (REC : RECTYPE2 := (C7,(0,1,2,3,4,5,6,7))) DO
               IF (REC /= (C7,(0,1,2,3,4,5,6,7))) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E2 PARAMETER");
               END IF;
          END E2;
     END T2;

     -- ATTRIBUTE NAME.

     TASK T3 IS
          ENTRY E3 (P1 : INT := INT'LAST);
     END T3;

     TASK BODY T3 IS
     BEGIN
          ACCEPT E3 (P1 : INT := INT'LAST) DO
               IF (P1 /= INT (10)) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E3 PARAMETER");
               END IF;
          END E3;
     END T3;

     -- VARIABLE.

     TASK T4 IS
          ENTRY E4 (P4 : RECTYPE3 := (V7,(0,1,2,3,4,5,6,7)));
     END T4;

     TASK BODY T4 IS
     BEGIN
          ACCEPT E4 (P4 : RECTYPE3 := (V7,(0,1,2,3,4,5,6,7))) DO
               IF (P4 /= (V7,(0,1,2,3,4,5,6,7))) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E4 PARAMETER");
               END IF;
          END E4;
     END T4;

     -- DEREFERENCED ACCESS.

     TASK T5 IS
          ENTRY E5 (P5 : INTEGER := C_A.ALL);
     END T5;

     TASK BODY T5 IS
     BEGIN
          ACCEPT E5 (P5 : INTEGER := C_A.ALL) DO
               IF (P5 /= C_A.ALL) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E5 PARAMETER");
               END IF;
          END E5;
     END T5;

     -- USER-DEFINED OPERATOR.

     TASK T6 IS
          ENTRY E6 (P6 : INTEGER := 6&4);
     END T6;

     TASK BODY T6 IS
     BEGIN
          ACCEPT E6 (P6 : INTEGER := 6&4) DO
               IF (P6 /= IDENT_INT(10)) THEN
                    FAILED ("INCORRECT DEFAULT VALUE " &
                            "FOR E6 PARAMETER");
               END IF;
          END E6;
     END T6;

     -- USER-DEFINED FUNCTION.

     TASK T7 IS
          ENTRY E7 (P7 : INTEGER := FUNC(10));
     END T7;

     TASK BODY T7 IS
     BEGIN
          ACCEPT E7 (P7 : INTEGER := FUNC(10)) DO
               IF (P7 /= IDENT_INT(10)) THEN
                    FAILED ("INCORRECT DEFAULT VALUE FOR " &
                            "E7 PARAMETER");
               END IF;
          END E7;
     END T7;

     -- ALLOCATOR.

     TASK T8 IS
          ENTRY E8 (P8 : A_INT := NEW INTEGER'(7));
     END T8;

     TASK BODY T8 IS
     BEGIN
          ACCEPT E8 (P8 : A_INT := NEW INTEGER'(7)) DO
               IF (P8.ALL /= IDENT_INT(7)) THEN
                    FAILED ("INCORRECT DEFAULT VALUE " &
                            "FOR E8 PARAMETER");
               END IF;
          END E8;
     END T8;

BEGIN
     TEST ("C95066A", "CHECK USE OF STATIC EXPRESSIONS, CONSTANT " &
                      "NAMES, ATTRIBUTE NAMES, VARIABLES, USER- " &
                      "DEFINED OPERATORS, USER-DEFINED FUNCTIONS, " &
                      "DEREFERENCED ACCESSES, AND ALLOCATORS IN " &
                      "THE FORMAL PART OF A TASK SPECIFICATION");

     T1.E1;
     T2.E2;
     T3.E3;
     T4.E4;
     T5.E5;
     T6.E6;
     T7.E7;
     T8.E8;

     RESULT;

END C95066A;
