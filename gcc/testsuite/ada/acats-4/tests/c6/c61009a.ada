-- C61009A.ADA

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
--   VARIABLE, DEREFERENCED ACCESS, USER-DEFINED OPERATOR, USER-
--   DEFINED FUNCTION, OR ALLOCATOR CAN BE USED IN THE INITIALIZATION
--   EXPRESSION OF A FORMAL PARAMETER, AND THAT THE APPROPRIATE
--   VALUE IS USED AS A DEFAULT PARAMETER VALUE WHEN THE SUBPROGRAM
--   IS CALLED.

-- DAS  1/21/81
-- ABW  7/20/82
-- SPS  12/10/82

WITH REPORT;
PROCEDURE C61009A IS

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

     -- STATIC EXPRESSION

     PROCEDURE PROC1 (REC : RECTYPE1 := (3+4,(0,1,2,3,4,5,6,7))) IS
     BEGIN
          IF (REC /= (7,(0,1,2,3,4,5,6,7))) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC1 PARAMETER");
          END IF;
     END PROC1;

     -- CONSTANT NAME

     PROCEDURE PROC2 (REC : RECTYPE2 := (C7,(0,1,2,3,4,5,6,7))) IS
     BEGIN
          IF (REC /= (C7,(0,1,2,3,4,5,6,7))) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC2 PARAMETER");
          END IF;
     END PROC2;

     -- ATTRIBUTE NAME

     PROCEDURE PROC3 (P1 : INT := INT'LAST) IS
     BEGIN
          IF (P1 /= INT (10)) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC3 PARAMETER");
          END IF;
     END PROC3;

     -- VARIABLE

     PROCEDURE PROC4 (P4 : RECTYPE3 := (V7,(0,1,2,3,4,5,6,7))) IS
     BEGIN
          IF (P4 /= (V7,(0,1,2,3,4,5,6,7))) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC4 PARAMETER");
               END IF;
          END PROC4;

     --DEREFERENCED ACCESS

     PROCEDURE PROC5 (P5 : INTEGER := C_A.ALL) IS
     BEGIN
          IF(P5 /= C_A.ALL) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC5 PARAMETER");
          END IF;
     END PROC5;

     --USER-DEFINED OPERATOR

     PROCEDURE PROC6 (P6 : INTEGER := 6&4) IS
     BEGIN
          IF (P6 /= IDENT_INT(10)) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC6 PARAMETER");
          END IF;
     END PROC6;

     --USER-DEFINED FUNCTION

     PROCEDURE PROC7 (P7 : INTEGER := FUNC(10)) IS
     BEGIN
          IF (P7 /= IDENT_INT(10)) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC7 PARAMETER");
          END IF;
     END PROC7;

     -- ALLOCATOR

     PROCEDURE PROC8 (P8 : A_INT := NEW INTEGER'(7)) IS
     BEGIN
          IF (P8.ALL /= IDENT_INT(7)) THEN
               FAILED ("INCORRECT DEFAULT VALUE FOR PROC8 PARAMETER");
          END IF;
     END PROC8;

BEGIN
     TEST ("C61009A", "CHECK USE OF STATIC EXPRESSIONS, CONSTANT " &
                      "NAMES, ATTRIBUTE NAMES, VARIABLES, USER- " &
                      "DEFINED OPERATORS, USER-DEFINED FUNCTIONS " &
                      "DEREFERENCED ACCESSES, AND ALLOCATORS IN " &
                      "THE FORMAL PART OF A SUBPROGRAM SPECIFICATION");

     PROC1;
     PROC2;
     PROC3;
     PROC4;
     PROC5;
     PROC6;
     PROC7;
     PROC8;

     RESULT;

END C61009A;
