-- C87B62D.TST

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
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--       IN A LENGTH CLAUSE THAT SPECIFIES 'STORAGE_SIZE,
--       THE EXPRESSION MUST BE OF SOME INTEGER TYPE.
--       TASK TYPE IS HERE; ACCESS TYPE IS IN C87B62B.DEP.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     TRH 09/08/82  CREATED ORIGINAL TEST.
--     EG  06/04/84
--     PWB 01/19/86  CREATED THIS TEST FILE FROM THE TASK TYPE PART
--                   OF THE OLD C87B62B;
--                   CLARIFIED COMMENTS REGARDING NON-APPLICABILITY.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.TST'.

WITH REPORT; USE REPORT;

PROCEDURE C87B62D IS

     TASK_STORAGE_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

     TYPE POS_INT IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
     TYPE POS_FIX IS DELTA 0.1 RANGE 0.0 .. 10.0;
     TYPE NUMERAL IS NEW CHARACTER RANGE '0' .. '9';
     TYPE BASE_5  IS ('0', '1', '2', '3', '4');
     ERR : BOOLEAN := FALSE;

     FUNCTION F (X : INTEGER) RETURN NUMERAL IS
     BEGIN
          ERR := TRUE;
          RETURN ('9');
     END F;

     FUNCTION F (X : INTEGER) RETURN BASE_5 IS
     BEGIN
          ERR := TRUE;
          RETURN ('4');
     END F;

     FUNCTION F (X : INTEGER) RETURN POS_FIX IS
     BEGIN
          ERR := TRUE;
          RETURN POS_FIX (X);
     END F;

     FUNCTION F (X : INTEGER) RETURN POS_INT IS
     BEGIN
          RETURN POS_INT (X);
     END F;

BEGIN
     TEST ("C87B62D","OVERLOADED EXPRESSION WITHIN LENGTH CLAUSE " &
           "- SPECIFICATION OF ATTRIBUTE T'STORAGE_SIZE " &
           "FOR TASK TYPES ");

     DECLARE

          TASK TYPE TSK1  IS
          END TSK1;

          FOR TSK1'STORAGE_SIZE USE F (TASK_STORAGE_SIZE);

          TASK BODY TSK1 IS
          BEGIN
               NULL;
          END TSK1;

     BEGIN
          IF ERR THEN
               FAILED ("RESOLUTION INCORRECT FOR EXPRESSION IN " &
                       "LENGTH CLAUSE USING 'STORAGE_SIZE");
          END IF;
     END;

     RESULT;
END C87B62D;
