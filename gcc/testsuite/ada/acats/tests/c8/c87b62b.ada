-- C87B62B.ADA

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
--       ACCESS TYPES ARE HERE; TASK TYPES ARE IN C87B62D.DEP.

-- HISTORY:
--     TRH 09/08/82  CREATED ORIGINAL TEST.
--     EG  06/04/84
--     PWB 01/19/86  CLARIFIED COMMENTS REGARDING NON-APPLICABILITY;
--                   REMOVED TEXT NOT RELATED TO TEST OBJECTIVE
--                   MOVED TASK TYPES TO C87B62D.DEP.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;

PROCEDURE C87B62B IS

     TYPE POS_FIX IS DELTA 0.1 RANGE 0.0 .. 10.0;
     TYPE POS_INT IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
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
     TEST ("C87B62B","OVERLOADED EXPRESSION WITHIN LENGTH CLAUSE " &
                     "- SPECIFICATION OF ATTRIBUTE T'STORAGE_SIZE " &
                     "FOR ACCESS TYPES");

     DECLARE

          TYPE DECEM IS NEW INTEGER RANGE    1 .. 10;
          TYPE LINK  IS ACCESS DECEM;

          TYPE JUST_LIKE_LINK IS ACCESS DECEM;
          TYPE CHECK IS ACCESS DECEM;

          FOR CHECK'STORAGE_SIZE
               USE 1024;
          FOR LINK'STORAGE_SIZE USE F (1024);

     BEGIN
          IF ERR THEN
               FAILED ("RESOLUTION INCORRECT FOR EXPRESSION IN " &
                       "LENGTH CLAUSE USING 'STORAGE_SIZE");
          END IF;
     END;

     RESULT;
END C87B62B;
