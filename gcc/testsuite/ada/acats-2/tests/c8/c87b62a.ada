-- C87B62A.ADA

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
--       IN A LENGTH CLAUSE THAT SPECIFIES 'SIZE,
--       THE EXPRESSION MUST BE OF SOME INTEGER TYPE.

-- HISTORY:
--     TRH 09/08/82  CREATED ORIGINAL TEST.
--     PWB 02/19/85  ADDED COMMENTS CLARIFYING NON-APPLICABILITY;
--                   DELETED TEXT NOT RELATED TO TEST OBJECTIVE.
--     BCB 01/04/88  MODIFIED HEADER.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;

PROCEDURE C87B62A IS

     TYPE POS_INT IS RANGE 1 .. INTEGER'LAST;
     TYPE POS_FIX IS DELTA 0.1 RANGE 0.0 .. 10.0;
     ERR : BOOLEAN := FALSE;

     FUNCTION "+" (X : POS_INT) RETURN POS_FIX IS
     BEGIN
          ERR := TRUE;
          RETURN POS_FIX (X);
     END "+";

     FUNCTION "+" (X : POS_FIX) RETURN POS_INT IS
     BEGIN
          ERR := TRUE;
          RETURN POS_INT (X);
     END "+";

BEGIN
     TEST ("C87B62A","OVERLOADED EXPRESSION WITHIN LENGTH CLAUSE " &
           "- SPECIFICATION OF ATTRIBUTE T'SIZE");

     DECLARE
          TYPE DECEM IS NEW INTEGER RANGE    1 .. 10;
          TYPE JUST_LIKE_DECEM IS NEW INTEGER RANGE 1 .. 10;
          DECEM_SIZE :  CONSTANT  := JUST_LIKE_DECEM'SIZE;
          TYPE CHECK IS NEW INTEGER RANGE 1 .. 10;

          FOR CHECK'SIZE USE DECEM_SIZE;
          FOR DECEM'SIZE        USE + DECEM_SIZE;

     BEGIN
          IF ERR THEN
               FAILED ("RESOLUTION INCORRECT FOR EXPRESSION IN " &
                       "LENGTH CLAUSE USING 'SIZE");
          END IF;
     END;

     RESULT;
END C87B62A;
