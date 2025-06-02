-- CD1C03B.ADA

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
--     CHECK THAT THE SIZE OF A DERIVED TYPE IS INHERITED FROM THE
--     PARENT IF THE SIZE OF THE PARENT WAS DETERMINED BY A PRAGMA
--     PACK.

-- HISTORY:
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     PWB 03/27/89  MODIFIED COMPARISON OF OBJECT SIZE TO PARENT SIZE.

WITH REPORT; USE REPORT;
PROCEDURE CD1C03B IS

     TYPE ENUM IS (E1, E2, E3);

     TYPE NORMAL_TYPE IS ARRAY (1 .. 100) OF ENUM;

     TYPE PARENT_TYPE IS ARRAY (1 .. 100) OF ENUM;
     PRAGMA PACK (PARENT_TYPE);

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;
     X : DERIVED_TYPE := (OTHERS => ENUM'FIRST);

BEGIN

     TEST("CD1C03B", "CHECK THAT THE SIZE OF A DERIVED TYPE IS " &
                     "INHERITED FROM THE PARENT IF THE SIZE OF " &
                     "THE PARENT WAS DETERMINED BY A PRAGMA PACK");

     IF PARENT_TYPE'SIZE = IDENT_INT (NORMAL_TYPE'SIZE) THEN
          COMMENT ("PRAGMA PACK HAD NO EFFECT ON THE SIZE OF " &
                   "PARENT_TYPE, WHICH IS" &
                   INTEGER'IMAGE(PARENT_TYPE'SIZE));
     ELSIF PARENT_TYPE'SIZE > IDENT_INT (NORMAL_TYPE'SIZE) THEN
          FAILED ("PARENT_TYPE'SIZE SHOULD NOT BE GREATER THAN" &
                  INTEGER'IMAGE(NORMAL_TYPE'SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(PARENT_TYPE'SIZE));
     END IF;

     IF DERIVED_TYPE'SIZE > IDENT_INT (PARENT_TYPE'SIZE) THEN
          FAILED ("DERIVED_TYPE'SIZE SHOULD NOT BE GREATER THAN" &
                  INTEGER'IMAGE(PARENT_TYPE'SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(DERIVED_TYPE'SIZE));
     END IF;

     IF X'SIZE < DERIVED_TYPE'SIZE THEN
          FAILED ("OBJECT SIZE TOO LARGE.  FIRST VALUE IS " &
                  ENUM'IMAGE ( X(1) ) );
     END IF;

     RESULT;

END CD1C03B;
