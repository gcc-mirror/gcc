-- C23003B.TST

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
--
-- CHECK THAT THE NAME OF A LIBRARY UNIT PACKAGE AND THE NAME OF A LIBRARY
-- SUBPROGRAM CAN BE AS LONG AS THE LONGEST IDENTIFIER ALLOWED BY
-- AN IMPLEMENTATION.

-- JBG 5/26/85
-- DTN 3/25/92  CONSOLIDATION OF C23003B.TST AND C23003C.TST.
-- KAS 11/04/95 CHANGE "LINE" TO "IDENTIFIER"

PACKAGE
$BIG_ID1
IS
  A : INTEGER := 1;
END
$BIG_ID1
;
PACKAGE
$BIG_ID2
IS
  B : INTEGER := 2;
END
$BIG_ID2
;

PROCEDURE
$BIG_ID3
     (X : OUT INTEGER) IS
BEGIN
     X := 1;
END
$BIG_ID3
;
PROCEDURE
$BIG_ID4
     (X : OUT INTEGER) IS
BEGIN
     X := 2;
END
$BIG_ID4
;

WITH
$BIG_ID1
,
$BIG_ID2
,
$BIG_ID3
,
$BIG_ID4
;
USE
$BIG_ID1
,
$BIG_ID2
;

WITH REPORT; USE REPORT;
PROCEDURE C23003B IS
     X1, X2 : INTEGER := 0;
BEGIN
     TEST ("C23003B", "CHECK LONGEST POSSIBLE IDENTIFIER CAN BE USED " &
                      "FOR LIBRARY PACKAGE AND SUBPROGRAM");

     IF A + IDENT_INT(1) /= B THEN
          FAILED ("INCORRECT PACKAGE IDENTIFICATION");
     END IF;


$BIG_ID3
          (X1);
$BIG_ID4
          (X2);

     IF X1 + IDENT_INT(1) /= X2 THEN
          FAILED ("INCORRECT PROCEDURE IDENTIFICATION");
     END IF;

     RESULT;
END C23003B; 
