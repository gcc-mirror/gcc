-- C23003I.TST

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
-- CHECK THAT THE LONGEST POSSIBLE IDENTIFIER CAN BE THE NAME OF A
-- LIBRARY PACKAGE CREATED BY A GENERIC INSTANTIATION.

-- JBG 5/26/85
-- DTN 3/25/92  DELETED TEST OF TWO MAXIMUM LENGTH PACKAGE NAMES THAT
--              DIFFER ONLY IN THEIR MIDDLE CHARACTER.

GENERIC
     C : INTEGER;
PACKAGE C23003I_PKG IS
     A : INTEGER := C;
END C23003I_PKG;

WITH C23003I_PKG;
PRAGMA ELABORATE (C23003I_PKG);
PACKAGE
$BIG_ID1
     IS NEW C23003I_PKG (1);

WITH REPORT; USE REPORT;
WITH C23003I_PKG;
PRAGMA ELABORATE (REPORT, C23003I_PKG);
PACKAGE
$BIG_ID2
     IS NEW C23003I_PKG (IDENT_INT(2));

WITH
$BIG_ID1
,
$BIG_ID2
;
WITH REPORT; USE REPORT;
PROCEDURE C23003I IS
BEGIN
     TEST ("C23003I", "CHECK THAT LONGEST POSSIBLE IDENTIFIER CAN BE " &
                      "USED FOR A LIBRARY PACKAGE INSTANTIATION");

     IF 
$BIG_ID1
          .A + IDENT_INT(1) /=
$BIG_ID2
          .A THEN
              FAILED ("INCORRECT PACKAGE IDENTIFICATION");
     END IF;

     RESULT;
END C23003I;
