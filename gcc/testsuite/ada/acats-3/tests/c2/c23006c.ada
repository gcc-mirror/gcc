-- C23006C.ADA

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
-- CHECK THAT UNDERSCORES ARE SIGNFICANT IN NAMES OF LIBRARY
-- SUBPROGRAMS. 

-- JBG 5/26/85

PROCEDURE C23006C_PROC (X : OUT INTEGER) IS
BEGIN
     X := 1;
END C23006C_PROC;

PROCEDURE C23006CPROC (X : OUT INTEGER);

PROCEDURE C23006CPROC (X : OUT INTEGER) IS
BEGIN
     X := 2;
END C23006CPROC;

FUNCTION C23006C_FUNC RETURN INTEGER IS
BEGIN
     RETURN 3;
END C23006C_FUNC;

FUNCTION C23006CFUNC RETURN INTEGER;

WITH REPORT; USE REPORT;
PRAGMA ELABORATE (REPORT);
FUNCTION C23006CFUNC RETURN INTEGER IS
BEGIN
     RETURN IDENT_INT(4);
END C23006CFUNC;

WITH C23006C_PROC, C23006CPROC, C23006C_FUNC, C23006CFUNC;
WITH REPORT; USE REPORT;
PROCEDURE C23006C IS
     X1, X2 : INTEGER;
BEGIN
     TEST ("C23006C", "CHECK UNDERSCORES ARE SIGNIFICANT " &
                      "FOR LIBRARY SUBPROGRAM");

     C23006C_PROC (X1);
     C23006CPROC (X2);
     IF X1 + IDENT_INT(1) /= X2 THEN
          FAILED ("INCORRECT PROCEDURE IDENTIFICATION");
     END IF;

     IF C23006C_FUNC + IDENT_INT(1) /= C23006CFUNC THEN
          FAILED ("INCORRECT FUNCTION IDENTIFICATION");
     END IF;

     RESULT;
END C23006C;
