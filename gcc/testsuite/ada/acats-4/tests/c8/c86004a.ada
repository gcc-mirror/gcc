-- C86004A.ADA

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
--     CHECK THAT IF A LIBRARY SUBPROGRAM DECLARATION IS PRECEDED BY A
--     "WITH" CLAUSE FOR A GENERIC LIBRARY PROCEDURE M, THEN IN THE
--     BODY OF THE SUBPROGRAM, "STANDARD.M" IS A LEGAL NAME
--     FOR THE GENERIC PROCEDURE.

-- HISTORY:
--     DHH 03/14/88 CREATED ORIGINAL TEST.

-- BEGIN BUILDING LIBRARY PROCEDURES

GENERIC
     TYPE ITEM IS (<>);
PROCEDURE C86004A_SWAP(X,Y: IN OUT ITEM);

PROCEDURE C86004A_SWAP(X,Y: IN OUT ITEM) IS
     T : ITEM;
BEGIN
     T := X;
     X := Y;
     Y := T;
END C86004A_SWAP;

WITH C86004A_SWAP; WITH REPORT; USE REPORT;
PROCEDURE C86004A1 IS
     SUBTYPE INT IS INTEGER RANGE 0 .. 10;
     A : INT := IDENT_INT(10);
     B : INT := IDENT_INT(0);
     PROCEDURE SWITCH IS NEW STANDARD.C86004A_SWAP(INT);
BEGIN
     SWITCH(A,B);

     IF A /= IDENT_INT(0) THEN
          FAILED("STANDARD.GENERIC PROCEDURE - 1");
     END IF;

     IF B /= IDENT_INT(10) THEN
          FAILED("STANDARD.GENERIC PROCEDURE - 2");
     END IF;
END C86004A1;

WITH C86004A_SWAP; WITH REPORT; USE REPORT;
PROCEDURE C86004A2;

PROCEDURE C86004A2 IS
     SUBTYPE INT IS INTEGER RANGE 0 .. 10;
     A : INT := IDENT_INT(10);
     B : INT := IDENT_INT(0);
BEGIN
     DECLARE
          PROCEDURE SWITCH IS NEW STANDARD.C86004A_SWAP(INT);
     BEGIN
          SWITCH(A,B);
     END;
     IF A /= IDENT_INT(0) THEN
          FAILED("STANDARD.GENERIC PROCEDURE - B-0");
     END IF;
     IF B /= IDENT_INT(10) THEN
          FAILED("STANDARD.GENERIC PROCEDURE - B-10");
     END IF;
END C86004A2;

WITH C86004A1; WITH C86004A2;
WITH REPORT; USE REPORT;
PROCEDURE C86004A IS
BEGIN
     TEST("C86004A", "CHECK THAT IF A LIBRARY SUBPROGRAM DECLARATION " &
                     "IS PRECEDED BY A ""WITH"" CLAUSE FOR A GENERIC " &
                     "LIBRARY PROCEDURE M, THEN IN THE BODY OF THE " &
                     "SUBPROGRAM, ""STANDARD.M"" IS A " &
                     "LEGAL NAME FOR THE GENERIC PROCEDURE");
     C86004A1;
     C86004A2;

     RESULT;
END C86004A;
