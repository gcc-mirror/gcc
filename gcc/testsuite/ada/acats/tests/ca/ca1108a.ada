-- CA1108A.ADA

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
-- CHECK THAT A WITH_CLAUSE AND USE_CLAUSE GIVEN FOR A PACKAGE
-- SPECIFICATION APPLIES TO THE BODY AND SUBUNITS OF THE BODY.

-- BHS 7/27/84
-- JBG 5/1/85

PACKAGE OTHER_PKG IS

     I : INTEGER := 4;
     FUNCTION F (X : INTEGER) RETURN INTEGER;

END OTHER_PKG;

PACKAGE BODY OTHER_PKG IS

     FUNCTION F (X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN X + 1;
     END F;

END OTHER_PKG;

WITH REPORT, OTHER_PKG;
USE REPORT, OTHER_PKG;
PRAGMA ELABORATE (OTHER_PKG);
PACKAGE CA1108A_PKG IS

     J : INTEGER := 2;
     PROCEDURE PROC;
     PROCEDURE CALL_SUBS (X, Y : IN OUT INTEGER);

END CA1108A_PKG;

PACKAGE BODY CA1108A_PKG IS
     
     PROCEDURE SUB (X, Y : IN OUT INTEGER) IS SEPARATE;

     PROCEDURE PROC IS
          Y : INTEGER := 2;
     BEGIN
          Y := OTHER_PKG.I;
          IF Y /= 4 THEN
               FAILED ("OTHER_PKG VARIABLE NOT VISIBLE " &
                       "IN PACKAGE BODY PROCEDURE");
          END IF;
     END PROC;

     PROCEDURE CALL_SUBS (X, Y : IN OUT INTEGER) IS
     BEGIN
          SUB (X, Y);
     END CALL_SUBS;

BEGIN  

     J := F(J);            -- J => J + 1.
     IF J /= 3 THEN
          FAILED ("OTHER_PKG FUNCTION NOT VISIBLE IN " &
                  "PACKAGE BODY");
     END IF;

END CA1108A_PKG;


WITH REPORT, CA1108A_PKG;
USE REPORT, CA1108A_PKG;
PROCEDURE CA1108A IS

     VAR1, VAR2 : INTEGER;

BEGIN  

     TEST ("CA1108A", "WITH_ AND USE_CLAUSES GIVEN FOR A PACKAGE " &
                      "SPEC APPLY TO THE BODY AND ITS SUBUNITS");

     PROC;

     VAR1 := 1;
     VAR2 := 1;
     CALL_SUBS (VAR1, VAR2);
     IF VAR1 /= 4 THEN
          FAILED ("OTHER_PKG VARIABLE NOT VISIBLE IN SUBUNIT");
     END IF;

     IF VAR2 /= 6 THEN
          FAILED ("OTHER_PKG FUNCTION NOT VISIBLE IN SUBUNIT " &
                  "OF SUBUNIT");
     END IF;

     RESULT;

END CA1108A;


SEPARATE (CA1108A_PKG)
PROCEDURE SUB (X, Y : IN OUT INTEGER) IS
     PROCEDURE SUB2 (Z : IN OUT INTEGER) IS SEPARATE;
BEGIN

     X := I;
     SUB2 (Y);

END SUB;


SEPARATE (CA1108A_PKG.SUB)
PROCEDURE SUB2 (Z : IN OUT INTEGER) IS
     I : INTEGER := 5;
BEGIN

     Z := OTHER_PKG.F(I);    -- Z => I + 1.

END SUB2;
