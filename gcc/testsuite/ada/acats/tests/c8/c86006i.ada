-- C86006I.ADA

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
--     CHECK THAT THE IDENTIFIERS "BOOLEAN, TRUE, AND FALSE" AND THE 
--     IDENTIFIERS "INTEGER, NATURAL, AND POSITIVE" ARE DECLARED IN 
--     THE PACKAGE "STANDARD", ALONG WITH THE OPERATORS OF THE TYPE 
--     BOOLEAN AND THE TYPE INTEGER.                                          

-- HISTORY:
--     DTN 04/15/92 CONSOLIDATION OF C86006A AND C86006B.

WITH REPORT; USE REPORT;
PROCEDURE C86006I IS
     
     ABOOL, BBOOL : STANDARD.BOOLEAN := STANDARD.FALSE;
     CBOOL : STANDARD.BOOLEAN := STANDARD.TRUE;
     INT1 : STANDARD.INTEGER := -2;
     NAT1 : STANDARD.NATURAL := 0;
     POS1, POS2 : STANDARD.POSITIVE := 2;

BEGIN

     TEST("C86006I", "CHECK THAT THE IDENTIFIERS ""BOOLEAN, TRUE, AND " &
                     "FALSE"" AND THE IDENTIFIERS ""INTEGER, NATURAL, " &
                     "AND POSITIVE"" ARE DECLARED IN THE PACKAGE " &
                     """STANDARD"", ALONG WITH THE OPERATORS OF THE " &
                     "TYPE BOOLEAN AND THE TYPE INTEGER");

     -- STANDARD.">" OPERATOR.

     IF STANDARD.">"(ABOOL,BBOOL) THEN
          FAILED("STANDARD.> FAILED FOR BOOLEAN TYPE");
     END IF;

     IF STANDARD.">"(INT1,NAT1) THEN
          FAILED("STANDARD.> FAILED FOR INTEGER-NATURAL TYPE");
     END IF;

     -- STANDARD."/=" OPERATOR.

     IF STANDARD."/="(ABOOL,BBOOL) THEN
          FAILED("STANDARD./= FAILED FOR BOOLEAN TYPE");
     END IF;

     IF STANDARD."/="(POS1,POS2) THEN
          FAILED("STANDARD./= FAILED FOR INTEGER-POSITIVE TYPE");
     END IF;

     -- STANDARD."AND" OPERATOR.

     IF STANDARD."AND"(CBOOL,ABOOL) THEN
          FAILED("STANDARD.AND FAILED");
     END IF;

     -- STANDARD."-" BINARY OPERATOR.

     IF STANDARD."-"(INT1,POS1) /= IDENT_INT(-4) THEN
          FAILED("STANDARD.- FAILED");
     END IF;

     -- STANDARD."-" UNARY OPERATOR.

     IF STANDARD."-"(INT1) /= IDENT_INT(2) THEN
          FAILED("STANDARD.UNARY - FAILED");
     END IF;

     -- STANDARD."REM" OPERATOR.

     IF STANDARD."REM"(IDENT_INT(14),IDENT_INT(5)) /= IDENT_INT(4) THEN
          FAILED("STANDARD.REM (++=+) FAILED");
     END IF;

     -- STANDARD."MOD" OPERATOR.

     IF STANDARD."MOD"(IDENT_INT(14),IDENT_INT(-5)) /= IDENT_INT(-1) THEN
          FAILED("STANDARD.MOD (+-=-) FAILED");
     END IF;

     RESULT;

END C86006I;
