-- C93006A.ADA

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
-- CHECK THAT A TASK OBJECT DECLARED IN A LIBRARY PACKAGE SPEC IS
-- ACTIVATED EVEN IF THE PACKAGE HAS NO BODY.

-- JEAN-PIERRE ROSEN 16-MAR-1984
-- JBG 6/1/84
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

WITH SYSTEM; USE SYSTEM;
PACKAGE C93006A0 IS
     TASK TYPE TT IS
          ENTRY E;
     END;
END C93006A0;

PACKAGE BODY C93006A0 IS
     TASK BODY TT IS
     BEGIN
          ACCEPT E;
     END;
END C93006A0;

WITH C93006A0; USE C93006A0;
PRAGMA ELABORATE(C93006A0);
PACKAGE C93006A1 IS
     T : TT;
END C93006A1;

with Impdef;
WITH REPORT, C93006A1, SYSTEM;
USE  REPORT, C93006A1, SYSTEM;
PROCEDURE C93006A IS
BEGIN

     TEST("C93006A", "CHECK ACTIVATION OF TASK DECLARED IN PACKAGE " &
                     "SPECIFICATION");

     SELECT
          T.E;
     OR 
          DELAY 60.0 * Impdef.One_Second;
          FAILED("RENDEZVOUS NOT ACCEPTED WITHIN 60 SECONDS");
     END SELECT;

     RESULT;
END C93006A;
