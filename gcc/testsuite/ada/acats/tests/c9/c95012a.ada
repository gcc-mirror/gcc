-- C95012A.ADA

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
-- CHECK THAT A CALL TO AN ENTRY OF A TASK THAT HAS NOT BEEN ACTIVATED
--   DOES NOT RAISE EXCEPTIONS.

-- THIS TEST CONTAINS RACE CONDITIONS.

-- JRK 11/6/81
-- SPS 11/21/82
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C95012A IS

     I : INTEGER := 0;


BEGIN
     TEST ("C95012A", "CHECK THAT A CALL TO AN ENTRY OF A TASK " &
                      "THAT HAS NOT BEEN ACTIVATED DOES NOT " &
                      "RAISE EXCEPTIONS");

     DECLARE

          TASK T1 IS
               ENTRY E1 (I : OUT INTEGER);
          END T1;

          TASK TYPE T2T IS
               ENTRY E2 (I : OUT INTEGER);
          END T2T;

          TYPE AT2T IS ACCESS T2T;
          AT2 : AT2T;

          TASK BODY T1 IS
          BEGIN
               ACCEPT E1 (I : OUT INTEGER) DO
                    I := IDENT_INT (1);
               END E1;
          END T1;

          TASK BODY T2T IS
               J : INTEGER := 0;
          BEGIN
               BEGIN
                    T1.E1 (J);
               EXCEPTION
                    WHEN OTHERS =>
                         J := -1;
               END;
               ACCEPT E2 (I : OUT INTEGER) DO
                    I := J;
               END E2;
          END T2T;

          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               AT2 := NEW T2T;
               DELAY 60.0 * Impdef.One_Second;
          END PKG;

     BEGIN

          AT2.ALL.E2 (I);

          IF I = -1 THEN 
               FAILED ("EXCEPTION RAISED");
               T1.E1 (I);
          END IF;

          IF I /= 1 THEN
               FAILED ("WRONG VALUE PASSED");
          END IF;

     END;

     RESULT;
END C95012A;
