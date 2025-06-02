-- CA5004A.ADA

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
-- CHECK THAT IF PRAGMA ELABORATE IS APPLIED TO A PACKAGE THAT DECLARES
-- A TASK OBJECT, THE IMPLICIT PACKAGE BODY IS ELABORATED AND THE TASK
-- IS ACTIVATED.

-- BHS 8/03/84
-- JRK 9/20/84
-- PWN 01/31/95  ADDED A PROCEDURE TO REQUIRE A BODY FOR ADA 9X.


PACKAGE CA5004A0 IS

     TASK TYPE TSK IS
          ENTRY E (VAR : OUT INTEGER);
     END TSK;

END CA5004A0;


PACKAGE BODY CA5004A0 IS

     TASK BODY TSK IS
     BEGIN
          ACCEPT E (VAR : OUT INTEGER) DO
               VAR := 4;
          END E;
     END TSK;

END CA5004A0;


WITH CA5004A0; USE CA5004A0; PRAGMA ELABORATE (CA5004A0);
PACKAGE CA5004A1 IS

     T : TSK;

END CA5004A1;


PACKAGE CA5004A2 IS
     PROCEDURE REQUIRE_BODY;
END CA5004A2;


WITH REPORT; USE REPORT;
WITH CA5004A1; USE CA5004A1;
PRAGMA ELABORATE (CA5004A1, REPORT);
PACKAGE BODY CA5004A2 IS

     I : INTEGER := 1;

     PROCEDURE REQUIRE_BODY IS
     BEGIN
          NULL;
     END;
BEGIN

     TEST ("CA5004A", "APPLYING PRAGMA ELABORATE TO A PACKAGE " &
                      "DECLARING A TASK OBJECT CAUSES IMPLICIT " &
                      "BODY ELABORATION AND TASK ACTIVATION");

     SELECT
          T.E(I);
          IF I /= 4 THEN
               FAILED ("TASK NOT EXECUTED PROPERLY");
          END IF;
     OR
          DELAY 10.0;
          FAILED ("TASK NOT ACTIVATED AFTER 10 SECONDS");
     END SELECT;

END CA5004A2;


WITH CA5004A2;
WITH REPORT; USE REPORT;
PROCEDURE CA5004A IS
BEGIN

     RESULT;

END CA5004A;
