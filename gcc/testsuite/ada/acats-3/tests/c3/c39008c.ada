-- C39008C.ADA

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
--     CHECK THAT PROGRAM_ERROR IS RAISED WHEN AN ATTEMPT IS MADE TO
--     ACTIVATE A TASK BEFORE ITS BODY HAS BEEN ELABORATED.  CHECK THE
--     CASE IN WHICH SEVERAL TASKS ARE TO BE ACTIVATED, AND ONLY SOME
--     HAVE UNELABORATED BODIES; NO TASKS SHOULD BE ACTIVATED.

-- HISTORY:
--     BCB 07/08/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C39008C IS

BEGIN
     TEST ("C39008C", "CHECK THAT PROGRAM_ERROR IS RAISED WHEN AN " &
                      "ATTEMPT IS MADE TO ACTIVATE A TASK BEFORE ITS " &
                      "BODY HAS BEEN ELABORATED.  CHECK THE CASE IN " &
                      "WHICH SEVERAL TASKS ARE TO BE ACTIVATED, AND " &
                      "ONLY SOME HAVE UNELABORATED BODIES; NO TASKS " &
                      "SHOULD BE ACTIVATED");

     BEGIN
          DECLARE
               TASK TYPE A;

               TASK TYPE B;

               TASK TYPE C;

               TASK TYPE D;

               PACKAGE P IS
                    W : A;
                    X : B;
                    Y : C;
                    Z : D;
               END P;

               TASK BODY A IS
               BEGIN
                    FAILED ("TASK A ACTIVATED");
               END A;

               TASK BODY D IS
               BEGIN
                    FAILED ("TASK D ACTIVATED");
               END D;

               PACKAGE BODY P IS
               END P;

               TASK BODY B IS
               BEGIN
                    FAILED ("TASK B ACTIVATED");
               END B;

               TASK BODY C IS
               BEGIN
                    FAILED ("TASK C ACTIVATED");
               END C;
          BEGIN
               FAILED ("PROGRAM_ERROR WAS NOT RAISED");
          END;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN PROGRAM_ERROR WAS " &
                       "RAISED");
     END;

     RESULT;
END C39008C;
