-- CB4007A.ADA

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
--     CHECK THAT THE STATEMENT PART OF A PACKAGE CAN RAISE, PROPAGATE,
--     AND HANDLE EXCEPTIONS. IF THE BODY'S HANDLERS HANDLE ALL
--     EXCEPTIONS RAISED AND DO NOT RAISE ANY UNHANDLED EXCEPTIONS,
--     NO EXCEPTION IS PROPAGATED.

-- HISTORY:
--     DHH 03/28/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CB4007A IS
BEGIN

     TEST("CB4007A", "CHECK THAT THE STATEMENT PART OF A PACKAGE " &
                     "CAN RAISE, PROPAGATE, AND HANDLE EXCEPTIONS. " &
                     "IF THE BODY'S HANDLERS HANDLE ALL EXCEPTIONS " &
                     "RAISED AND DO NOT RAISE ANY UNHANDLED " &
                     "EXCEPTIONS, NO EXCEPTION IS PROPAGATED");
     DECLARE

          PACKAGE OUTSIDE IS
          END OUTSIDE;

          PACKAGE BODY OUTSIDE IS

          BEGIN
               DECLARE
                    PACKAGE HANDLER IS
                    END HANDLER;

                    PACKAGE BODY HANDLER IS
                    BEGIN
                         DECLARE
                              PACKAGE PROPAGATE IS
                              END PROPAGATE;

                              PACKAGE BODY PROPAGATE IS
                              BEGIN
                                   DECLARE
                                        PACKAGE RISE IS
                                        END RISE;

                                        PACKAGE BODY RISE IS
                                        BEGIN
                                             RAISE CONSTRAINT_ERROR;
                                             FAILED("EXCEPTION " &
                                                    "NOT RAISED");
                                        END RISE;

                                   BEGIN
                                        NULL;
                                   END;   -- PACKAGE PROPAGATE DECLARE.
                              EXCEPTION
                                   WHEN CONSTRAINT_ERROR =>
                                        RAISE CONSTRAINT_ERROR;
                                   WHEN OTHERS =>
                                        FAILED("UNEXPECTED EXCEPTION " &
                                               "RAISED IN PROPAGATE " &
                                               "PACKAGE");
                              END PROPAGATE;

                         BEGIN
                              NULL;
                         END;               -- PACKAGE HANDLER DECLARE.
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                             NULL;
                         WHEN OTHERS =>
                              FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                                     "HANDLER PACKAGE");
                    END HANDLER;

               BEGIN
                    NULL;
               END;                    -- PACKAGE OUTSIDE DECLARE.
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN OUTSIDE " &
                           "PACKAGE");
          END OUTSIDE;
     BEGIN
         NULL;
     END;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED("UNEXPECTED EXCEPTION RAISED");
          RESULT;
END CB4007A;
