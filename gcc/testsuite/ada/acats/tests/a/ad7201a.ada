-- AD7201A.ADA

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
--     CHECK THAT THE PREFIX OF THE 'ADDRESS ATTRIBUTE CAN DENOTE A
--     PACKAGE, SUBPROGRAM, TASK TYPE, SINGLE TASK, AND LABEL.

-- HISTORY:
--     DHH 09/01/88  CREATED ORIGINAL TEST.
--     RJW 02/23/90  REMOVED TESTS FOR THE 'ADDRESS ATTRIBUTE APPLIED TO
--                   A GENERIC UNIT.  REMOVED DECLARATION OF TYPE
--                   "COLOR".
--     DTN 11/22/91  DELETED SUBPART (A).

WITH SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE AD7201A IS

     SUBTYPE MY_ADDRESS IS SYSTEM.ADDRESS;

BEGIN
     TEST ("AD7201A", "CHECK THAT THE PREFIX OF THE 'ADDRESS " &
                      "ATTRIBUTE CAN DENOTE A PACKAGE, " &
                      "SUBPROGRAM, TASK TYPE, SINGLE TASK, AND LABEL");

     DECLARE
          PACKAGE B IS
          END B;
          B1 : BOOLEAN := (B'ADDRESS IN MY_ADDRESS);

          PROCEDURE C;
          C1 : BOOLEAN := (C'ADDRESS IN MY_ADDRESS);

          FUNCTION D RETURN BOOLEAN;
          D1 : BOOLEAN := (D'ADDRESS IN MY_ADDRESS);

          TASK E IS
          END E;
          E1 : BOOLEAN := (E'ADDRESS IN MY_ADDRESS);

          TASK TYPE F IS
          END F;
          F1 : BOOLEAN := (F'ADDRESS IN MY_ADDRESS);

          G1 : BOOLEAN;

          PACKAGE BODY B IS
          BEGIN
               NULL;
          END B;

          PROCEDURE C IS
          BEGIN
               NULL;
          END C;

          FUNCTION D RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END D;

          TASK BODY E IS
          BEGIN
               NULL;
          END E;

          TASK BODY F IS
          BEGIN
               NULL;
          END F;

     BEGIN
<<G>>     G1 := (G'ADDRESS IN MY_ADDRESS);
     END;

     RESULT;
END AD7201A;
