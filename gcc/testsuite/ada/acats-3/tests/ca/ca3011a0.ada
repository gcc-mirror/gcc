-- CA3011A0.ADA

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
-- A GENERIC UNIT.
-- SUBUNITS ARE IN CA3011A1, CA3011A2, AND CA3011A3.
-- INSTANTIATION IS IN CA3011A4M.

-- APPLICABILITY CRITERIA:
--     THIS UNIT MUST BE ACCEPTED BY ALL ADA 95 IMPLEMENTATIONS.

-- HISTORY:
--     RJW 09/22/86  CREATED ORIGINAL TEST.
--     BCB 01/05/88  MODIFIED HEADER.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.

WITH REPORT; USE REPORT;

GENERIC
     TYPE T IS (<>);
     X : T;
PROCEDURE CA3011A0 (Z : OUT T);

PROCEDURE CA3011A0 (Z : OUT T) IS
     T1 : T;

     FUNCTION CA3011A1 RETURN T IS SEPARATE;

     PROCEDURE CA3011A2 (Y : OUT T) IS SEPARATE;

     PACKAGE CA3011A3 IS
          FUNCTION CA3011A3F RETURN T;
     END CA3011A3;

     PACKAGE BODY CA3011A3 IS SEPARATE;

BEGIN
     IF CA3011A1 /= X THEN
          FAILED ( "INCORRECT VALUE RETURNED BY FUNCTION CA3011A1" );
     END IF;

     CA3011A2 (T1);

     IF T1 /= X THEN
          FAILED ( "INCORRECT VALUE RETURNED BY PROCEDURE CA3011A2 " );
     END IF;

     IF CA3011A3.CA3011A3F /= X THEN
          FAILED ( "INCORRECT VALUE RETURNED BY FUNCTION CA3011A3F " );
     END IF;

     Z := X;

END CA3011A0;
