-- CA1012A0.ADA

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
-- GENERIC PROCEDURE DECLARATION.
-- BODY IS IN CA1012A1.DEP.
-- INSTANTIATION IS IN CA1012A4M.DEP.

-- APPLICABILITY CRITERIA:
--     THIS UNIT MUST BE ACCEPTED BY ALL ADA 95 IMPLEMENTATIONS.

-- HISTORY:
--     WKB 07/20/81  CREATED ORIGINAL TEST.
--     PWB 02/19/86  ADDED COMMENTS TO DESCRIBE RELATION TO OTHER FILES
--                   AND CLARIFY POSSIBLE NON-APPLICABILITY.
--     BCB 01/05/88  MODIFIED HEADER.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.

GENERIC
     TYPE INDEX IS RANGE <>;
PROCEDURE CA1012A0 (I : IN OUT INDEX);
