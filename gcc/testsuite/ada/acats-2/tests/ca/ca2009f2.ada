-- CA2009F2.ADA

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
-- SEPARATE GENERIC FUNCTION BODY.
-- SPECIFICATION, BODY STUB, AND AN INSTANTIATION ARE
-- IN CA2009F0M.DEP.

-- APPLICABILITY CRITERIA:
--     THIS UNIT MUST BE ACCEPTED BY ALL ADA 95 IMPLEMENTATIONS.

-- HISTORY:
--     BHS 08/01/84  CREATED ORIGINAL TEST.
--     PWB 02/19/86  MODIFIED COMMENTS TO DESCRIBE RELATION TO OTHER
--                   FILES AND POSSIBLE NON-APPLICABILITY.
--     BCB 01/05/88  MODIFIED HEADER.
--     EDS 08/04/98  REMOVE CONTROL Z AT END OF FILE.
--     RLB 09/13/99  UPDATED APPLICABILITY CRITERIA FOR ADA 95.

SEPARATE (CA2009F0M)
FUNCTION FUNC1 RETURN OBJ IS
BEGIN
     FVAR1 := FCON1;
     RETURN FVAR1;
END FUNC1;
