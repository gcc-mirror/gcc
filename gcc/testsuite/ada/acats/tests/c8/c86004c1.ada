-- C86004C1.ADA

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
--     SUBUNIT FOR THE C86004C01 PARENT.

-- HISTORY:
--     DHH 09/14/88 CREATED ORIGINAL TEST.

SEPARATE (C86004C01)
PROCEDURE C86004C1 IS
BEGIN
     TEST("C86004C", "CHECK THAT IF THE SPECIFICATION OF A " &
                     "SUBPROGRAM HAS A ""WITH"" CLAUSE FOR A GENERIC " &
                     "SUBPROGRAM INSTANTIANTION M, THEN IN THE " &
                     "FORMAL PART AND IN THE BODY (A SUBUNIT IN " &
                     "ANOTHER FILE), ""STANDARD.M"" IS " &
                     "A LEGAL NAME FOR THE SUBPROGRAM M");

     IF B /= STANDARD.C86004C0(0) THEN
          FAILED("STANDARD.SUBPROGRAM - B");
     END IF;

     IF A /= STANDARD.C86004C0(10) THEN
          FAILED("STANDARD.SUBPROGRAM - A");
     END IF;

     RESULT;
END C86004C1;
