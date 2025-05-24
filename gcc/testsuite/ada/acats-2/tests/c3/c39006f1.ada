-- C39006F1.ADA

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
--     CHECK THAT NO PROGRAM_ERROR IS RAISED IF A SUBPROGRAM'S BODY HAS
--     BEEN ELABORATED BEFORE IT IS CALLED.  CHECK THE FOLLOWING:
--        B) FOR A SUBPROGRAM LIBRARY UNIT USED IN ANOTHER UNIT, NO
--           PROGRAM_ERROR IS RAISED IF PRAGMA ELABORATE NAMES THE
--           SUBPROGRAM.

--     THIS LIBRARY PACKAGE SPECIFICATION IS USED BY C39006F3M.ADA.

-- HISTORY:
--     TBN  08/22/86  CREATED ORIGINAL TEST.
--     BCB  03/29/90  CORRECTED HEADER.  CHANGED TEST NAME IN CALL
--                    TO 'TEST'.
--     PWN  05/25/94  ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.

PACKAGE C39006F1 IS
     PROCEDURE REQUIRE_BODY;
END C39006F1;
