-- CA5004B1.ADA

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
-- OBJECTIVE: See CA5004B2M.ADA
--
-- SPECIAL INSTRUCTIONS:  See CA5004B2M.ADA
--
-- TEST FILES:
--        CA5004B0.ADA
--     => CA5004B1.ADA
--        CA5004B2M.ADA

-- PWN 05/31/96 Split test into files without duplicate unit names.
-- RLB 03/11/99 Split test into files so that units that will be replaced
--              and units that won't are not in the same source file.

------------------------------------------------------------------

PACKAGE CA5004B0 IS

     I : INTEGER := 1;

     FUNCTION F RETURN BOOLEAN;

END CA5004B0;


PACKAGE BODY CA5004B0 IS

     FUNCTION F RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END F;

END CA5004B0;
