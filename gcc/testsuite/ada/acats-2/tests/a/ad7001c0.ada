-- AD7001C0M.ADA

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
--     CHECK THAT A DECLARATION IN PACKAGE SYSTEM IS ACCESSIBLE
--     IN A LIBRARY PACKAGE BODY IF A WITH CLAUSE NAMING SYSTEM
--     IS PROVIDED FOR THE PACKAGE SPECIFICATION, ALTHOUGH IN A
--     SEPARATE FILE.

-- HISTORY:
--     JET 09/09/87  CREATED ORIGINAL TEST.
--     RJW 05/03/88  REVISED AND ENTERED TEST INTO ACVC.
--     PWN 05/25/94  ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.

-- THIS FILE CONTAINS PACKAGE SPEC AD7001C_PACKAGE AND THE MAIN
--     PROCEDURE FOR TEST AD7001C.  FILE AD7001C1.ADA CONTAINS
--     THE PACKAGE BODY FOR THE PACKAGE SPEC AND IS ALSO REQUIRED
--     FOR TEST EXECUTION.

WITH SYSTEM;

PACKAGE AD7001C_PACKAGE IS

     I : INTEGER;
     F : FLOAT;

  PROCEDURE REQUIRE_BODY;

END AD7001C_PACKAGE;


WITH AD7001C_PACKAGE; USE AD7001C_PACKAGE;
WITH REPORT;  USE REPORT;

PROCEDURE AD7001C0M IS

BEGIN
     TEST ("AD7001C", "CHECK THAT A DECLARATION IN PACKAGE SYSTEM " &
                      "IS ACCESSIBLE IN A LIBRARY PACKAGE BODY IF " &
                      "A WITH CLAUSE NAMING SYSTEM IS PROVIDED FOR " &
                      "THE PACKAGE SPECIFICATION, ALTHOUGH IN A " &
                      "SEPARATE FILE");
     RESULT;
END AD7001C0M;
