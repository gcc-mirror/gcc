-- AE3709A.ADA

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
-- CHECK THE NAMES OF THE FORMAL PARAMETERS.

-- JBG 3/30/83

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE AE3709A IS

     PACKAGE INT IS NEW INTEGER_IO(INTEGER);
     USE INT;
     FILE : FILE_TYPE;
     STR  : STRING(1..3);
     LAST : POSITIVE;
     ITEM : INTEGER;

BEGIN

     TEST ("AE3709A", "CHECK NAMES OF FORMAL PARAMETERS");

     IF EQUAL(2, 3) THEN
          GET (FILE => FILE, ITEM => ITEM, WIDTH => 0);
          GET (ITEM => ITEM, WIDTH => 0);
          PUT (FILE => FILE, ITEM => ITEM, WIDTH => 4, BASE => 4);
          PUT (ITEM => ITEM, WIDTH => 4, BASE => 4);
          GET (FROM => STR, ITEM => ITEM, LAST => LAST);
          PUT (TO   => STR, ITEM => ITEM, BASE => 4);
     END IF;

     RESULT;

END AE3709A;
