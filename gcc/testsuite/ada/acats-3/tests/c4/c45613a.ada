-- C45613A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED
-- BY "**" FOR INTEGERS WHEN THE RESULT EXCEEDS THE RANGE 
-- OF THE BASE TYPE.


-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- H. TILTON 10/06/86
--      MRM 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE C45613A IS
 
BEGIN
     TEST ("C45613A","CHECK THAT CONSTRAINT_ERROR " &
                     "IS RAISED BY ""**"" FOR INTEGERS WHEN THE " &
                     "RESULT EXCEEDS THE RANGE OF THE BASE TYPE");

     DECLARE
          INT : INTEGER;
     BEGIN
          INT := IDENT_INT(INTEGER'LAST ** IDENT_INT(2));
          FAILED ("NO EXCEPTION FOR SECOND POWER OF INTEGER'LAST");
             
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS => 
                    FAILED ("WRONG EXCEPTION RAISED FOR " &
                            "SECOND POWER OF " &
                            "INTEGER'LAST");
     END;

     DECLARE
          INT : INTEGER;
     BEGIN
          INT := IDENT_INT(INTEGER'FIRST ** IDENT_INT(3));
          FAILED ("NO EXCEPTION FOR THIRD POWER OF INTEGER'FIRST");
               
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR " &
                            "THIRD POWER OF " &
                            "INTEGER'FIRST");
            
     END;

     RESULT;
   
END C45613A;
