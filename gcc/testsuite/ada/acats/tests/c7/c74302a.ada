-- C74302A.ADA

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
-- CHECK THAT MULTIPLE DECLARATIONS MAY BE USED FOR DEFERRED CONSTANT
-- DECLARATIONS, EVEN IF THE FULL DECLARATIONS ARE GIVEN INDIVIDUALLY.

-- CHECK THAT MULTIPLE DECLARATIONS MAY BE USED FOR THE FULL
-- DECLARATIONS, EVEN IF THE DEFERRED CONSTANT DECLARATIONS ARE GIVEN
-- INDIVIDUALLY.


-- DSJ  5/09/83
-- SPS 10/24/83
-- EG  12/19/83
-- JRK 12/20/83

-- DTN 11/19/91     DELETED SUBPART (C).

WITH REPORT;
PROCEDURE C74302A IS

     USE REPORT;

BEGIN

     TEST("C74302A", "CHECK THAT MULTIPLE DECLARATIONS MAY BE USED " &
                     "FOR DEFERRED CONSTANT DECLARATIONS");

     DECLARE

          PACKAGE PACK1 IS

               TYPE T IS PRIVATE;

               B, E : CONSTANT T;

               F : CONSTANT T;
          PRIVATE

               TYPE T IS NEW INTEGER;

               E : CONSTANT T := T(IDENT_INT(4));

               B, F : CONSTANT T := T(IDENT_INT(2));

          END PACK1;

          USE PACK1;

     BEGIN

          IF B/=F THEN
               FAILED("VALUES OF DEFERRED CONSTANTS B AND F NOT EQUAL");
          END IF;

     END;

     RESULT;

END C74302A;
