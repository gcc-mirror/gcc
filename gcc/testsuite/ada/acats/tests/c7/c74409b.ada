-- C74409B.ADA

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
-- CHECK THAT IF A COMPOSITE TYPE IS DECLARED IN THE SAME PACKAGE 
-- AS A LIMITED PRIVATE TYPE AND HAS A COMPONENT OF THAT TYPE, 
-- THE COMPOSITE TYPE IS TREATED AS A LIMITED TYPE UNTIL THE 
-- EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE DECLARATION 
-- OF THE COMPOSITE TYPE AND AFTER THE FULL DECLARATION OF THE 
-- LIMITED PRIVATE TYPE

-- DSJ 5/5/83
-- JBG 9/23/83

WITH REPORT;
PROCEDURE C74409B IS

     USE REPORT;

BEGIN

     TEST("C74409B", "CHECK THAT A COMPOSITE TYPE WITH A LIMITED " &
                     "PRIVATE COMPONENT IS TREATED AS A LIMITED " &
                     "TYPE UNTIL ASSIGNMENT AND EQUALITY ARE BOTH " &
                     "AVAILABLE FOR THE COMPOSITE TYPE");

     DECLARE

          PACKAGE P IS
               TYPE LP IS LIMITED PRIVATE;
               PACKAGE Q IS
                    TYPE LP_ARRAY IS ARRAY (1 .. 2) OF LP;
               END Q;
          PRIVATE
               TYPE LP IS NEW INTEGER;
          END P;

          PACKAGE BODY P IS
               USE Q;
               FUNCTION "=" (L,R : LP_ARRAY) RETURN BOOLEAN IS  -- LEGAL
               BEGIN
                    RETURN TRUE;
               END;

               GENERIC
                    TYPE T IS PRIVATE;     -- NOTE: NOT LIMITED PRIVATE
                    C, D : T;
               PACKAGE A IS
                    -- IRRELEVANT DETAILS
               END A;

               PACKAGE BODY A IS
               BEGIN
                    IF C = D THEN
                         FAILED ("USED WRONG EQUALITY OPERATOR");
                    END IF;
               END A;

               PACKAGE BODY Q IS
                    PACKAGE ANOTHER_NEW_A IS 
                         NEW A (LP_ARRAY, (2,3), (4,5)); -- LEGAL
               END Q;
          END P;

     BEGIN

          NULL;

     END;

     RESULT;

END C74409B;
