-- C9A003A.ADA

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
-- CHECK THAT ABORTING A TERMINATED TASK DOES NOT CAUSE EXCEPTIONS.


-- RM 5/21/82
-- SPS 11/21/82
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE  C9A003A  IS

     -- THE TASK WILL HAVE HIGHER PRIORITY ( PRIORITY'LAST )

BEGIN


     -------------------------------------------------------------------


     TEST ("C9A003A", "CHECK THAT  ABORTING A TERMINATED TASK" &
                      "  DOES NOT CAUSE EXCEPTIONS"   );

  
     DECLARE


          TASK TYPE  T_TYPE  IS


               ENTRY  E ;

          END  T_TYPE ;


          T_OBJECT1 : T_TYPE ;


          TASK BODY  T_TYPE  IS
               BUSY : BOOLEAN := FALSE ;
          BEGIN

               NULL;

          END  T_TYPE ;


     BEGIN


          IF NOT  T_OBJECT1'TERMINATED  THEN
               DELAY  20.0 * Impdef.One_Second;
          END IF;

          IF NOT  T_OBJECT1'TERMINATED  THEN
               COMMENT( "TASK NOT YET TERMINATED (AFTER 20 S.)" );
          END IF;


          BEGIN
               ABORT T_OBJECT1 ;
          EXCEPTION

               WHEN  OTHERS  =>
                    FAILED(  "EXCEPTION RAISED (WHEN ABORTING A" &
                             "  TERMINATED TASK)"  );

          END ;


     END ;


     -------------------------------------------------------------------



     RESULT;


END  C9A003A ;
