-- C95065A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED WHEN AN ENTRY IS DECLARED
-- IF THE VALUE OF THE DEFAULT EXPRESSION FOR THE FORMAL PARAMETER DOES
-- NOT SATISFY THE CONSTRAINTS OF THE TYPE MARK, BUT IS RAISED WHEN THE
-- ENTRY IS CALLED AND THE DEFAULT VALUE IS USED.

-- CASE (A) ARRAY PARAMETERS CONSTRAINED WITH NONSTATIC BOUNDS AND
--          INITIALIZED WITH A STATIC AGGREGATE.

-- JWC 6/19/85

WITH REPORT; USE REPORT;
PROCEDURE C95065A IS

BEGIN

     TEST ("C95065A", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED IF " &
                      "AN INITIALIZATION VALUE DOES NOT SATISFY " &
                      "CONSTRAINTS ON A FORMAL PARAMETER WHEN THE " &
                      "FORMAL PART IS ELABORATED");

     BEGIN

          DECLARE

               TYPE A1 IS ARRAY (1 .. IDENT_INT(1), 1 .. IDENT_INT(10))
                                OF INTEGER;

               TASK T IS
                    ENTRY E1 (A : A1 := ((1, 0), (0, 1)));
               END T;

               TASK BODY T IS
               BEGIN
                    SELECT
                         ACCEPT E1 (A : A1 := ((1, 0), (0, 1))) DO
                              FAILED ("ACCEPT E1 EXECUTED");
                         END E1;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN TASK T");
               END T;

          BEGIN
               T.E1;
               FAILED ("CONSTRAINT ERROR NOT RAISED ON CALL TO T.E1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - E1");
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED (BY ENTRY DECL)");
          WHEN TASKING_ERROR =>
               FAILED ("TASKING_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED");
     END;

     RESULT;

END C95065A;
