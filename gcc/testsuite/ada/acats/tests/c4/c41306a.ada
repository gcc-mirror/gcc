-- C41306A.ADA

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
-- CHECK THAT IF  F  IS A FUNCTION RETURNING A TASK OF A TYPE HAVING
--     AN ENTRY  E ,  AN ENTRY CALL OF THE FORM
--
--                           F.E
--
--     IS PERMITTED.


-- RM  2/2/82
-- ABW 7/16/82

WITH REPORT;
USE REPORT;
PROCEDURE C41306A IS


BEGIN

     TEST ( "C41306A" , "CHECK THAT IF  F  IS A FUNCTION RETURNING" &
                        " A TASK OF A TYPE HAVING AN ENTRY  E ,  AN" &
                        " ENTRY CALL OF THE FORM  F.E  IS PERMITTED");


     -------------------------------------------------------------------

     DECLARE

          X  : INTEGER  :=  0 ;

          TASK TYPE  T  IS
               ENTRY  E ;
          END  T ;

          T1 : T ;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  E  DO
                    X := IDENT_INT(17) ;
               END  E ;
               ACCEPT  E  DO
                    X := IDENT_INT(16) ;
               END  E ;
          END  T ;

          FUNCTION  F1  RETURN  T  IS
          BEGIN
               RETURN  T1 ;
          END  F1 ;

          FUNCTION F2 (A,B : BOOLEAN) RETURN T IS
          BEGIN
               IF A AND B THEN NULL; END IF;
               RETURN T1;
          END F2;

     BEGIN

          F1.E ;                                  -- X SET TO 17.

          IF  X /= 17 THEN
               FAILED("WRONG VALUE FOR GLOBAL VARIABLE - 1");
          END IF;

          X := 0;
          F2(TRUE,TRUE).E;                        -- X SET TO 16.
                            -- X TO BE SET TO 16.

          IF X /= 16 THEN
               FAILED("WRONG VALUE FOR GLOBAL VARIABLE - 2");
          END IF;

     END ;

     -------------------------------------------------------------------

     RESULT;


END C41306A;
