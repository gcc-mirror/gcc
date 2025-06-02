-- C41306B.ADA

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
-- CHECK THAT IF  F  IS A FUNCTION RETURNING AN ACCESS VALUE DESIGNATING
--     A TASK OF A TYPE HAVING
--     AN ENTRY  E ,  AN ENTRY CALL OF THE FORM
--
--                           F.ALL.E
--
--     IS PERMITTED.

-- RM  02/02/82
-- ABW 07/16/82
-- EG  05/28/85

WITH REPORT; USE REPORT;

PROCEDURE C41306B IS

BEGIN

     TEST ( "C41306B" , "CHECK THAT IF  F  IS A FUNCTION RETURNING" &
                        " AN ACCESS VALUE DESIGNATING"              &
                        " A TASK OF A TYPE HAVING AN ENTRY  E ,  AN" &
                        " ENTRY CALL OF THE FORM  F.ALL.E  IS"       &
                        " PERMITTED" );


     -------------------------------------------------------------------

     DECLARE

          X  : INTEGER  :=  0 ;

          TASK TYPE  T  IS
               ENTRY  E ;
          END  T ;

          TYPE  A_T  IS  ACCESS T ;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  E  DO
                    X := IDENT_INT(17) ;
               END  E ;
          END  T ;

          FUNCTION  F1  RETURN  A_T  IS
               A_T_VAR1 : A_T := NEW T ;
          BEGIN
               RETURN  A_T_VAR1 ;
          END  F1 ;

          FUNCTION F2 (A, B : BOOLEAN) RETURN A_T IS
               A_T_VAR2 : A_T := NEW T;
          BEGIN
               IF A AND B THEN
                    NULL;
               END IF;
               RETURN A_T_VAR2;
          END F2;

     BEGIN

          F1.ALL.E ;  --  THE ELABOR. OF  F1 (BODY)  ACTIVATES THE TASK,
                      --      WHICH  PROCEEDS TO WAIT FOR ENTRY  E  TO
                      --      BE CALLED.

                      --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

          IF  X /= 17
          THEN
               FAILED( "WRONG VALUE FOR GLOBAL VARIABLE   (1)" );
          END IF;

          X := 0;
          F2(TRUE, TRUE).ALL.E;    -- THE ELABORATION OF F2 (BODY)
                                   -- ACTIVATES THE TASK, WHICH 
                                   -- PROCEEDS TO WAIT FOR THE
                                   -- ENTRY E TO BE CALLED.

                                   -- THE CALLED ENTRY CAUSES X TO BE 
                                   -- SET TO 17.

          IF X /= 17 THEN
               FAILED ("WRONG VALUE FOR GLOBAL VARIABLE (2)");
          END IF;

     END ;

     -------------------------------------------------------------------

     DECLARE

          X  : INTEGER  :=  0 ;

          TASK TYPE  T  IS
               ENTRY  E ;
          END  T ;

          TYPE  A_T  IS  ACCESS T ;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  E  DO
                    X := IDENT_INT(17) ;
               END  E ;
          END  T ;

          FUNCTION  F3  RETURN  A_T  IS
          BEGIN
               RETURN  NEW T ;
          END  F3;

          FUNCTION F4 (C, D : BOOLEAN) RETURN A_T IS
          BEGIN
               IF C AND D THEN
                    NULL;
               END IF;
               RETURN NEW T;
          END F4;

     BEGIN

          F3.ALL.E ;  --  THE ELABOR. OF  F3 (BODY)  ACTIVATES THE TASK,
                      --      WHICH  PROCEEDS TO WAIT FOR ENTRY  E  TO
                      --      BE CALLED.

                      --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

          IF  X /= 17
          THEN
               FAILED( "WRONG VALUE FOR GLOBAL VARIABLE   (3)" );
          END IF;

          X := 0;
          F4(TRUE, TRUE).ALL.E;    -- THE ELABORATION OF F4 (BODY)
                                   -- ACTIVATES THE TASK, WHICH
                                   -- PROCEEDS TO WAIT FOR THE
                                   -- ENTRY E TO BE CALLED.

                                   -- THE CALLED ENTRY CAUSES X TO BE
                                   -- SET TO 17.

          IF X /= 17 THEN
               FAILED ("WRONG VALUE FOR GLOBAL VARIABLE (4)");
          END IF;

     END ;

     -------------------------------------------------------------------

     DECLARE

          X  : INTEGER  :=  0 ;

          TASK TYPE  T  IS
               ENTRY  E ;
          END  T ;

          TYPE  A_T  IS  ACCESS T ;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  E  DO
                    X := IDENT_INT(17) ;
               END  E ;
          END  T ;

     BEGIN

          DECLARE

               F3 : A_T := NEW T;

          BEGIN

               F3.ALL.E;

                      --  THE CALLED ENTRY CAUSES  X  TO BE SET TO  17 .

               IF  X /= 17 THEN
                    FAILED( "WRONG VALUE FOR GLOBAL VARIABLE   (5)" );
               END IF;

          END;

     END ;

     -------------------------------------------------------------------


     RESULT;


END C41306B;
