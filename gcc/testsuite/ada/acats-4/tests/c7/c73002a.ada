-- C73002A.ADA

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
-- CHECK THAT THE STATEMENTS IN A PACKAGE BODY ARE EXECUTED AFTER THE
--    ELABORATION OF THE DECLARATIONS (IN SPEC AND IN BODY).


-- RM 05/15/81
-- JBG 9/21/83

WITH REPORT;
PROCEDURE  C73002A  IS

     USE  REPORT;

BEGIN

     TEST( "C73002A" , "CHECK: EXECUTION OF STATEMENTS IN A PACKAGE " &
                       "BODY FOLLOWS ELABORATION OF THE DECLARATIONS");

     DECLARE

          PACKAGE  P1  IS

               A : INTEGER := IDENT_INT(7);

               PACKAGE  P2  IS
                    B : INTEGER := IDENT_INT(11);
               END  P2;

          END  P1;


          PACKAGE BODY  P1  IS               --   A  AA   B  BB

               AA : INTEGER := IDENT_INT(7); --   7   7  11 (11)

               PACKAGE BODY  P2  IS
                    BB : INTEGER := IDENT_INT(11);--  7  11  11
               BEGIN

                    B  := 2*B ;             --   7   7  22  11
                    BB := 2*BB;             --   7   7  22  22
                    A  := 5*A ;             --  35   7  22  22
                    AA := 2*AA;             --  35  14  22  22

                    IF  BB /=  22  OR
                        AA /=  14  OR
                        A  /=  35  OR
                        B  /=  22
                    THEN
                         FAILED( "ASSIGNED VALUES INCORRECT  -  1" );
                    END IF;

               END  P2;

          BEGIN

               A  :=  A  + 20;              --  55  14  22  22
               AA :=  AA + 20;              --  55  34  22  22

               IF  AA /=  34  OR
                   A  /=  55  OR
                   P2.B /=  22
               THEN
                    FAILED( "ASSIGNED VALUES INCORRECT  -  2" );
               END IF;

          END  P1;


          USE  P1;
          USE  P2;

     BEGIN

          IF  A  /=  55  OR
              B  /=  22
          THEN
               FAILED( "ASSIGNED VALUES INCORRECT  -  3" );
          END IF;

     END;


     RESULT;


END C73002A;
