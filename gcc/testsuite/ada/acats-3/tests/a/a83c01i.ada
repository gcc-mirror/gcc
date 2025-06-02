-- A83C01I.ADA

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
-- CHECK THAT COMPONENT NAMES MAY BE THE SAME AS NAMES OF
--    LOOP PARAMETERS.

--    RM    24 JUNE 1980
--    JRK   10 NOV  1980
--    RM    01 JAN  1982


WITH REPORT;
PROCEDURE  A83C01I  IS

     USE REPORT;

BEGIN

     TEST( "A83C01I" , "CHECK THAT COMPONENT NAMES MAY BE THE SAME AS" &
                       " NAMES OF LOOP PARAMETERS" ) ;



     -- TEST FOR LOOP PARAMETERS


     DECLARE

          TYPE  R1A  IS
               RECORD
                    LOOP3 : INTEGER ;
               END RECORD ;

          TYPE  R1  IS
               RECORD
                    LOOP1 : INTEGER ;
                    LOOP2 : R1A ;
               END RECORD ;

          A1 : R1 := ( 3 , ( LOOP3 => 7 ) );

     BEGIN

          FOR  LOOP1  IN  0..1  LOOP

               FOR  LOOP2  IN  0..2  LOOP

                    FOR  LOOP3  IN  0..3  LOOP

                         A1.LOOP1 := A1.LOOP2.LOOP3 ;

                         DECLARE

                              TYPE  R1A  IS
                                   RECORD
                                        LOOP3 : INTEGER ;
                                        LOOP4 : INTEGER ;
                                   END RECORD ;

                              TYPE  R1  IS
                                   RECORD
                                        LOOP1 : INTEGER ;
                                        LOOP2 : R1A ;
                                   END RECORD ;

                              A1 : R1 := ( 3 , ( 6 , 7 ) );

                         BEGIN

                              FOR  LOOP4  IN  0..4  LOOP

                                   A1.LOOP1 := A1.LOOP2.LOOP3 +
                                               A1.LOOP2.LOOP4 ;

                              END LOOP ;

                         END ;

                    END LOOP ;

               END LOOP ;

          END LOOP ;

     END ;



     RESULT;

END A83C01I;
