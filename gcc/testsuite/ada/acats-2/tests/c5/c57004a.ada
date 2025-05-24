-- C57004A.ADA

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
-- CHECK THAT AN EXIT STATEMENT WITH A LOOP NAME TERMINATES EXECUTION
--    OF THE LOOP STATEMENT WHOSE NAME IT MENTIONS, AND OF ALL OTHER
--    LOOP STATEMENTS (IF ANY) INTERIOR TO THE FIRST LOOP AND ENCLOSING
--    THE EXIT STATEMENT.

-- CASE 1 :  UNCONDITIONAL EXITS.


-- RM 04/24/81
-- SPS 3/7/83

WITH REPORT;
PROCEDURE  C57004A  IS

     USE  REPORT ;

BEGIN

     TEST( "C57004A" , "CHECK THAT A NAMING EXIT STATEMENT TERMINATES" &
                       " EXECUTION OF THE NAMED LOOP AND OF ALL LOOPS" &
                       " SITUATED IN-BETWEEN"                         );

     DECLARE

          COUNT : INTEGER     := 0   ;

     BEGIN

          OUTERMOST :
          FOR  X  IN  INTEGER RANGE 1..2  LOOP

               FOR  Y  IN  INTEGER RANGE 1..2  LOOP

                    COMMENT( "BEFORE 1" );

                    LOOP1 :
                    FOR  I  IN  1..10  LOOP
                         COMMENT( "INSIDE 1" );
                         EXIT  LOOP1 ;
                         FAILED( "EXIT NOT OBEYED (1)" );
                         FOR  J  IN  1..10  LOOP
                              FAILED( "OUTER EXIT NOT OBEYED (1)" );
                              EXIT ;
                              FAILED( "BOTH EXITS IGNORED (1)" );
                         END LOOP;
                    END LOOP  LOOP1 ;


                    COMMENT( "BEFORE 2" );
                    COUNT := COUNT + 1 ;

                    LOOP2 :
                    FOR  A  IN  1..1  LOOP
                         FOR  B  IN  1..1  LOOP

                              FOR  I  IN  CHARACTER  LOOP
                                   COMMENT( "INSIDE 2" );
                                   EXIT  LOOP2 ;
                                   FAILED( "EXIT NOT OBEYED (2)" );
                                   FOR  J  IN  BOOLEAN  LOOP
                                        FAILED( "OUTER EXIT NOT " &
                                                "OBEYED (2)");
                                        EXIT ;
                                        FAILED( "BOTH EXITS IGNORED " &
                                                "(2)");
                                   END LOOP;
                              END LOOP;

                         END LOOP;
                    END LOOP  LOOP2 ;


                    COMMENT( "BEFORE 3" );
                    COUNT := COUNT + 1 ;

                    LOOP3 :
                    FOR  A  IN  1..1  LOOP
                         FOR  B  IN  1..1  LOOP

                              FOR  I  IN  BOOLEAN  LOOP
                                   COMMENT( "INSIDE 3" );
                                   BEGIN
                                        EXIT  LOOP3 ;
                                        FAILED( "EXIT NOT OBEYED (3)" );
                                   END ;
                                   FAILED( "EXIT NOT OBEYED (3BIS)" );
                              END LOOP;

                         END LOOP;
                    END LOOP  LOOP3 ;


                    COMMENT( "BEFORE 4" );
                    COUNT := COUNT + 1 ;

                    LOOP4 :
                    FOR  A  IN  1..1  LOOP
                         FOR  B  IN  1..1  LOOP


                              FOR  I  IN  INTEGER RANGE 1..10  LOOP
                                   COMMENT( "INSIDE 4" );
                                   CASE  A  IS
                                        WHEN  1  =>
                                             EXIT  LOOP4 ;
                                             FAILED("EXIT NOT OBEYED " &
                                                    "(4)" );
                                   END CASE;
                                   FAILED( "EXIT NOT OBEYED (4BIS)" );
                              END LOOP;

                         END LOOP;
                    END LOOP  LOOP4 ;


                    COMMENT( "AFTER 4" );
                    COUNT := COUNT + 1 ;
                    EXIT  OUTERMOST ;

               END LOOP;

               FAILED( "MISSED FINAL EXIT" );

          END LOOP  OUTERMOST ;


          IF  COUNT /= 4  THEN
               FAILED( "WRONG FLOW OF CONTROL" );
          END IF;

     END ;

     RESULT ;


END  C57004A ;
