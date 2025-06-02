-- C48006B.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T'(X)" ALLOCATES A NEW
-- OBJECT EACH TIME IT IS EXECUTED AND THAT IF T IS A RECORD, ARRAY, OR
-- PRIVATE TYPE (CONSTRAINED OR UNCONSTRAINED), THE ALLOCATED OBJECT HAS
-- THE VALUE OF (X).

-- RM  01/14/80
-- RM  01/O1/82
-- SPS 10/27/82
-- EG  07/05/84
-- JBG 11/08/85 AVOID CONFLICT WITH AI-7 OR AI-275

WITH REPORT;

PROCEDURE C48006B IS

     USE REPORT ;

BEGIN

     TEST("C48006B","CHECK THAT THE FORM 'NEW T'(X)' " &
                    "ALLOCATES A NEW OBJECT " &
                    "AND THAT IF T IS A RECORD, ARRAY, OR PRIVATE "    &
                    "TYPE, THE ALLOCATED OBJECT HAS THE VALUE (X)");

     -- RECORD OR ARRAY TYPE (CONSTRAINED OR UNCONSTRAINED)

     DECLARE

          TYPE  TB0(  A , B : INTEGER )  IS
               RECORD
                    C : INTEGER := 7 ;
               END RECORD;
          SUBTYPE  TB  IS  TB0( 2 , 3 );
          TYPE ATB  IS  ACCESS TB  ;
          TYPE ATB0 IS  ACCESS TB0 ;
          VB1  ,  VB2  : ATB  ;
          VB01 , VB02  : ATB0 ;

          TYPE  ARR0  IS  ARRAY( INTEGER RANGE <> ) OF INTEGER ;
          SUBTYPE  ARR  IS ARR0( 1..4 );
          TYPE  A_ARR   IS  ACCESS ARR  ;
          TYPE  A_ARR0  IS  ACCESS ARR0 ;
          VARR1  , VARR2  : A_ARR  ;
          VARR01 , VARR02 : A_ARR0 ;

     BEGIN

          VB1  :=  NEW TB'( 2 , 3 , 5 );
          IF ( VB1.A /=IDENT_INT( 2)  OR
               VB1.B /=IDENT_INT( 3)  OR
               VB1.C /=IDENT_INT( 5) )
          THEN FAILED( "WRONG VALUES  -  B1 1" );
          END IF;

          VB2  :=  NEW TB'( IDENT_INT(2), IDENT_INT(3), IDENT_INT(6));
          IF ( VB2.A /= 2  OR
               VB2.B /= 3  OR
               VB2.C /= 6  OR
               VB1.A /= 2  OR
               VB1.B /= 3  OR
               VB1.C /= 5 )
          THEN FAILED( "WRONG VALUES  -  B1 2" );
          END IF;

          VB01  :=  NEW TB0'( 1 , 2 , 3 );
          IF ( VB01.A /=IDENT_INT( 1)  OR
               VB01.B /=IDENT_INT( 2)  OR
               VB01.C /=IDENT_INT( 3) )
          THEN FAILED( "WRONG VALUES  -  B2 1" );
          END IF;

          VB02  :=  NEW TB0'( IDENT_INT(4) , IDENT_INT(5) ,
                                                      IDENT_INT(6) );
          IF ( VB02.A /=IDENT_INT( 4)  OR
               VB02.B /=IDENT_INT( 5)  OR
               VB02.C /=IDENT_INT( 6)  OR
               VB01.A /=IDENT_INT( 1)  OR
               VB01.B /=IDENT_INT( 2)  OR
               VB01.C /=IDENT_INT( 3) )
          THEN FAILED( "WRONG VALUES  -  B2 2" );
          END IF;

          VARR1 := NEW ARR'( 5 , 6 , 7 , 8 );
          IF  ( VARR1(1) /=IDENT_INT( 5)  OR
                VARR1(2) /=IDENT_INT( 6)  OR
                VARR1(3) /=IDENT_INT( 7)  OR
                VARR1(4) /=IDENT_INT( 8) )
          THEN FAILED( "WRONG VALUES  -  B3 1" );
          END IF ;

          VARR2 := NEW ARR'( IDENT_INT(1) , IDENT_INT(2) , IDENT_INT(3),
                                                         IDENT_INT(4) );
          IF  ( VARR2(1) /= 1  OR
                VARR2(2) /= 2  OR
                VARR2(3) /= 3  OR
                VARR2(4) /= 4  OR
                VARR1(1) /= 5  OR
                VARR1(2) /= 6  OR
                VARR1(3) /= 7  OR
                VARR1(4) /= 8 )
          THEN FAILED( "WRONG VALUES  -  B3 2" );
          END IF ;

          VARR01 := NEW ARR0'( 11 , 12 , 13 );
          IF  ( VARR01(INTEGER'FIRST) /= IDENT_INT(11)  OR
                VARR01(INTEGER'FIRST + 1) /= IDENT_INT(12)  OR
                VARR01(INTEGER'FIRST + 2) /= IDENT_INT(13) )
          THEN FAILED( "WRONG VALUES -  B4 1" );
          END IF ;
          IF  ( VARR01.ALL'FIRST /= IDENT_INT( INTEGER'FIRST )  OR
                VARR01.ALL'LAST  /= IDENT_INT( INTEGER'FIRST + 2 ) )
          THEN FAILED( "WRONG VALUES -  B4 2" );
          END IF ;

          VARR02 := NEW ARR0'( 1 => IDENT_INT(14) , 2 => IDENT_INT(15));
          IF  ( VARR02(1) /= 14  OR
                VARR02(2) /= 15  OR
                VARR01(INTEGER'FIRST) /= 11  OR
                VARR01(INTEGER'FIRST + 1) /= 12  OR
                VARR01(INTEGER'FIRST + 2) /= 13 )
          THEN FAILED( "WRONG VALUES -  B4 3" );
          END IF ;

     END ;

     -- PRIVATE TYPE (CONSTRAINED OR UNCONSTRAINED)

     DECLARE

          PACKAGE P IS
               TYPE UP(A, B : INTEGER) IS PRIVATE;
--             SUBTYPE CP IS UP(1, 2);
--             TYPE A_CP IS ACCESS CP;
               TYPE A_UP IS ACCESS UP;
               CONS1_UP : CONSTANT UP;
               CONS2_UP : CONSTANT UP;
               CONS3_UP : CONSTANT UP;
               CONS4_UP : CONSTANT UP;
--             PROCEDURE CHECK1 (X : A_CP);
--             PROCEDURE CHECK2 (X, Y : A_CP);
               PROCEDURE CHECK3 (X : A_UP);
               PROCEDURE CHECK4 (X, Y : A_UP);
          PRIVATE
               TYPE UP(A, B : INTEGER) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;
               CONS1_UP : CONSTANT UP := (1, 2, 3);
               CONS2_UP : CONSTANT UP := (IDENT_INT(1), IDENT_INT(2),
                                          IDENT_INT(4));
               CONS3_UP : CONSTANT UP := (7, 8, 9);
               CONS4_UP : CONSTANT UP := (IDENT_INT(10), IDENT_INT(11),
                                          IDENT_INT(12));
          END P;

          USE P;

--        V_A_CP1, V_A_CP2 : A_CP;
          V_A_UP1, V_A_UP2 : A_UP;

          PACKAGE BODY P IS
--             PROCEDURE CHECK1 (X : A_CP) IS
--             BEGIN
--                  IF (X.A /= IDENT_INT(1) OR
--                      X.B /= IDENT_INT(2) OR
--                      X.C /= IDENT_INT(3)) THEN
--                       FAILED ("WRONG VALUES - CP1");
--                  END IF;
--             END CHECK1;
--             PROCEDURE CHECK2 (X, Y : A_CP) IS
--             BEGIN
--                  IF (X.A /= 1 OR X.B /= 2 OR X.C /= 3 OR
--                      Y.A /= 1 OR Y.B /= 2 OR Y.C /= 4) THEN
--                       FAILED ("WRONG VALUES - CP2");
--                  END IF;
--             END CHECK2;
               PROCEDURE CHECK3 (X : A_UP) IS
               BEGIN
                    IF (X.A /= IDENT_INT(7) OR
                        X.B /= IDENT_INT(8) OR
                        X.C /= IDENT_INT(9)) THEN
                         FAILED ("WRONG VALUES - UP1");
                    END IF;
               END CHECK3;
               PROCEDURE CHECK4 (X, Y : A_UP) IS
               BEGIN
                    IF (X.A /=  7 OR X.B /=  8 OR X.C /=  9 OR
                        Y.A /= 10 OR Y.B /= 11 OR Y.C /= 12) THEN
                         FAILED ("WRONG VALUES - UP2");
                    END IF;
               END CHECK4;
          END P;

     BEGIN

--        V_A_CP1 := NEW CP'(CONS1_UP);
--        CHECK1(V_A_CP1);

--        V_A_CP2 := NEW CP'(CONS2_UP);
--        CHECK2(V_A_CP1, V_A_CP2);

          V_A_UP1 := NEW P.UP'(CONS3_UP);
          CHECK3(V_A_UP1);

          V_A_UP2 := NEW P.UP'(CONS4_UP);
          CHECK4(V_A_UP1, V_A_UP2);

     END;

     RESULT;

END C48006B;
