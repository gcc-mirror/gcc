-- C52102B.ADA

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
-- CHECK THAT THE ASSIGNMENT OF OVERLAPPING SOURCE AND TARGET VARIABLES
--    (INCLUDING ARRAYS AND SLICES IN VARIOUS COMBINATIONS) SATISFIES
--    THE SEMANTICS OF "COPY" ASSIGNMENT.  (THIS TEST IS IN TWO PARTS,
--    COVERING RESPECTIVELY STATIC AND DYNAMIC BOUNDS.)

-- PART 2:  DYNAMIC BOUNDS


-- RM 02/27/80
-- SPS 2/18/83
-- JBG 3/15/84
-- JBG 6/9/84

WITH REPORT;
PROCEDURE  C52102B  IS

     USE  REPORT; 
     IDENT_INT_0 : INTEGER := IDENT_INT(0);
     IDENT_INT_1 : INTEGER := IDENT_INT (1);
     IDENT_INT_2 : INTEGER := IDENT_INT (2);
     IDENT_INT_3 : INTEGER := IDENT_INT (3);
     IDENT_INT_4 : INTEGER := IDENT_INT (4);
     IDENT_INT_5 : INTEGER := IDENT_INT (5);
     IDENT_INT_6 : INTEGER := IDENT_INT (6);
     IDENT_INT_8 : INTEGER := IDENT_INT (8);
     IDENT_INT_9 : INTEGER := IDENT_INT (9);

BEGIN


     TEST( "C52102B" , "CHECK THAT THE ASSIGNMENT OF OVERLAPPING " &
                       "SOURCE AND TARGET VARIABLES (INCLUDING " &
                       "ARRAYS AND SLICES IN VARIOUS COMBINATIONS) " &
                       "SATISFIES THE SEMANTICS OF ""COPY"" " &
                       "ASSIGNMENT (PART 2: DYNAMIC BOUNDS)" );


     -------------------------------------------------------------------
     --------------------  ARRAYS OF INTEGERS  -------------------------

     DECLARE
          A   :   ARRAY( 1..IDENT_INT_4 ) OF INTEGER; 

     BEGIN
          A   :=   (  11  ,  12  ,  13  ,  14  );
          A   :=   (  1   , A(IDENT_INT_1) , A(IDENT_INT_2) ,
                                              A(IDENT_INT_1) );
          IF  A /= (  1   ,  11  ,  12  ,  11  )  THEN
               FAILED( "WRONG VALUES  -  I1" );
          END IF;

          A   :=   (  11  ,  12  ,  13  ,  14  );
          A   :=   ( A(IDENT_INT_4) , A(IDENT_INT_3) ,
                                       A(IDENT_INT_4) ,  1   );
          IF  A /= (  14  ,  13  ,  14  ,  1   )  THEN
               FAILED( "WRONG VALUES  -  I2" );
          END IF;

     END; 


     DECLARE
          A   :    ARRAY( -4..IDENT_INT_4 ) OF INTEGER; 

     BEGIN
          A         :=  (  -4 , -3 , -2 , -1 , 100 , 1 , 2 , 3 , 4 );
          A(-4..IDENT_INT_0)  :=  A(IDENT_INT_0..4); 
          IF    A   /=  ( 100 ,  1 ,  2 ,  3 ,  4  , 1 , 2 , 3 , 4 )
          THEN
               FAILED( "WRONG VALUES  -  I3" );
          END IF;

          A   :=  ( -4 , -3 , -2 , -1 , 100 ,  1 ,  2 ,  3 ,  4 );
          A(IDENT_INT_0..4)   :=  A(-4..IDENT_INT_0); 
          IF    A   /=  ( -4 , -3 , -2 , -1 , -4  , -3 , -2 , -1 , 100 )
          THEN
               FAILED( "WRONG VALUES  -  I4" );
          END IF;

     END; 


     DECLARE
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          A   :  ARR (1..10);

     BEGIN
          A   :=  ( 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 );
          A   :=  0  &  A(IDENT_INT_1..IDENT_INT_2)  &
                        A(IDENT_INT_1..IDENT_INT_2)  &
                         A(IDENT_INT_1..IDENT_INT_5); 
          IF   A  /=  ( 0 , 1 , 2 , 1 , 2 , 1 , 2 , 3 , 4 , 5  )
          THEN
               FAILED( "WRONG VALUES  -  I5" );
          END IF;

          A   :=  ( 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 );
          A   :=  A(IDENT_INT_6..IDENT_INT_9)  &
                  A(IDENT_INT_8..IDENT_INT_9)  &
                  A(IDENT_INT_8..IDENT_INT_9)  &  0  &  0; 
          IF   A  /=  ( 6 , 7 , 8 , 9 , 8 , 9 , 8 , 9 , 0 , 0  )
          THEN
               FAILED( "WRONG VALUES  -  I6" );
          END IF;

     END; 


     -------------------------------------------------------------------
     --------------------  ARRAYS OF BOOLEANS  -------------------------

     DECLARE
          A   :    ARRAY( 1..4 ) OF BOOLEAN; 

     BEGIN
          A   :=   (  FALSE , TRUE , TRUE , FALSE );
          A   :=   (  TRUE  , A(IDENT_INT_1) , A(IDENT_INT_2) ,
                                                A(IDENT_INT_1)  );
          IF  A /= (  TRUE , FALSE , TRUE , FALSE )
          THEN
               FAILED( "WRONG VALUES  -  B1" );
          END IF;

          A   :=   (  FALSE , TRUE , TRUE , FALSE );
          A   :=   (  A(IDENT_INT_4)  , A(IDENT_INT_3) ,
                                         A(IDENT_INT_4) ,  TRUE );
          IF  A /= (  FALSE , TRUE , FALSE,  TRUE )
          THEN
               FAILED( "WRONG VALUES  -  B2" );
          END IF;

     END; 


     DECLARE
          A   :    ARRAY( -IDENT_INT_4..4 ) OF BOOLEAN; 

     BEGIN
          A  := (FALSE,FALSE,FALSE,FALSE,FALSE,TRUE, TRUE, TRUE,TRUE);
          A(-IDENT_INT_4..IDENT_INT_0)  :=  A(IDENT_INT_0..4); 
          IF A /= (FALSE, TRUE, TRUE, TRUE, TRUE,TRUE, TRUE, TRUE,TRUE)
          THEN
               FAILED( "WRONG VALUES  -  B3" );
          END IF;

          A  := (FALSE,FALSE,FALSE,FALSE, TRUE,TRUE, TRUE, TRUE,TRUE);
          A(IDENT_INT_0..4)   :=  A(-4..IDENT_INT_0); 
          IF A /= (FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
          THEN
               FAILED( "WRONG VALUES  -  B4" );
          END IF;

     END; 


     DECLARE
          TYPE B_ARR IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
          A  :  B_ARR (1..10); 

     BEGIN
          A := (TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE);
          A :=  FALSE  &  A(IDENT_INT_1..IDENT_INT_2)  &
                          A(IDENT_INT_1..IDENT_INT_2)  &
                          A(IDENT_INT_1..IDENT_INT_5); 
          IF A/=(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)
          THEN
               FAILED( "WRONG VALUES  -  B5" );
          END IF;

          A := (TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE);
          A :=  A(IDENT_INT_6..IDENT_INT_9)  &
                A(IDENT_INT_8..IDENT_INT_9)  &
                A(IDENT_INT_8..IDENT_INT_9)  &      FALSE  &  TRUE; 
          IF A/=(FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE,FALSE,TRUE)
          THEN
               FAILED( "WRONG VALUES  -  B6" );
          END IF;

     END; 


     -------------------------------------------------------------------
     --------------------  CHARACTER STRINGS  --------------------------

     DECLARE
          A   :    STRING( 1..4 ); 

     BEGIN
          A   :=   "ARGH"; 
          A   :=   (  'Q' , A(IDENT_INT_1) , A(IDENT_INT_2) ,
                                              A(IDENT_INT_1) );
          IF  A /= "QARA"  THEN
               FAILED( "WRONG VALUES  -  C1" );
          END IF;

          A   :=   "ARGH"; 
          A   :=   ( A(IDENT_INT_4) , A(IDENT_INT_3) ,
                     A(IDENT_INT_4) , 'X' );
          IF  A /= "HGHX"  THEN
               FAILED( "WRONG VALUES  -  C2" );
          END IF;

     END; 


     DECLARE
          A   :    STRING( IDENT_INT(96)..104 );

     BEGIN
          A   :=  "APHRODITE"; 
          A(IDENT_INT(96)..IDENT_INT(100))  :=  A(IDENT_INT(100)..
                                                  IDENT_INT(104)); 
          IF  A  /=  "ODITEDITE"   THEN
               FAILED( "WRONG VALUES  -  C3" );
          END IF;

          A   :=  "APHRODITE"; 
          A(IDENT_INT(100)..IDENT_INT(104)) :=  A(IDENT_INT(96)..
                                                  IDENT_INT(100)) ; 
          IF  A  /=  "APHRAPHRO"   THEN
               FAILED( "WRONG VALUES  -  C4" );
          END IF;

     END; 


     DECLARE
          TYPE CH_ARR IS ARRAY (INTEGER RANGE <>) OF CHARACTER;
          A  :  CH_ARR (IDENT_INT_1..9); 

     BEGIN
          A   :=  "CAMBRIDGE"; 
          A   :=  'S'  &  A(IDENT_INT_1..IDENT_INT_2)  &
                          A(IDENT_INT_1..IDENT_INT_2)  &
                          A(IDENT_INT_1..IDENT_INT_4); 
          IF  A  /=  "SCACACAMB"  THEN
               FAILED( "WRONG VALUES  -  C5" );
          END IF;

          A   :=  "CAMBRIDGE"; 
          A   :=  A(IDENT_INT_8..IDENT_INT_8)  &
                  A(IDENT_INT_6..IDENT_INT_8)  &
                  A(IDENT_INT_6..IDENT_INT_8)  &  "EA"; 
          IF  A  /=  "GIDGIDGEA"  THEN
               FAILED( "WRONG VALUES  -  C6" );
          END IF;

     END; 


     RESULT; 


END C52102B; 
