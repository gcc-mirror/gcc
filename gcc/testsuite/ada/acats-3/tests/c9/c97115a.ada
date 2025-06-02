-- C97115A.ADA

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
-- CHECK WHETHER AN ENTRY FAMILY INDEX EXPRESSION FOLLOWING AN OPEN
-- GUARD IS EVALUATED DIRECTLY AFTER THE GUARD, OR ONLY AFTER ALL GUARDS
-- HAVE BEEN EVALUATED, OR IN SOME MIXED ORDER SUCH THAT INDEX
-- EXPRESSIONS ARE EVALUATED AFTER THEIR GUARDS ARE DETERMINED TO BE
-- OPEN. 

-- RM 5/11/82
-- SPS 11/21/82
-- JBG 10/24/83
-- PWN 09/11/94  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C97115A IS


     -- THE TASK WILL HAVE LAST PRIORITY ( PRIORITY'FIRST )

     EVAL_ORDER  :  STRING (1..6)       := ( 1..6 => '*' );
     EVAL_ORD    :  STRING (1..6)       := ( 1..6 => '*' );
     INDEX       :  INTEGER             := 0; 


     FUNCTION F1 (X:INTEGER) RETURN INTEGER IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'F';    -- 123: FGH
          EVAL_ORD   (INDEX) := 'G';    -- 123: GGG ( 'G' FOR 'GUARD' )
          RETURN ( IDENT_INT(7) ); 
     END F1; 


     FUNCTION F2 (X:INTEGER) RETURN INTEGER IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'G'; 
          EVAL_ORD   (INDEX) := 'G'; 
          RETURN ( IDENT_INT(7) ); 
     END F2; 


     FUNCTION F3 (X:INTEGER) RETURN INTEGER IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'H'; 
          EVAL_ORD   (INDEX) := 'G'; 
          RETURN ( IDENT_INT(7) ); 
     END F3; 


     FUNCTION  I1 ( X:INTEGER )  RETURN BOOLEAN  IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'A';  -- 123: ABC
          EVAL_ORD   (INDEX) := 'I';  -- 123: III ( 'I' FOR 'INDEX' )
          RETURN ( IDENT_BOOL(TRUE) );    -- (THAT'S ENTRY-FAMILY INDEX)
     END I1; 


     FUNCTION  I2 ( X:INTEGER )  RETURN BOOLEAN  IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'B'; 
          EVAL_ORD   (INDEX) := 'I'; 
          RETURN ( IDENT_BOOL(TRUE) );
     END I2; 


     FUNCTION  I3 ( X:INTEGER )  RETURN BOOLEAN  IS
     BEGIN
          INDEX := INDEX + 1; 
          EVAL_ORDER (INDEX) := 'C'; 
          EVAL_ORD   (INDEX) := 'I'; 
          RETURN ( IDENT_BOOL(TRUE) );
     END I3; 

     FUNCTION POS_OF (FUNC : CHARACTER) RETURN INTEGER  IS
     BEGIN
          FOR I IN EVAL_ORDER'RANGE LOOP
               IF EVAL_ORDER(I) = FUNC THEN
                    RETURN I;
               END IF;
          END LOOP;
          FAILED ("DID NOT FIND LETTER " & FUNC);
          RETURN 0;
     END POS_OF;


BEGIN


     TEST ("C97115A", "CHECK THAT THE INDEX EXPRESSIONS ARE" &
                      " EVALUATED AFTER THE GUARDS BUT"      &
                      " BEFORE THE RENDEZVOUS IS ATTEMPTED"  );


     DECLARE
 

          TASK T IS


               ENTRY E ( BOOLEAN );
               ENTRY E1; 

          END T; 


          TASK BODY T IS
          BEGIN


               WHILE  E1'COUNT = 0  -- IF  E1  NOT YET CALLED, THEN GIVE
               LOOP                 --     THE MAIN TASK AN OPPORTUNITY
                    DELAY  10.01 * Impdef.One_Second;   --     TO ISSUE THE CALL.
               END LOOP;


               SELECT               

                         ACCEPT  E1; 

               OR

                    WHEN  6 + F1(7) = 13  =>
                         ACCEPT  E ( I1(17) );

               OR

                    WHEN  6 + F2(7) = 13  =>
                         ACCEPT  E ( I2(17) );

               OR

                    WHEN  6 + F3(7) = 13  =>
                         ACCEPT  E ( I3(17) );

               END SELECT;


          END T; 


     BEGIN

          T.E1; 

     END; -- END OF BLOCK CONTAINING THE ENTRY CALLS


     COMMENT ("GUARD AND INDEX FUNCTIONS WERE CALLED IN ORDER " & 
               EVAL_ORDER);
     COMMENT ("GUARD AND INDEX EXPRESSIONS WERE EVALUATED IN THE " &
              "ORDER " & EVAL_ORD);

     IF POS_OF ('F') > POS_OF ('A') OR
        POS_OF ('G') > POS_OF ('B') OR
        POS_OF ('H') > POS_OF ('C') THEN
          FAILED ("AN INDEX EXPRESSION WAS EVALUATED TOO EARLY");
     END IF;

     RESULT;

END  C97115A;  
