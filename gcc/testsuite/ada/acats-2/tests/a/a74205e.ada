-- A74205E.ADA

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
-- CHECK THAT THE ADDITIONAL OPERATIONS FOR A COMPOSITE TYPE WITH A
-- COMPONENT OF A PRIVATE TYPE ARE AVAILABLE AT THE EARLIEST
-- PLACE WITHIN THE IMMEDIATE SCOPE OF THE DECLARATION OF THE COMPOSITE
-- TYPE AND AFTER THE FULL DECLARATION OF THE PRIVATE TYPE.

-- IN PARTICULAR, CHECH FOR THE FOLLOWING :

-- (1)  RELATIONAL OPERATORS WITH ARRAYS OF SCALAR TYPES
-- (2)  EQUALITY WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES 
-- (3)  LOGICAL OPERATORS WITH ARRAYS OF BOOLEAN TYPES
-- (4)  CATENATION WITH ARRAYS OF LIMITED PRIVATE TYPES
-- (5)  INITIALIZATION WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES
-- (6)  ASSIGNMENT WITH ARRAYS AND RECORDS OF LIMITED PRIVATE TYPES
-- (7)  SELECTED COMPONENTS WITH COMPOSITES OF PRIVATE RECORD TYPES
-- (8)  INDEXED COMPONENTS WITH COMPOSITES OF PRIVATE ARRAY TYPES
-- (9)  SLICES WITH COMPOSITES OF PRIVATE ARRAY TYPES
-- (10) QUALIFICATION FOR COMPOSITES OF PRIVATE TYPES
-- (11) AGGREGATES FOR ARRAYS AND RECORDS OF PRIVATES TYPES
-- (12) USE OF 'SIZE FOR ARRAYS AND RECORDS OF PRIVATE TYPES

-- DSJ 5/2/83

WITH REPORT ;
PROCEDURE A74205E IS

     USE REPORT ;

BEGIN

     TEST("A74205E", "CHECK THAT ADDITIONAL OPERATIONS FOR "
                   & "COMPOSITE TYPES OF PRIVATE TYPES ARE "
                   & "AVAILABLE AT THE EARLIEST PLACE AFTER THE "
                   & "FULL DECLARATION AND IN THE IMMEDIATE "
                   & "SCOPE OF THE COMPOSITE TYPE") ;

     DECLARE

          PACKAGE PACK1 IS
               TYPE LP1 IS LIMITED PRIVATE ;
               PACKAGE PACK_LP IS
                    TYPE LP_ARR IS ARRAY (INTEGER RANGE <>) OF LP1 ;
                    SUBTYPE LP_ARR2 IS LP_ARR ( 1 .. 2 ) ;
                    SUBTYPE LP_ARR4 IS LP_ARR ( 1 .. 4 ) ;
               END PACK_LP ;

               TYPE T1 IS PRIVATE ;
               PACKAGE PACK2 IS
                    TYPE ARR IS ARRAY (INTEGER RANGE <>) OF T1 ;
                    SUBTYPE ARR2 IS ARR ( 1 .. 2 ) ;
                    SUBTYPE ARR4 IS ARR ( 1 .. 4 ) ;
               END PACK2 ;

               TYPE T2 IS PRIVATE ;
               TYPE T3 IS PRIVATE ;
               PACKAGE PACK3 IS
                    TYPE ARR_T2 IS ARRAY ( 1 .. 2 ) OF T2 ;
                    TYPE ARR_T3 IS ARRAY ( 1 .. 2 ) OF T3 ;
               END PACK3 ;
          PRIVATE
               TYPE LP1 IS NEW BOOLEAN ;
               TYPE T1 IS NEW BOOLEAN ;
               TYPE T2 IS ARRAY ( 1 .. 2 ) OF INTEGER ;
               TYPE T3 IS
                    RECORD
                         C1 : INTEGER ;
                    END RECORD ;
          END PACK1 ;

          PACKAGE BODY PACK1 IS

               PACKAGE BODY PACK_LP IS
                    L1, L2 : LP_ARR2 := (TRUE,FALSE) ; -- LEGAL
                    A3 : LP_ARR2 := L1 ;               -- LEGAL
                    B3 : BOOLEAN := L1 =  L2 ;         -- LEGAL
                    B4 : BOOLEAN := L1 /= L2 ;         -- LEGAL
               END PACK_LP ;

               PACKAGE BODY PACK2 IS
                    A1, A2 : ARR2 := (FALSE,TRUE) ;    -- LEGAL
                    A4 : ARR2    := ARR2'(A1) ;        -- LEGAL
                    B1 : BOOLEAN := A1 <  A2 ;         -- LEGAL
                    B2 : BOOLEAN := A1 >= A2 ;         -- LEGAL
                    N3 : INTEGER := A1'SIZE ;          -- LEGAL
                    PROCEDURE G1 (X : ARR2 := NOT A1) IS    -- LEGAL
                    BEGIN
                         NULL ; 
                    END G1 ;

                    PROCEDURE G2 (X : ARR2 := A1 AND A2) IS -- LEGAL
                    BEGIN
                         NULL ;
                    END G2 ;

                    PROCEDURE G3 (X : ARR4 := A1 & A2) IS   -- LEGAL
                    BEGIN
                         NULL ;
                    END G3 ;

                    PROCEDURE G4 (X : ARR2 := (FALSE,TRUE) ) IS -- LEGAL
                    BEGIN
                         NULL ;
                    END G4 ;
               END PACK2 ;

               PACKAGE BODY PACK3 IS
                    X2 : ARR_T2 :=
                         (1=>(1,2), 2=>(3,4)) ;        -- LEGAL
                    X3 : ARR_T3 :=
                         (1=>(C1=>5), 2=>(C1=>6)) ;    -- LEGAL
                    N1 : INTEGER := X3(1).C1 ;         -- LEGAL
                    N2 : INTEGER := X2(1)(2) ;         -- LEGAL
                    N4 : T2      := X2(1)(1..2) ;      -- LEGAL
               END PACK3 ;

          END PACK1 ;

     BEGIN

          NULL ;

     END ;

     RESULT ;

END A74205E ;
