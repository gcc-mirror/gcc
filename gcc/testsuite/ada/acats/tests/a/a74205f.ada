-- A74205F.ADA

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
-- CHECK THAT FOR AN ACCESS TYPE WHOSE DESIGNATED TYPE IS A PRIVATE TYPE
-- ADDITIONAL OPERATIONS FOR THE ACCESS TYPE WHICH DEPEND ON
-- CHARACTERISTICS OF THE FULL DECLARATION OF THE PRIVATE TYPE ARE MADE
-- AVAILABLE AT THE EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE
-- ACCESS TYPE DECLARATION AND AFTER THE FULL DECLARATION OF THE PRIVATE
-- TYPE.

-- (1) CHECK FOR COMPONENT SELECTION WITH RECORD TYPES
-- (2) CHECK FOR INDEXED COMPONENTS AND SLICES WITH ARRAY TYPES
-- (3) CHECK FOR USE OF 'FIRST, 'LAST, 'RANGE, AND 'LENGTH WITH ARRAY
--     TYPES

-- DSJ 5/5/83

WITH REPORT ;
PROCEDURE A74205F IS

     USE REPORT ;

BEGIN

     TEST("A74205F", "CHECK THAT ADDITIONAL OPERATIONS OF ACCESS TYPES "
                   & "OF PRIVATE TYPES ARE AVAILABLE AT THE EARLIEST "
                   & "PLACE IN THE IMMEDIATE SCOPE OF THE ACCESS TYPE "
                   & "AND AFTER THE FULL DECLARATION") ;

     DECLARE

          PACKAGE PACK1 IS
               TYPE T1 IS PRIVATE ;
               TYPE T2 IS PRIVATE ;
               PACKAGE PACK2 IS
                    TYPE ACC1 IS ACCESS T1 ;
                    TYPE ACC2 IS ACCESS T2 ;
               END PACK2 ;
          PRIVATE
               TYPE T1 IS ARRAY ( 1 .. 2 ) OF INTEGER ;
               TYPE T2 IS
                    RECORD
                         C1, C2 : INTEGER ;
                    END RECORD ;
          END PACK1 ;

          PACKAGE BODY PACK1 IS
               A1 : PACK2.ACC1 := NEW T1'(2,4) ;  -- LEGAL
               A2 : PACK2.ACC1 := NEW T1'(6,8) ;  -- LEGAL
               R1 : PACK2.ACC2 := NEW T2'(3,5) ;  -- LEGAL
               R2 : PACK2.ACC2 := NEW T2'(7,9) ;  -- LEGAL

               PACKAGE BODY PACK2 IS
                    X1 : INTEGER := A1(1) ;            -- LEGAL
                    X2 : INTEGER := A1'FIRST ;         -- LEGAL
                    X3 : INTEGER := A1'LAST ;          -- LEGAL
                    X4 : INTEGER := A1'LENGTH ;        -- LEGAL
                    B1 : BOOLEAN := 3 IN A1'RANGE ;    -- LEGAL
                    X5 : INTEGER := R1.C1 ;            -- LEGAL
               END PACK2 ;

          END PACK1 ;

     BEGIN

          NULL ;

     END ;

     RESULT ;

END A74205F ;
