-- C74206A.ADA

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
-- CHECK THAT IF A COMPOSITE TYPE IS DECLARED IN THE PACKAGE AS A
-- PRIVATE TYPE AND CONTAINS A COMPONENT OF THE PRIVATE TYPE, OPERATIONS
-- OF THE COMPOSITE TYPE WHICH DO NOT DEPEND ON CHARACTERISTICS OF THE
-- PRIVATE TYPE ARE AVAILABLE AFTER THE FULL DECLARATION OF THE PRIVATE
-- TYPE, BUT BEFORE THE EARLIEST PLACE WITHIN THE IMMEDIATE SCOPE OF THE
-- DECLARATION OF THE COMPOSITE TYPE THAT IS AFTER THE FULL DECLARATION
-- OF THE PRIVATE TYPE.  IN PARTICULAR, CHECK FOR THE FOLLOWING :

--   'FIRST, 'LAST, 'RANGE, AND 'LENGTH FOR ARRAY TYPES
--   SELECTED COMPONENTS FOR DISCRIMINANTS AND COMPONENTS OF RECORDS
--   INDEXED COMPONENTS AND SLICES FOR ARRAYS

-- DSJ 5/5/83
-- JBG 3/8/84

WITH REPORT; 
PROCEDURE C74206A IS

     USE REPORT; 

BEGIN

     TEST("C74206A", "CHECK THAT ADDITIONAL OPERATIONS FOR "
                   & "COMPOSITE TYPES OF PRIVATE TYPES ARE "
                   & "AVAILABLE AT THE EARLIEST PLACE AFTER THE "
                   & "FULL DECLARATION OF THE PRIVATE TYPE EVEN "
                   & "IF BEFORE THE EARLIEST PLACE WITHIN THE "
                   & "IMMEDIATE SCOPE OF THE COMPOSITE TYPE"); 

     DECLARE

          PACKAGE PACK1 IS
               TYPE P1  IS PRIVATE; 
               TYPE LP1 IS LIMITED PRIVATE; 

               PACKAGE PACK_LP IS
                    TYPE LP_ARR IS ARRAY (1 .. 2) OF LP1; 
                    TYPE LP_REC (D : INTEGER) IS
                         RECORD
                              C1, C2 : LP1; 
                         END RECORD; 
               END PACK_LP; 

               PACKAGE PACK2 IS
                    TYPE ARR IS ARRAY ( 1 .. 2 ) OF P1; 
                    TYPE REC (D : INTEGER) IS
                         RECORD
                              C1, C2 : P1; 
                         END RECORD; 
               END PACK2; 
          PRIVATE
               TYPE P1  IS NEW BOOLEAN; 
               TYPE LP1 IS NEW BOOLEAN; 
          END PACK1; 

          PACKAGE BODY PACK1 IS

               USE PACK_LP; 
               USE PACK2; 

               A1 : ARR; 
               L1 : LP_ARR; 

               N1 : INTEGER := ARR'FIRST;           -- LEGAL
               N2 : INTEGER := ARR'LAST;            -- LEGAL
               N3 : INTEGER := A1'LENGTH;           -- LEGAL
               N4 : INTEGER := LP_ARR'FIRST;        -- LEGAL
               N5 : INTEGER := LP_ARR'LAST;         -- LEGAL
               N6 : INTEGER := L1'LENGTH;           -- LEGAL
               B1 : BOOLEAN := 1 IN ARR'RANGE;      -- LEGAL
               B2 : BOOLEAN := 5 IN LP_ARR'RANGE;   -- LEGAL

               N7 : INTEGER := A1(1)'SIZE;          -- LEGAL: A1(1)
               N8 : INTEGER := L1(2)'SIZE;          -- LEGAL: L1(2)

               R1 : REC(1); 
               Q1 : LP_REC(1); 

               K1 : INTEGER := R1.D'SIZE;           -- LEGAL: R1.D
               K2 : INTEGER := R1.C1'SIZE;          -- LEGAL: R1.C1
               K3 : INTEGER := Q1.D'SIZE;           -- LEGAL: Q1.D
               K4 : INTEGER := Q1.C2'SIZE;          -- LEGAL: Q1.C2

          BEGIN

               IF N1 /= 1 OR N4 /= 1 THEN
                    FAILED ("WRONG VALUE FOR 'FIRST"); 
               END IF; 

               IF N2 /= 2 OR N5 /= 2 THEN
                    FAILED ("WRONG VALUE FOR 'LAST"); 
               END IF; 

               IF N3 /= 2 OR N6 /= 2 THEN
                    FAILED ("WRONG VALUE FOR 'LENGTH"); 
               END IF; 

               IF B1 /= TRUE OR B2 /= FALSE THEN
                    FAILED ("INCORRECT RANGE TEST"); 
               END IF; 

               IF N7 /= N8 THEN
                    FAILED ("INCORRECT INDEXED COMPONENTS"); 
               END IF; 

               IF K1 /= K3 OR K2 /= K4 THEN
                    FAILED ("INCORRECT COMPONENT SELECTION"); 
               END IF; 

          END PACK1; 

     BEGIN

          NULL; 

     END; 

     RESULT; 

END C74206A; 
