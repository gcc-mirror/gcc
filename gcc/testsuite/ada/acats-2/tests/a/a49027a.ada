-- A49027A.ADA

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
--   CHECK THAT A SUBTYPE CAN BE NONSTATIC IN A GENERIC TEMPLATE AND 
--   STATIC IN THE CORRESPONDING INSTANCE.
--   CHECK THAT FOR A GENERIC INSTANTIATION, IF THE ACTUAL PARAMETER
--   IS A STATIC SUBTYPE, THEN EVERY USE OF THE CORRESPONDING FORMAL
--   PARAMETER WITHIN THE INSTANCE IS CONSIDERED TO DENOTE A STATIC
--   SUBTYPE
--
-- THIS IS A TEST BASED ON AI-00409/05-BI-WJ.

-- HISTORY:
--         EDWARD V. BERARD, 27 AUGUST 1990
--         CJJ   10 OCT 1990  TEST OBJECTIVE CHANGED TO REFLECT AIG 
--                            OBJECTIVE.

WITH REPORT ;

PROCEDURE A49027A IS

BEGIN  -- A49027A

    REPORT.TEST ("A49027A", "CHECK THAT A SUBTYPE CAN BE NONSTATIC " &
                 "IN A GENERIC TEMPLATE AND STATIC IN THE " &
                 "CORRESPONDING INSTANCE.") ;
                 
    LOCAL_BLOCK:
    
    DECLARE
    
        TYPE NUMBER IS RANGE 1 .. 10 ;
        
        GENERIC
        
            TYPE NUMBER_TYPE IS RANGE <> ;
            
        PACKAGE STATIC_TEST IS
        
            TYPE NEW_NUMBER_TYPE IS NEW NUMBER_TYPE ;
            SUBTYPE SUB_NUMBER_TYPE IS NUMBER_TYPE ;

        END STATIC_TEST ;
        
        PACKAGE NEW_STATIC_TEST IS NEW STATIC_TEST
                                    (NUMBER_TYPE => NUMBER) ;
                                    
        TYPE ANOTHER_NUMBER IS RANGE 
                NEW_STATIC_TEST.NEW_NUMBER_TYPE'FIRST ..
                NEW_STATIC_TEST.NEW_NUMBER_TYPE'LAST ;
        
        TYPE YET_ANOTHER_NUMBER IS RANGE 
                NEW_STATIC_TEST.SUB_NUMBER_TYPE'FIRST ..
                NEW_STATIC_TEST.SUB_NUMBER_TYPE'LAST ;
        
    BEGIN  -- LOCAL_BLOCK
    
        NULL ;
        
    END LOCAL_BLOCK ;
    
    REPORT.RESULT ;
    
END A49027A ;
