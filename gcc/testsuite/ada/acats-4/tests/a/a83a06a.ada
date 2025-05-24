-- A83A06A.ADA

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
-- CHECK THAT A STATEMENT LABEL INSIDE A BLOCK BODY CAN BE THE
--    SAME AS A VARIABLE, CONSTANT, NAMED LITERAL, SUBPROGRAM,
--    ENUMERATION LITERAL, TYPE, OR PACKAGE DECLARED IN THE
--    ENCLOSING BODY.


-- RM 02/12/80
-- JBG 5/16/83
-- JBG 8/21/83
-- JRK 12/19/83

WITH REPORT; USE REPORT;
PROCEDURE  A83A06A  IS

     LAB_VAR            :  INTEGER;
     LAB_CONST          :  CONSTANT INTEGER := 12;
     LAB_NAMEDLITERAL   :  CONSTANT := 13;
     TYPE  ENUM  IS        ( AA , BB , LAB_ENUMERAL );
     TYPE  LAB_TYPE  IS    NEW INTEGER;

     PROCEDURE  LAB_PROCEDURE  IS
     BEGIN
          NULL;
     END LAB_PROCEDURE;

     FUNCTION  LAB_FUNCTION  RETURN INTEGER  IS
     BEGIN
          RETURN 7;
     END LAB_FUNCTION;

     PACKAGE  LAB_PACKAGE  IS
          INT : INTEGER;
     END LAB_PACKAGE;

BEGIN

     TEST ("A83A06A", "CHECK THAT STATEMENT LABELS INSIDE A BLOCK " &
                      "BODY CAN BE THE SAME AS IDENTIFIERS DECLARED "&
                      "OUTSIDE THE BODY");

     LAB_BLOCK_1 :   BEGIN  NULL;  END     LAB_BLOCK_1;

     LAB_LOOP_1  :   LOOP   EXIT;  END LOOP LAB_LOOP_1;

     BEGIN

          << LAB_VAR >>                    -- OK.
               BEGIN NULL; END;
          << LAB_ENUMERAL >>        NULL;  -- OK.

     << LAB_PROCEDURE >>                   -- OK.
          FOR  I  IN  INTEGER  LOOP
               << LAB_CONST >>      NULL;  -- OK.
               << LAB_TYPE >>       NULL;  -- OK.
               << LAB_FUNCTION >>   EXIT;  -- OK.
          END LOOP;

          << LAB_NAMEDLITERAL >>    NULL;  
          << LAB_PACKAGE >>         NULL;  
     END;

     LAB_BLOCK_2 :                          -- OK.
          BEGIN  NULL;  END LAB_BLOCK_2;

     LAB_LOOP_2  :                          -- OK.
          LOOP   EXIT;  END LOOP LAB_LOOP_2;

     RESULT;

END A83A06A;
