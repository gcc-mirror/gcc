-- A73001J.ADA

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
-- CHECK THAT IF A SUBPROGRAM IS DECLARED BY A RENAMING DECLARATION OR
-- GENERIC INSTANTIATION IN A GENERIC PACKAGE SPECIFICATION, NO PACKAGE
-- BODY IS REQUIRED.


-- BHS 6/27/84

WITH REPORT;
PROCEDURE A73001J IS

     USE REPORT;

BEGIN

     TEST ("A73001J", "CHECK THAT NO PACKAGE BODY IS REQUIRED FOR " &
                      "SUBPROGRAM DECLARED BY RENAMING DECLARATION " &
                      "OR GENERIC INSTANTIATION IN A GENERIC " &
                      "PACKAGE SPECIFICATION");

     DECLARE
          GENERIC
               TYPE ITEM IS RANGE <>;
          PACKAGE PACK1 IS
               FUNCTION ADDI (X,Y : ITEM) RETURN ITEM RENAMES "+";
          END PACK1;

     BEGIN
          NULL;
     END;


     DECLARE
          GENERIC
               TYPE ITEM IS RANGE <>;
          PROCEDURE P (X : IN OUT ITEM);

          PROCEDURE P (X : IN OUT ITEM) IS
          BEGIN
               NULL;
          END P;

          GENERIC
               TYPE OBJ IS RANGE <>;
          PACKAGE PACK2 IS
               PROCEDURE NADA IS NEW P (OBJ);
          END PACK2;

     BEGIN
          NULL;
     END;

     RESULT;

END A73001J;
