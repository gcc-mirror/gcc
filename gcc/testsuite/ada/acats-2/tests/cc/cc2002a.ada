-- CC2002A.ADA

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
-- CHECK THAT THE ELABORATION OF A GENERIC BODY HAS NO EFFECT OTHER
-- THAN TO ESTABLISH THE TEMPLATE BODY TO BE USED FOR THE
-- CORRESPONDING INSTANTIATIONS.

-- ASL 09/02/81
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE CC2002A IS

     GLOBAL : INTEGER := 0;
     Q : INTEGER RANGE 1..1 := 1;
BEGIN
     TEST ("CC2002A","NO SIDE EFFECTS OF ELABORATION OF GENERIC BODY");

     BEGIN
          DECLARE
               GENERIC
                    PACKAGE P IS
                    END P;

               GENERIC PROCEDURE PROC;

                    PROCEDURE PROC IS
                         C : CONSTANT INTEGER RANGE 1 .. 1 := 2;
                    BEGIN
                         RAISE PROGRAM_ERROR;
                    END PROC;

                    PACKAGE BODY P IS
                         C : CONSTANT BOOLEAN := 
                              BOOLEAN'SUCC(IDENT_BOOL(TRUE));
                    BEGIN
                         GLOBAL := 1;
                         Q := Q + 1;
                    END P;
          BEGIN
               NULL;
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED DURING ELABORATION OF " &
                       "GENERIC BODY");
     END;

     IF GLOBAL /= 0 THEN
          FAILED ("VALUE OF GLOBAL VARIABLE CHANGED BY ELABORATION " &
                  "OF GENERIC BODY");
     END IF;

     RESULT;
END CC2002A;
