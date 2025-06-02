-- C93008A.ADA

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
-- CHECK THAT FOR A TASK CREATED BY AN OBJECT DECLARATION, EXECUTION 
-- DOES NOT PROCEED IN PARALLEL WITH ACTIVATION.

-- R.WILLIAMS 8/20/86

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C93008A IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     TASK T IS
          ENTRY FINIT_POS (DIGT : IN ARG);
     END T;

     TASK BODY T IS
     BEGIN
          LOOP
               SELECT
                    ACCEPT FINIT_POS (DIGT : IN ARG) DO
                         SPYNUMB := 10*SPYNUMB+DIGT;
                    END FINIT_POS;
               OR
                    TERMINATE;
               END SELECT;          
          END LOOP;
     END T;

BEGIN

     TEST ("C93008A", "CHECK THAT EXECUTION DOES NOT PROCEED IN  " &
                      "PARALLEL WITH ACTIVATION OF A TASK CREATED " &
                      "BY AN OBJECT DECLARATION");

BLOCK:
     DECLARE

          TASK TYPE TT1;

          TASK TT2;

          T1 : TT1;

          TASK BODY TT1 IS
               PACKAGE DUMMY IS
               END DUMMY;

               PACKAGE BODY DUMMY IS
               BEGIN
                    DELAY 2.0 * Impdef.One_Second;
                    T.FINIT_POS(1);
               END DUMMY;
          BEGIN
               NULL;
          END TT1;

          TASK BODY TT2 IS
               PACKAGE DUMMY IS
               END DUMMY;

               PACKAGE BODY DUMMY IS
               BEGIN
                    DELAY 2.0 * Impdef.One_Second;
                    T.FINIT_POS(2);
               END DUMMY;
          BEGIN
               NULL;
          END TT2;
          

     BEGIN               -- TASKS ACTIVATED NOW.

          IF SPYNUMB = 12 OR SPYNUMB = 21 THEN
               NULL;
          ELSE
               FAILED ("TASKS NOT ACTIVATED PROPERLY - SPYNUMB HAS " &
                       "ACTUAL VALUE OF: " & INTEGER'IMAGE(SPYNUMB));
          END IF;
     END BLOCK;

     RESULT;

END C93008A;
