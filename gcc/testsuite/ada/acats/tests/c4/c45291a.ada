-- C45291A.ADA

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
-- OBJECTIVE:
--     CHECK THAT THE MEMBERSHIP TESTS YIELD CORRECT RESULTS FOR TASK
--     TYPES, LIMITED PRIVATE TYPES, COMPOSITE LIMITED TYPES, AND
--     PRIVATE TYPES WITHOUT DISCRIMINANTS.

-- HISTORY:
--     JET 08/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C45291A IS

     TASK TYPE TASK1 IS
          ENTRY E;
     END TASK1;

     PACKAGE PACK IS
          TYPE LIM_PRIV IS LIMITED PRIVATE;
          TYPE LIM_COMP IS ARRAY (1..10) OF LIM_PRIV;
          TYPE PRIV IS PRIVATE;
          PROCEDURE INIT(LP : OUT LIM_PRIV;
                         LC : IN OUT LIM_COMP;
                         P  : OUT PRIV);
     PRIVATE
          TYPE LIM_PRIV IS RANGE -100..100;
          TYPE PRIV IS RECORD
               I : INTEGER;
          END RECORD;
     END PACK;

     SUBTYPE SUB_TASK1 IS TASK1;
     SUBTYPE SUB_LIM_PRIV IS PACK.LIM_PRIV;
     SUBTYPE SUB_LIM_COMP IS PACK.LIM_COMP;
     SUBTYPE SUB_PRIV IS PACK.PRIV;

     T1 : TASK1;
     LP : PACK.LIM_PRIV;
     LC : PACK.LIM_COMP;
     P  : PACK.PRIV;

     TASK BODY TASK1 IS
     BEGIN
          ACCEPT E DO
               NULL;
          END E;
     END TASK1;

     PACKAGE BODY PACK IS
          PROCEDURE INIT (LP : OUT LIM_PRIV;
                          LC : IN OUT LIM_COMP;
                          P  : OUT PRIV) IS
          BEGIN
               LP := 0;
               LC := (OTHERS => 0);
               P  := (I => 0);
          END INIT;
     END PACK;

BEGIN
     TEST ("C45291A", "CHECK THAT THE MEMBERSHIP TESTS YIELD CORRECT " &
                      "RESULTS FOR TASK TYPES, LIMITED PRIVATE TYPES," &
                      " COMPOSITE LIMITED TYPES, AND PRIVATE TYPES " &
                      "WITHOUT DISCRIMINANTS");

     PACK.INIT(LP, LC, P);

     IF NOT IDENT_BOOL(T1 IN TASK1) THEN
          FAILED ("INCORRECT VALUE OF 'T1 IN TASK1'");
     END IF;

     IF IDENT_BOOL(T1 NOT IN TASK1) THEN
          FAILED ("INCORRECT VALUE OF 'T1 NOT IN TASK1'");
     END IF;

     IF NOT IDENT_BOOL(LP IN PACK.LIM_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'LP IN LIM_PRIV'");
     END IF;

     IF IDENT_BOOL(LP NOT IN PACK.LIM_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'LP NOT IN LIM_PRIV'");
     END IF;

     IF NOT IDENT_BOOL(LC IN PACK.LIM_COMP) THEN
          FAILED ("INCORRECT VALUE OF 'LC IN LIM_COMP'");
     END IF;

     IF IDENT_BOOL(LC NOT IN PACK.LIM_COMP) THEN
          FAILED ("INCORRECT VALUE OF 'LC NOT IN LIM_COMP'");
     END IF;

     IF NOT IDENT_BOOL(P IN PACK.PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'P IN PRIV'");
     END IF;

     IF IDENT_BOOL(P NOT IN PACK.PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'P NOT IN PRIV'");
     END IF;

     IF NOT IDENT_BOOL(T1 IN SUB_TASK1) THEN
          FAILED ("INCORRECT VALUE OF 'T1 IN SUB_TASK1'");
     END IF;

     IF IDENT_BOOL(T1 NOT IN SUB_TASK1) THEN
          FAILED ("INCORRECT VALUE OF 'T1 NOT IN SUB_TASK1'");
     END IF;

     IF NOT IDENT_BOOL(LP IN SUB_LIM_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'LP IN SUB_LIM_PRIV'");
     END IF;

     IF IDENT_BOOL(LP NOT IN SUB_LIM_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'LP NOT IN SUB_LIM_PRIV'");
     END IF;

     IF NOT IDENT_BOOL(LC IN SUB_LIM_COMP) THEN
          FAILED ("INCORRECT VALUE OF 'LC IN SUB_LIM_COMP'");
     END IF;

     IF IDENT_BOOL(LC NOT IN SUB_LIM_COMP) THEN
          FAILED ("INCORRECT VALUE OF 'LC NOT IN SUB_LIM_COMP'");
     END IF;

     IF NOT IDENT_BOOL(P IN SUB_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'P IN SUB_PRIV'");
     END IF;

     IF IDENT_BOOL(P NOT IN SUB_PRIV) THEN
          FAILED ("INCORRECT VALUE OF 'P NOT IN SUB_PRIV'");
     END IF;

     T1.E;

     RESULT;

END C45291A;
