-- A83009B.ADA

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
--     CHECK THAT A DERIVED TYPE DECLARATION IN A GENERIC
--     UNIT MAY DERIVE TWO OR MORE SUBPROGRAM HOMOGRAPHS.
--     CHECK THE CASES WHERE:
--          1) THE DERIVED SUBPROGRAMS BECOME HOMOGRAPHS BECAUSE OF THE
--             SUBSTITUTION OF THE DERIVED TYPE FOR THE PARENT TYPE IN
--             THE IMPLICIT SUBPROGRAM SPECIFICATIONS.
--          2) THE PARENT TYPE IS DECLARED IN A GENERIC INSTANCE AND
--             THE INSTANCE INCLUDES TWO OR MORE DERIVABLE SUBPROGRAMS
--             THAT ARE HOMOGRAPHS AS A RESULT OF THE ARGUMENTS GIVEN
--             FOR THE GENERIC FORMAL-TYPE PARAMETERS.
--     TEST CASES WHERE THE DERIVED TYPE DECLARATIONS ARE GIVEN IN:
--          . THE VISIBLE PART OF A GENERIC PACKAGE SPECIFICATION,
--          . THE PRIVATE PART OF A GENERIC PACKAGE SPECIFICATION,
--          . A GENERIC PACKAGE BODY,
--          . A GENERIC SUBPROGRAM BODY.
--
-- HISTORY:
--     DHH 09/20/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE A83009B IS
     TYPE ENUM IS (E1, E2, E3);

     GENERIC
          TYPE T1 IS (<>);
          TYPE T2 IS (<>);
     PACKAGE G_PACK IS
          TYPE PARENT IS (E1, E2, E3);

          PROCEDURE HP (P1 : PARENT; P2 : T1);
          PROCEDURE HP (P3 : PARENT; P4 : T2);

          FUNCTION HF (P1 : T1) RETURN PARENT;
          FUNCTION HF (P2 : T2) RETURN PARENT;
     END G_PACK;

     PACKAGE BODY G_PACK IS
          PROCEDURE HP (P1 : PARENT; P2 : T1) IS
          BEGIN
               NULL;
          END HP;

          PROCEDURE HP (P3 : PARENT; P4 : T2) IS
          BEGIN
               NULL;
          END HP;

          FUNCTION HF (P1 : T1) RETURN PARENT IS
          BEGIN
               RETURN E1;
          END HF;

          FUNCTION HF (P2 : T2) RETURN PARENT IS
          BEGIN
               RETURN E2;
          END HF;
     END G_PACK;
BEGIN
     TEST ("A83009B", "A DERIVED TYPE DECLARATION IN A GENERIC " &
                      "UNIT MAY DERIVE TWO OR MORE SUBPROGRAM " &
                      "HOMOGRAPHS");

     DECLARE
     -- SUBPROGRAMS BECOME HOMOGRAPHS BECAUSE OF SUBSTITUTION.

          GENERIC
          PACKAGE PACK2 IS
               TYPE CHILD1 IS PRIVATE;

               PACKAGE IN_PACK2 IS
                    TYPE PARENT IS (E1, E2, E3);
                    PROCEDURE HP (P1 : PARENT; P2 : CHILD1);
                    PROCEDURE HP (P3 : CHILD1; P4 : PARENT);

                    FUNCTION HF (P1 : CHILD1; P2 : PARENT)
                                RETURN PARENT;
                    FUNCTION HF (P3 : PARENT; P4 : CHILD1)
                                RETURN PARENT;
               END IN_PACK2;

               USE IN_PACK2;
          PRIVATE
               TYPE CHILD1 IS NEW IN_PACK2.PARENT;  -- PRIVATE PART
          END PACK2;                                -- OF SPEC.

          PACKAGE BODY PACK2 IS
               TYPE CHILD2 IS NEW CHILD1;  -- VISIBLE PART OF BODY.

               GENERIC
               PACKAGE IN_BODY IS
                    TYPE CHILD3 IS NEW CHILD1; -- VISIBLE PART OF SPEC.
               END IN_BODY;

               GENERIC
               PROCEDURE P;
               PROCEDURE P IS
                    TYPE CHILD4 IS NEW CHILD1;  -- SUBPROGRAM BODY.
               BEGIN
                    NULL;
               END;

               PACKAGE BODY IN_PACK2 IS
                    PROCEDURE HP (P1 : PARENT; P2 : CHILD1) IS
                    BEGIN
                         NULL;
                    END HP;

                    PROCEDURE HP (P3 : CHILD1; P4 : PARENT) IS
                    BEGIN
                         NULL;
                    END HP;

                    FUNCTION HF (P1 : CHILD1; P2 : PARENT)
                                RETURN PARENT IS
                    BEGIN
                         RETURN E1;
                    END HF;

                    FUNCTION HF (P3 : PARENT; P4 : CHILD1)
                                RETURN PARENT IS
                    BEGIN
                         RETURN E2;
                    END HF;
               END IN_PACK2;
          BEGIN
               NULL;
          END PACK2;
     BEGIN
          NULL;
     END;

     DECLARE
     -- PARENT TYPE IN GENERIC INSTANCE HAS DERIVABLE HOMOGRAPHS.

          GENERIC
          PACKAGE PACK1 IS
               PACKAGE INSTANCE2 IS
                  NEW G_PACK (CHARACTER, CHARACTER);

               TYPE CHILD2 IS NEW INSTANCE2.PARENT;
               TYPE CHILD3 IS PRIVATE;
          PRIVATE
               PACKAGE INSTANCE3 IS
                    NEW G_PACK (ENUM, ENUM);

               TYPE CHILD3 IS NEW INSTANCE3.PARENT;
          END PACK1;

          GENERIC
          PROCEDURE P1;
          PROCEDURE P1 IS
               PACKAGE INSTANCE4 IS
                    NEW G_PACK (BOOLEAN, BOOLEAN);

               TYPE CHILD4 IS NEW INSTANCE4.PARENT;
          BEGIN
               NULL;
          END P1;

          PACKAGE BODY PACK1 IS
               PACKAGE INSTANCE5 IS
                    NEW G_PACK (ENUM, ENUM);

               TYPE CHILD5 IS NEW INSTANCE5.PARENT;
          END PACK1;
     BEGIN
          NULL;
     END;

     RESULT;
END A83009B;
