-- C35102A.ADA

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
-- CHECK THAT AN ENUMERATION LITERAL BELONGING TO ONE ENUMERATION TYPE
-- MAY BE DECLARED IN ANOTHER ENUMERATION TYPE DEFINITION IN THE SAME
-- DECLARATIVE REGION.

-- R.WILLIAMS 8/20/86
-- GMT 6/30/87           MOVED THE CALL TO  REPORT.TEST INTO A NEWLY
--                       CREATED PACKAGE NAMED SHOW_TEST_HEADER.
--                       ADDED CODE FOR MY_PACK AND MY_FTN.


WITH REPORT; USE REPORT;
PROCEDURE C35102A IS

     TYPE E1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
     TYPE E2 IS ('A', 'C', RED, BLUE);

     PACKAGE  SHOW_TEST_HEADER  IS
              -- PURPOSE OF THIS PACKAGE:
              -- WE WANT THE TEST HEADER INFORMATION TO BE
              -- PRINTED  BEFORE  ANY OF THE  PASS/FAIL  MESSAGES.
     END SHOW_TEST_HEADER;

     PACKAGE  BODY  SHOW_TEST_HEADER  IS
     BEGIN
          TEST ( "C35102A",
                 "CHECK THAT AN ENUMERATION LITERAL BELONGING "   &
                 "TO ONE ENUMERATION TYPE MAY BE DECLARED IN "    &
                 "ANOTHER ENUMERATION TYPE DEFINITION IN THE "    &
                 "SAME DECLARATIVE REGION" );
     END SHOW_TEST_HEADER;

     FUNCTION  MY_FTN (  E : E1  ) RETURN  E2  IS
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);
     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN MY_FTN - 1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN MY_FTN - 1" );
          END IF;

          RETURN E2'VAL (  IDENT_INT ( E1'POS(E) )  );
     END MY_FTN;


     PACKAGE MY_PACK IS
     END MY_PACK;

     PACKAGE BODY MY_PACK IS
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);
     BEGIN  -- MY_PACK
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN MY_PACK - 1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN MY_PACK - 1" );
          END IF;
     END MY_PACK;

     PACKAGE PKG IS
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     END PKG;

     PACKAGE BODY PKG IS
     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN PKG - 1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN PKG - 1" );
          END IF;
     END PKG;

     PACKAGE PRIV IS
          TYPE ENUM1 IS PRIVATE;
          TYPE ENUM2 IS PRIVATE;

          FUNCTION FE1 (E : E1) RETURN ENUM1;

          FUNCTION FE2 (E : E2) RETURN ENUM2;

     PRIVATE
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     END PRIV;

     PACKAGE BODY PRIV IS
          FUNCTION FE1 (E : E1) RETURN ENUM1 IS
          BEGIN
               RETURN ENUM1'VAL (IDENT_INT (E1'POS (E)));
          END FE1;

          FUNCTION FE2 (E : E2) RETURN ENUM2 IS
          BEGIN
               RETURN ENUM2'VAL (IDENT_INT (E2'POS (E)));
          END FE2;

     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN PRIV - 1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN PRIV - 1" );
          END IF;
     END PRIV;

     PACKAGE LPRIV IS
          TYPE ENUM1 IS LIMITED PRIVATE;
          TYPE ENUM2 IS LIMITED PRIVATE;

          FUNCTION FE1 (E : E1) RETURN ENUM1;

          FUNCTION FE2 (E : E2) RETURN ENUM2;

          FUNCTION EQUALS (A, B : ENUM1) RETURN BOOLEAN;

          FUNCTION EQUALS (A, B : ENUM2) RETURN BOOLEAN;

     PRIVATE
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     END LPRIV;

     PACKAGE BODY LPRIV IS
          FUNCTION FE1 (E : E1) RETURN ENUM1 IS
          BEGIN
               RETURN ENUM1'VAL (IDENT_INT (E1'POS (E)));
          END FE1;

          FUNCTION FE2 (E : E2) RETURN ENUM2 IS
          BEGIN
               RETURN ENUM2'VAL (IDENT_INT (E2'POS (E)));
          END FE2;

          FUNCTION EQUALS (A, B : ENUM1) RETURN BOOLEAN IS
          BEGIN
               IF A = B THEN
                    RETURN TRUE;
               ELSE
                    RETURN FALSE;
               END IF;
          END EQUALS;

          FUNCTION EQUALS (A, B : ENUM2) RETURN BOOLEAN IS
          BEGIN
               IF A = B THEN
                    RETURN TRUE;
               ELSE
                    RETURN FALSE;
               END IF;
          END EQUALS;
     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN LPRIV - 1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN LPRIV - 2" );
          END IF;
     END LPRIV;

     TASK T1;

     TASK BODY T1 IS
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN T1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN T1" );
          END IF;
     END T1;

     TASK T2 IS
          ENTRY E;
     END T2;

     TASK BODY T2 IS
     BEGIN
          ACCEPT E DO
               DECLARE
                    TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
                    TYPE ENUM2 IS ('A', 'C', RED, BLUE);

               BEGIN
                    IF ENUM2'SUCC ('A') /= 'C' THEN
                    FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                             "IN T2.E" );
                    END IF;

                    IF ENUM1'POS (RED) /= 3 THEN
                         FAILED ( "RED NOT DECLARED CORRECTLY IN " &
                                  "ENUM1 IN T2.E" );
                    END IF;
               END;
          END E;
     END T2;

     GENERIC
     PROCEDURE GP1;

     PROCEDURE GP1 IS
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN GP1" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN GP1" );
          END IF;
     END GP1;

     GENERIC
          TYPE E1 IS (<>);
          TYPE E2 IS (<>);
     PROCEDURE GP2;

     PROCEDURE GP2 IS
     BEGIN
          IF E2'SUCC (E2'VALUE ("'A'")) /= E2'VALUE ("'C'") THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN E2 " &
                        "IN GP2" );
          END IF;

          IF E1'POS (E1'VALUE ("RED")) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN E1 " &
                        "IN GP2" );
          END IF;
     END GP2;

     PROCEDURE NEWGP1 IS NEW GP1;
     PROCEDURE NEWGP2 IS NEW GP2 (E1, E2);

BEGIN

     DECLARE
          TYPE ENUM1 IS ('A', 'B', 'C', RED, YELLOW, BLUE);
          TYPE ENUM2 IS ('A', 'C', RED, BLUE);

     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN BLOCK" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN BLOCK" );
          END IF;
     END;

     DECLARE
          USE PKG;
     BEGIN
          IF ENUM2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN PKG - 2" );
          END IF;

          IF ENUM1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN PKG - 2" );
          END IF;
     END;

     DECLARE
          USE PRIV;
     BEGIN
          IF FE2 (E2'SUCC('A')) /= FE2 ('C') THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN PRIV - 2" );
          END IF;

          IF FE1 (RED) /= FE1 (E1'VAL (3)) THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN PRIV - 2" );
          END IF;
     END;

     DECLARE
          USE LPRIV;
     BEGIN
          IF NOT EQUALS (FE2 (E2'SUCC('A')), FE2 ('C')) THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN ENUM2 " &
                        "IN LPRIV - 2" );
          END IF;

          IF NOT EQUALS (FE1 (RED), FE1 (E1'VAL (3))) THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN ENUM1 " &
                        "IN LPRIV - 2" );
          END IF;
     END;

     BEGIN
          IF E2'SUCC ('A') /= 'C' THEN
               FAILED ( "'A' NOT DECLARED CORRECTLY IN E2" );
          END IF;

          IF E1'POS (RED) /= 3 THEN
               FAILED ( "RED NOT DECLARED CORRECTLY IN E1" );
          END IF;
     END;

     NEWGP1;
     NEWGP2;
     T2.E;

     RESULT;
END C35102A;
