-- C37213J.ADA

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
--     CHECK, FOR A GENERIC FORMAL TYPE - WHERE A DISCRIMINANT OR AN
--     INDEX CONSTRAINT DEPENDS ON A RECORD DISCRIMINANT AND THE
--     RECORD TYPE IS CONSTRAINED BY DEFAULT - USED TO DECLARE AN
--     OBJECT OR A SUBTYPE, THAT THE NON-DISCRIMINANT EXPRESSIONS
--     OF THE CONSTRAINT ARE CHECKED FOR COMPATIBILITY:
--          1) ONLY IN AN OBJECT DECLARATION, AND
--          2) ONLY IF THE DISCRIMINANT-DEPENDENT COMPONENT IS PRESENT
--             IN THE SUBTYPE.

-- HISTORY:
--     JBG  10/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER; SEPARATED THIS TEST INTO
--                    3 NEW TESTS (J,K,L); CHANGED THE AGGREGATE FOR
--                    THE PARAMETER 'VALUE' IN THE CALL OF PROCEDURE
--                    'SUBTYPE_CHK1';  MOVED THE CALL TO REPORT.TEST
--                    SO THAT IT COMES BEFORE ANY DECLARATIONS; ADDED
--                    A SEQUENCE COUNTER TO IDENTIFY WHICH SUBTEST
--                    DECLARATION PART RAISES CONSTRAINT_ERROR.
--     VCL  03/28/88  MODIFIED THE TEST DISCRIPTION TO MORE ACCURATELY
--                    DESCRIBE THE OBJECTIVE; CHANGED THE FORMAL
--                    PARAMETERS TO THE GENERIC UNITS AND THE
--                    CORRESPONDING ACTUAL PARAMETERS; REORGANIZED THE
--                    TEST SO THAT ALL OPERATIONS ON A SPECIFIC TYPE
--                    ARE TOGETHER.

WITH REPORT; USE REPORT;
PROCEDURE C37213J IS
BEGIN
     TEST ("C37213J", "THE NON-DISCRIMINANT VALUES OF A DISCRIMINANT " &
                      "OR AN INDEX CONSTRAINT THAT DEPEND ON A " &
                      "DISCRIMINANT ARE PROPERLY CHECKED WHEN THE " &
                      "RECORD TYPE IS CONSTRAINED BY DEFAULT AND " &
                      "USED AS THE ACTUAL PARAMETER TO A GENERIC " &
                      "FORMAL TYPE USED TO DECLARE AN OBJECT OR A " &
                      "SUBTYPE");

     DECLARE
          SUBTYPE SM IS INTEGER RANGE 1..10;
          TYPE REC (D1, D2 : SM) IS
               RECORD NULL; END RECORD;
          TYPE MY_ARR IS ARRAY (SM RANGE <>) OF INTEGER;

          SEQUENCE_NUMBER : INTEGER;

          GENERIC
               TYPE CONS IS PRIVATE;
               OBJ_XCP : BOOLEAN;
               TAG     : STRING;
          PACKAGE OBJ_CHK IS END OBJ_CHK;

          GENERIC
               TYPE CONS IS PRIVATE;
          PROCEDURE SUBTYP_CHK (OBJ_XCP : BOOLEAN;
                                TAG     : STRING);

          PACKAGE BODY OBJ_CHK IS
          BEGIN          -- DECLARE AN OBJECT OF THE FORMAL TYPE.
               DECLARE
                    X : CONS;

                    FUNCTION VALUE RETURN CONS IS
                    BEGIN
                         IF EQUAL (3,3) THEN
                              RETURN X;
                         ELSE
                              RETURN X;
                         END IF;
                    END VALUE;
               BEGIN
                    IF OBJ_XCP THEN
                         FAILED ("NO CHECK DURING DECLARATION " &
                                 "OF OBJECT OF TYPE CONS - " & TAG);
                    ELSIF X /= VALUE THEN
                         FAILED ("INCORRECT VALUE FOR OBJECT OF " &
                                 "TYPE CONS - " & TAG);
                    END IF;
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT OBJ_XCP THEN
                         FAILED ("IMPROPER CONSTRAINT CHECKED " &
                                 "DURING DECLARATION OF OBJECT " &
                                 "OF TYPE CONS - " & TAG);
                    END IF;
          END OBJ_CHK;

          PROCEDURE SUBTYP_CHK (OBJ_XCP : BOOLEAN;
                                TAG     : STRING)    IS
          BEGIN          -- DECLARE A SUBTYPE OF THE FORMAL TYPE.
               DECLARE
                    SUBTYPE SCONS IS CONS;
               BEGIN
                    DECLARE
                         X : SCONS;

                         FUNCTION VALUE RETURN SCONS IS
                         BEGIN
                              IF EQUAL (5, 5) THEN
                                   RETURN X;
                              ELSE
                                   RETURN X;
                              END IF;
                         END VALUE;
                    BEGIN
                         IF OBJ_XCP THEN
                              FAILED ("NO CHECK DURING DECLARATION " &
                                      "OF OBJECT OF SUBTYPE SCONS - " &
                                      TAG);
                         ELSIF X /= VALUE THEN
                              FAILED ("INCORRECT VALUE FOR OBJECT " &
                                      "OF SUBTYPE SCONS - " & TAG);
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         IF NOT OBJ_XCP THEN
                              FAILED ("IMPROPER CONSTRAINT CHECKED " &
                                      "DURING DECLARATION OF OBJECT " &
                                      "OF SUBTYPE SCONS - " & TAG);
                         END IF;
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ("CONSTRAINT IMPROPERLY CHECKED " &
                            "DURING SUBTYPE DECLARATION - " & TAG);
          END SUBTYP_CHK;
     BEGIN
          SEQUENCE_NUMBER := 1;
          DECLARE
               TYPE REC_DEF (D3 : INTEGER := 1) IS
                    RECORD
                         C1 : REC (D3, 0);
                    END RECORD;

               PACKAGE PACK1 IS NEW OBJ_CHK (REC_DEF,
                         OBJ_XCP => TRUE,
                         TAG     => "PACK1");

               PROCEDURE PROC1 IS NEW SUBTYP_CHK (REC_DEF);
          BEGIN
               PROC1 (OBJ_XCP => TRUE, TAG => "PROC1");
          END;

          SEQUENCE_NUMBER := 2;
          DECLARE
               TYPE ARR_DEF (D3 : INTEGER := IDENT_INT(1)) IS
                    RECORD
                         C1 : MY_ARR (0..D3);
                    END RECORD;

               PACKAGE PACK2 IS NEW OBJ_CHK (ARR_DEF,
                         OBJ_XCP => TRUE,
                         TAG     => "PACK2");

               PROCEDURE PROC2 IS NEW SUBTYP_CHK (ARR_DEF);
          BEGIN
               PROC2 (OBJ_XCP => TRUE, TAG => "PROC2");
          END;


          SEQUENCE_NUMBER := 3;
          DECLARE
               TYPE VAR_REC_DEF1 (D3 : INTEGER := 1) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : REC (D3, IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK3 IS NEW OBJ_CHK (VAR_REC_DEF1,
                              OBJ_XCP => TRUE,
                              TAG     => "PACK3");

               PROCEDURE PROC3 IS NEW SUBTYP_CHK (VAR_REC_DEF1);
          BEGIN
               PROC3 (OBJ_XCP => TRUE, TAG => "PROC3");
          END;

          SEQUENCE_NUMBER := 4;
          DECLARE
               TYPE VAR_REC_DEF6 (D3 : INTEGER := IDENT_INT(-6)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : REC (D3, IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK4 IS NEW OBJ_CHK (VAR_REC_DEF6,
                              OBJ_XCP => FALSE,
                              TAG     => "PACK4");

               PROCEDURE PROC4 IS NEW SUBTYP_CHK (VAR_REC_DEF6);
          BEGIN
               PROC4 (OBJ_XCP => FALSE,TAG => "PROC4");
          END;

          SEQUENCE_NUMBER := 5;
          DECLARE
               TYPE VAR_REC_DEF11 (D3 : INTEGER := 11) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : REC (D3, IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK5 IS NEW OBJ_CHK (VAR_REC_DEF11,
                              OBJ_XCP => FALSE,
                              TAG     => "PACK5");

               PROCEDURE PROC5 IS NEW SUBTYP_CHK (VAR_REC_DEF11);
          BEGIN
               PROC5 (OBJ_XCP => FALSE, TAG => "PROC5");
          END;

          SEQUENCE_NUMBER := 6;
          DECLARE
               TYPE VAR_ARR_DEF1 (D3 : INTEGER := IDENT_INT(1)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK6 IS NEW OBJ_CHK (VAR_ARR_DEF1,
                              OBJ_XCP => TRUE,
                              TAG     => "PACK6");

               PROCEDURE PROC6 IS NEW SUBTYP_CHK (VAR_ARR_DEF1);
          BEGIN
               PROC6 (OBJ_XCP => TRUE, TAG => "PROC6");
          END;

          SEQUENCE_NUMBER := 7;
          DECLARE
               TYPE VAR_ARR_DEF6 (D3 : INTEGER := -6) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK7 IS NEW OBJ_CHK (VAR_ARR_DEF6,
                              OBJ_XCP => FALSE,
                              TAG     => "PACK7");

               PROCEDURE PROC7 IS NEW SUBTYP_CHK (VAR_ARR_DEF6);
          BEGIN
               PROC7 (OBJ_XCP => FALSE, TAG => "PROC7");
          END;

          SEQUENCE_NUMBER := 8;
          DECLARE
               TYPE VAR_ARR_DEF11 (D3 : INTEGER := IDENT_INT(11)) IS
                    RECORD
                         CASE D3 IS
                              WHEN -5..10 =>
                                   C1 : MY_ARR(D3..IDENT_INT(11));
                              WHEN OTHERS =>
                                   C2 : INTEGER := IDENT_INT(5);
                         END CASE;
                    END RECORD;

               PACKAGE PACK8 IS NEW OBJ_CHK (VAR_ARR_DEF11,
                              OBJ_XCP => FALSE,
                              TAG     => "PACK8");

               PROCEDURE PROC8 IS NEW SUBTYP_CHK (VAR_ARR_DEF11);
          BEGIN
               PROC8 (OBJ_XCP => FALSE, TAG => "PROC8");
          END;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED DURING DECLARATION / " &
                       "INSTANTIATION ELABORATION - " &
                       INTEGER'IMAGE(SEQUENCE_NUMBER));
     END;

     RESULT;
END C37213J;
