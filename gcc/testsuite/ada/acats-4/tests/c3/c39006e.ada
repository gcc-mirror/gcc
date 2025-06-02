-- C39006E.ADA

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
-- CHECK THAT PROGRAM_ERROR IS NOT RAISED IF A SUBPROGRAM'S BODY HAS
-- BEEN ELABORATED BEFORE IT IS CALLED. CHECK THE FOLLOWING:
--     A) A SUBPROGRAM CAN APPEAR IN A NON-ELABORATED DECLARATIVE PART
--        OR PACKAGE SPECIFICATION BEFORE ITS BODY.

-- TBN  8/21/86

WITH REPORT; USE REPORT;
PROCEDURE C39006E IS

BEGIN
     TEST ("C39006E", "CHECK THAT PROGRAM_ERROR IS NOT RAISED IF A " &
                      "SUBPROGRAM IS CALLED IN A NON-ELABORATED " &
                      "DECLARATIVE PART OR PACKAGE SPECIFICATION " &
                      "BEFORE ITS BODY IS ELABORATED");
     DECLARE -- (A)

          FUNCTION INIT_1 (A : INTEGER) RETURN INTEGER;

          PACKAGE P IS
               PROCEDURE USE_INIT1;
          END P;

          PACKAGE BODY P IS
               PROCEDURE USE_INIT1 IS
               BEGIN
                    IF NOT EQUAL (3, 3) THEN
                         DECLARE
                              X : INTEGER := INIT_1 (1);
                         BEGIN
                              NULL;
                         END;
                    ELSE
                         NULL;
                    END IF;

               EXCEPTION
                    WHEN PROGRAM_ERROR =>
                         FAILED ("PROGRAM_ERROR RAISED - 1");
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
               END USE_INIT1;

          BEGIN
               USE_INIT1;
          END P;

          FUNCTION INIT_1 (A : INTEGER) RETURN INTEGER IS
          BEGIN
               RETURN (A + IDENT_INT(1));
          END INIT_1;

     BEGIN -- (A)
          NULL;
     END; -- (A)

     DECLARE -- (B)

          PROCEDURE INIT_2 (A : IN OUT INTEGER);

          PACKAGE P IS
               FUNCTION USE_INIT2 RETURN BOOLEAN;
          END P;

          PACKAGE BODY P IS
               FUNCTION USE_INIT2 RETURN BOOLEAN IS
               BEGIN
                    IF NOT EQUAL (3, 3) THEN
                         DECLARE
                              X : INTEGER;
                         BEGIN
                              INIT_2 (X);
                         END;
                    END IF;
                    RETURN IDENT_BOOL (FALSE);

               EXCEPTION
                    WHEN PROGRAM_ERROR =>
                         FAILED ("PROGRAM_ERROR RAISED - 2");
                         RETURN FALSE;
                    WHEN OTHERS =>
                         FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
                         RETURN FALSE;
               END USE_INIT2;
          BEGIN
               IF USE_INIT2 THEN
                    FAILED ("INCORRECT RESULTS FROM FUNCTION CALL - 2");
               END IF;
          END P;

          PROCEDURE INIT_2 (A : IN OUT INTEGER) IS
          BEGIN
               A := A + IDENT_INT(1);
          END INIT_2;

     BEGIN -- (B)
          NULL;
     END; -- (B)

     DECLARE -- (C)
          FUNCTION INIT_3 RETURN INTEGER;

          PACKAGE Q IS
               VAR : INTEGER;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               IF NOT EQUAL (3, 3) THEN
                    VAR := INIT_3;
               END IF;
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    FAILED ("PROGRAM_ERROR RAISED - 3");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
          END Q;

          FUNCTION INIT_3 RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (1);
          END INIT_3;

     BEGIN -- (C)
          NULL;
     END; -- (C)

     DECLARE -- (D)
          PROCEDURE INIT_4 (A : IN OUT INTEGER);

          PACKAGE Q IS
               VAR : INTEGER := 1;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               IF NOT EQUAL (3, 3) THEN
                    INIT_4 (VAR);
               END IF;
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    FAILED ("PROGRAM_ERROR RAISED - 4");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
          END Q;

          PROCEDURE INIT_4 (A : IN OUT INTEGER) IS
          BEGIN
               A := IDENT_INT (4);
          END INIT_4;

     BEGIN -- (D)
          NULL;
     END; -- (D)

     BEGIN -- (E)

          DECLARE
               FUNCTION INIT_5 (A : INTEGER) RETURN INTEGER;

               PROCEDURE USE_INIT5 IS
                    PACKAGE Q IS
                         X : INTEGER := INIT_5 (1);
                    END Q;
                    USE Q;
               BEGIN
                    X := IDENT_INT (5);

               END USE_INIT5;

               FUNCTION INIT_5 (A : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN (A + IDENT_INT(1));
               END INIT_5;

          BEGIN
               USE_INIT5;
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               FAILED ("PROGRAM_ERROR RAISED - 5");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 5");

     END; -- (E)

     RESULT;
END C39006E;
