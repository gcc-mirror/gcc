-- C39006A.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO CALL A
-- SUBPROGRAM WHOSE BODY HAS NOT YET BEEN ELABORATED.  CHECK THE
-- FOLLOWING:
--     A) A FUNCTION IS CALLED IN THE INITIALIZATION EXPRESSION OF A
--        SCALAR VARIABLE OR A RECORD COMPONENT, AND THE SCALAR OR
--        RECORD VARIABLE'S DECLARATION IS ELABORATED BEFORE THE
--        SUBPROGRAM BODY IS ELABORATED.

-- TBN  8/14/86

WITH REPORT; USE REPORT;
PROCEDURE C39006A IS

BEGIN
     TEST ("C39006A", "CHECK THAT PROGRAM_ERROR IS RAISED IF AN " &
                      "ATTEMPT IS MADE TO CALL A SUBPROGRAM WHOSE " &
                      "BODY HAS NOT YET BEEN ELABORATED");
     BEGIN
          DECLARE

               FUNCTION INIT_1 (A : INTEGER) RETURN INTEGER;

               VAR1 : INTEGER := INIT_1 (1);

               FUNCTION INIT_1 (A : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN (A + IDENT_INT(1));
               END INIT_1;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 1");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
     END;

     BEGIN
          DECLARE

               FUNCTION INIT_2 (A : INTEGER) RETURN INTEGER;

               TYPE REC1 IS
                    RECORD
                         NUMBER : INTEGER := INIT_2 (2);
                    END RECORD;

               VAR2 : REC1;

               FUNCTION INIT_2 (A : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN (A + IDENT_INT(1));
               END INIT_2;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 2");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

     BEGIN
          DECLARE

               FUNCTION F1 RETURN INTEGER;

               PACKAGE PACK IS
                    VAR1 : INTEGER := F1;
               END PACK;

               FUNCTION F1 RETURN INTEGER IS
               BEGIN
                    RETURN (IDENT_INT(1));
               END F1;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 3");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
     END;

     BEGIN
          DECLARE

               PACKAGE PACK IS
                    FUNCTION F2 RETURN INTEGER;
                    VAR2 : INTEGER := F2;
               END PACK;

               PACKAGE BODY PACK IS
                    FUNCTION F2 RETURN INTEGER IS
                    BEGIN
                         RETURN (IDENT_INT(3));
                    END F2;
               END PACK;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 4");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
     END;

     BEGIN
          DECLARE

               FUNCTION INIT_3 (A : INTEGER) RETURN INTEGER;

               GENERIC
                    PACKAGE Q IS
                         VAR1 : INTEGER := INIT_3 (1);
                    END Q;

               PACKAGE NEW_Q IS NEW Q;

               FUNCTION INIT_3 (A : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN (A + IDENT_INT(3));
               END INIT_3;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 5");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 5");
     END;

     BEGIN
          DECLARE

               FUNCTION FUN RETURN INTEGER;

               TYPE PARAM IS
                    RECORD
                         COMP : INTEGER := FUN;
                    END RECORD;

               GENERIC
                    TYPE T IS PRIVATE;
               PACKAGE GP IS
                    OBJ : T;
               END GP;

               PACKAGE INST IS NEW GP(PARAM);

               FUNCTION FUN RETURN INTEGER IS
               BEGIN
                    RETURN (IDENT_INT(3));
               END FUN;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 6");
          END;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 6");
     END;

     RESULT;
END C39006A;
