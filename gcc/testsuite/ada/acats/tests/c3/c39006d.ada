-- C39006D.ADA

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
-- CHECK THAT IF A FUNCTION IS USED IN A DEFAULT EXPRESSION FOR A
-- SUBPROGRAM OR FORMAL GENERIC PARAMETER, PROGRAM_ERROR IS RAISED
-- WHEN AN ATTEMPT IS MADE TO EVALUATE THE DEFAULT EXPRESSION,
-- BECAUSE THE FUNCTION'S BODY HAS NOT BEEN ELABORATED YET.

-- TBN  8/20/86

WITH REPORT; USE REPORT;
PROCEDURE C39006D IS

BEGIN
     TEST ("C39006D", "CHECK THAT IF A FUNCTION IS USED IN A DEFAULT " &
                      "EXPRESSION FOR A SUBPROGRAM OR FORMAL GENERIC " &
                      "PARAMETER, PROGRAM_ERROR IS RAISED WHEN AN " &
                      "ATTEMPT IS MADE TO EVALUATE THE DEFAULT " &
                      "EXPRESSION");
     DECLARE
          FUNCTION FUN RETURN INTEGER;

          PACKAGE P IS
               PROCEDURE DEFAULT (A : INTEGER := FUN);
          END P;

          PACKAGE BODY P IS
               PROCEDURE DEFAULT (A : INTEGER := FUN) IS
                    B : INTEGER := 1;
               BEGIN
                    B := B + IDENT_INT(A);
               END DEFAULT;
          BEGIN
               DEFAULT (2);
               DEFAULT;
               FAILED ("PROGRAM_ERROR NOT RAISED - 1");
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
          END P;

          FUNCTION FUN RETURN INTEGER IS
          BEGIN
               RETURN (IDENT_INT(1));
          END FUN;
     BEGIN
          NULL;
     END;

     BEGIN
          DECLARE
               FUNCTION INIT_1 RETURN INTEGER;

               GENERIC
                    LENGTH : INTEGER := INIT_1;
               PACKAGE P IS
                    TYPE ARRAY1 IS ARRAY (1 .. LENGTH) OF INTEGER;
               END P;

               PACKAGE NEW_P1 IS NEW P (4);
               PACKAGE NEW_P2 IS NEW P;

               FUNCTION INIT_1 RETURN INTEGER IS
               BEGIN
                    RETURN (IDENT_INT(2));
               END INIT_1;

          BEGIN
               FAILED ("PROGRAM_ERROR NOT RAISED - 2");
          END;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

     DECLARE
          FUNCTION INIT_2 RETURN INTEGER;

          GLOBAL_INT : INTEGER := IDENT_INT(1);

          GENERIC
          PACKAGE Q IS
               PROCEDURE ADD1 (A : INTEGER := INIT_2);
          END Q;

          PACKAGE BODY Q IS
               PROCEDURE ADD1 (A : INTEGER := INIT_2) IS
                    B : INTEGER;
               BEGIN
                    B := A;
               END ADD1;
          BEGIN
               IF GLOBAL_INT = IDENT_INT(1) THEN
                    ADD1;
                    FAILED ("PROGRAM_ERROR NOT RAISED - 3");
               ELSE
                    ADD1 (2);
               END IF;

          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
          END Q;

          PACKAGE NEW_Q IS NEW Q;

          FUNCTION INIT_2 RETURN INTEGER IS
          BEGIN
               RETURN (IDENT_INT(1));
          END INIT_2;

     BEGIN
          NULL;
     END;

     RESULT;
END C39006D;
