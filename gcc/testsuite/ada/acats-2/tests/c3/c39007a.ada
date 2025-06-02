-- C39007A.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO
-- INSTANTIATE A GENERIC UNIT WHOSE BODY HAS NOT BEEN ELABORATED.
-- CHECK THE FOLLOWING CASE:
--     A) A SIMPLE CASE WHERE THE GENERIC UNIT BODY OCCURS LATER IN
--        THE SAME DECLARATIVE PART.

-- TBN  9/12/86

WITH REPORT; USE REPORT;
PROCEDURE C39007A IS

BEGIN
     TEST ("C39007A", "CHECK THAT PROGRAM_ERROR IS RAISED IF AN " &
                      "ATTEMPT IS MADE TO INSTANTIATE A GENERIC " &
                      "UNIT WHOSE BODY HAS NOT BEEN ELABORATED, " &
                      "BUT OCCURS IN THE SAME DECLARATIVE PART");

     BEGIN
          IF EQUAL (1, 1) THEN
               DECLARE
                    GENERIC
                    PACKAGE P IS
                         A : INTEGER;
                         PROCEDURE ASSIGN (X : OUT INTEGER);
                    END P;

                    PACKAGE NEW_P IS NEW P;

                    PACKAGE BODY P IS
                         PROCEDURE ASSIGN (X : OUT INTEGER) IS
                         BEGIN
                              X := IDENT_INT (1);
                         END ASSIGN;
                    BEGIN
                         ASSIGN (A);
                    END P;

               BEGIN
                    NULL;
               END;
               FAILED ("PROGRAM_ERROR WAS NOT RAISED - 1");
          END IF;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
     END;

------------------------------------------------------------------------

     BEGIN
          IF EQUAL (2, 2) THEN
               DECLARE
                    GENERIC
                    PROCEDURE ADD1 (X : IN OUT INTEGER);

                    PROCEDURE NEW_ADD1 IS NEW ADD1;

                    PROCEDURE ADD1 (X : IN OUT INTEGER) IS
                    BEGIN
                         X := X + IDENT_INT (1);
                    END ADD1;
               BEGIN
                    NULL;
               END;
               FAILED ("PROGRAM_ERROR WAS NOT RAISED - 2");
          END IF;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
     END;

------------------------------------------------------------------------

     BEGIN
          IF EQUAL (3, 3) THEN
               DECLARE
                    GENERIC
                    FUNCTION INIT RETURN INTEGER;

                    FUNCTION NEW_INIT IS NEW INIT;

                    FUNCTION INIT RETURN INTEGER IS
                    BEGIN
                         RETURN (IDENT_INT (1));
                    END INIT;
               BEGIN
                    NULL;
               END;
               FAILED ("PROGRAM_ERROR WAS NOT RAISED - 3");
          END IF;

     EXCEPTION
          WHEN PROGRAM_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
     END;

------------------------------------------------------------------------

     RESULT;
END C39007A;
