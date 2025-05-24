-- C32112B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR THE DECLARATION OF A NULL
-- ARRAY OBJECT IF THE INITIAL VALUE IS NOT A NULL ARRAY.

-- RJW 7/20/86
-- GMT 7/01/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--              CHANGED THE RANGE VALUES OF A FEW DIMENSIONS.

WITH REPORT; USE REPORT;

PROCEDURE C32112B IS

     TYPE ARR1 IS ARRAY (NATURAL RANGE <>) OF INTEGER;
     SUBTYPE NARR1 IS ARR1 (IDENT_INT (2) .. IDENT_INT (1));


     TYPE ARR2 IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>)
          OF INTEGER;
     SUBTYPE NARR2 IS ARR2 (IDENT_INT (1) .. IDENT_INT (2),
                            IDENT_INT (1) .. IDENT_INT (0));

BEGIN
     TEST ("C32112B", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                      "THE DECLARATION OF A NULL ARRAY OBJECT IF " &
                      "THE INITIAL VALUE IS NOT A NULL ARRAY");

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT(1) .. IDENT_INT(2));
               N1A : NARR1 := (A'RANGE => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1A'");
               A(1) := IDENT_INT(N1A(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1A'");
     END;

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT (1) .. IDENT_INT (2));
               N1B : CONSTANT NARR1 := (A'RANGE => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1B'");
               A(1) := IDENT_INT(N1B(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1B'");
     END;

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT (1) .. IDENT_INT (1));
               N1C : CONSTANT NARR1 := (A'RANGE => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1C'");
               A(1) := IDENT_INT(N1C(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1C'");
     END;

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT (1) .. IDENT_INT (1));
               N1D : NARR1 := (A'RANGE => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1D'");
               A(1) := IDENT_INT(N1D(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1D'");
     END;

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT (0) .. IDENT_INT (1));
               N1E : ARR1 (IDENT_INT (1) .. IDENT_INT (0)) :=
                          (A'RANGE  => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1E'");
               A(1) := IDENT_INT(N1E(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N1E'");
     END;

     BEGIN
          DECLARE
               A   : ARR1 (IDENT_INT (0) .. IDENT_INT (1));
               N1F : CONSTANT ARR1 (IDENT_INT (1) .. IDENT_INT (0)) :=
                     (A'RANGE => 0);
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1F'");
               A(1) := IDENT_INT(N1F(1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N1F'");
     END;

     BEGIN
          DECLARE
               A   : ARR2 (IDENT_INT (1) .. IDENT_INT (2),
                           IDENT_INT (0) .. IDENT_INT (1));
               N2A : CONSTANT NARR2 := (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2'");
               A(1,1) := IDENT_INT(N2A(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2A'");
     END;

     BEGIN
          DECLARE
               A : ARR2 (IDENT_INT (1) .. IDENT_INT (2),
                         IDENT_INT (0) .. IDENT_INT (1));
               N2B : NARR2 := (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2B'");
               A(1,1) := IDENT_INT(N2B(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2B'");
     END;

     BEGIN
          DECLARE
               A : ARR2 (IDENT_INT (1) .. IDENT_INT (3),
                         IDENT_INT (1) .. IDENT_INT (1));
               N2C : CONSTANT NARR2 := (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2C'");
               A(1,1) := IDENT_INT(N2C(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2C'");
     END;

     BEGIN
          DECLARE
               A : ARR2 (IDENT_INT (1) .. IDENT_INT (3),
                         IDENT_INT (1) .. IDENT_INT (1));
               N2D : NARR2 := (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2D'");
               A(1,1) := IDENT_INT(N2D(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2D'");
     END;

     BEGIN
          DECLARE
               A : ARR2 (IDENT_INT (1) .. IDENT_INT (1),
                         IDENT_INT (1) .. IDENT_INT (1));
               N2E : CONSTANT ARR2 (IDENT_INT (2) .. IDENT_INT (1),
                                    IDENT_INT (1) .. IDENT_INT (1)) :=
                                   (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2E'");
               A(1,1) := IDENT_INT(N2E(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF CONSTANT 'N2E'");
     END;

     BEGIN
          DECLARE
               A   : ARR2 (IDENT_INT (1) .. IDENT_INT (1),
                           IDENT_INT (1) .. IDENT_INT (1));
               N2F : ARR2 (IDENT_INT (2) .. IDENT_INT (1),
                           IDENT_INT (1) .. IDENT_INT (1)) :=
                           (A'RANGE => (A'RANGE (2) =>0));
          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2F'");
               A(1,1) := IDENT_INT(N2F(1,1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR INITIALIZATION " &
                       "OF VARIABLE 'N2F'");
     END;

     RESULT;
END C32112B;
