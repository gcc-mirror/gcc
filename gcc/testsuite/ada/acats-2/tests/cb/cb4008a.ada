-- CB4008A.ADA

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
-- CHECK THAT NESTED LAST WISHES EXCEPTION HANDLERS WORK 
-- (FOR PROCEDURES).

-- DAT 4/15/81
-- SPS 3/28/83

WITH REPORT; USE REPORT;

PROCEDURE CB4008A IS

     C : INTEGER := 0;

     E : EXCEPTION;

     DEPTH : CONSTANT := 99;

     PROCEDURE F;

     PROCEDURE I IS
     BEGIN
          C := C + 1;
          IF C >= DEPTH THEN
               RAISE E;
          END IF;
     END I;

     PROCEDURE O IS
     BEGIN
          C := C - 1;
     END O;

     PROCEDURE X IS
          PROCEDURE X1 IS
               PROCEDURE X2 IS
               BEGIN
                    F;
               END X2;

               PROCEDURE X3 IS
               BEGIN
                    I;
                    X2;
               EXCEPTION
                    WHEN E => O; RAISE;
               END X3;
          BEGIN
               I;
               X3;
          EXCEPTION
               WHEN E => O; RAISE;
          END X1;

          PROCEDURE X1A IS
          BEGIN
               I;
               X1;
               FAILED ("INCORRECT EXECUTION SEQUENCE");
          EXCEPTION
               WHEN E => O; RAISE;
          END X1A;
     BEGIN
          I;
          X1A;
     EXCEPTION
          WHEN E => O; RAISE;
     END X;

     PROCEDURE Y IS
     BEGIN
          I;
          X;
     EXCEPTION WHEN E => O; RAISE;
     END Y;

     PROCEDURE F IS
          PROCEDURE F2;

          PROCEDURE F1 IS
          BEGIN
               I;
               F2;
          EXCEPTION WHEN E => O; RAISE;
          END F1;

          PROCEDURE F2 IS
          BEGIN
               I;
               Y;
          EXCEPTION WHEN E => O; RAISE;
          END F2;
     BEGIN
          I;
          F1;
     EXCEPTION WHEN E => O; RAISE;
     END F;

BEGIN
     TEST ("CB4008A", "(PROCEDURE) LAST WISHES UNWIND PROPERLY");

     BEGIN
          I;
          Y;
          FAILED ("INCORRECT EXECUTION SEQUENCE 2");
     EXCEPTION
          WHEN E =>
               O;
               IF C /= 0 THEN
                    FAILED ("EXCEPTION HANDLER MISSED SOMEWHERE");
               END IF;
     END;

     RESULT;
END CB4008A;
