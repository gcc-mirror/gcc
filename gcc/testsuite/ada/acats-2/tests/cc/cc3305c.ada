-- CC3305C.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS A SCALAR TYPE, THE BOUNDS OF
-- THE ACTUAL PARAMETER ARE USED WITHIN THE INSTANTIATED UNIT.

-- CHECK WHEN THE SCALAR TYPE IS DEFINED BY DIGITS <>.

-- SPS 7/15/82

WITH REPORT;
USE REPORT;

PROCEDURE CC3305C IS
BEGIN

     TEST ("CC3305C", "TEST THE BOUNDS OF GENERIC FORMAL SCALAR " &
           "TYPES OF THE FORM DIGITS <>");

     DECLARE
          SUBTYPE FL IS FLOAT RANGE 1.0 .. 3.0;

          GENERIC
               TYPE GFT IS DIGITS <>;
          PACKAGE PK IS END PK;

          PACKAGE BODY PK IS
          BEGIN
               FOR I IN IDENT_INT(0) .. IDENT_INT(4) LOOP
                    COMMENT ("START OF ITERATION");
                    DECLARE
                         VAR : GFT;
                    BEGIN
                         VAR := GFT (I);
                         IF I = IDENT_INT(0) OR I = IDENT_INT(4) THEN
                              FAILED ("CONSTRAINT_ERROR NOT RAISED");
                         END IF;
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              IF I /= IDENT_INT(0) AND 
                                 I /= IDENT_INT(4) THEN
                                   FAILED ("CONSTRAINT_ERROR RAISED " &
                                      "INAPPROPRIATELY");
                              END IF;
                    END;
               END LOOP;
          END PK;

     BEGIN

          DECLARE
               PACKAGE NP IS NEW PK (FL);
          BEGIN
               NULL;
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED ON INSTANTIATION");
     END;

     RESULT;
END CC3305C;
