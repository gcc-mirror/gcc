-- CC3011D.ADA

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
-- CHECK THAT WHEN A GENERIC PACKAGE INSTANTIATION CONTAINS DECLARATIONS
-- OF SUBPROGRAMS WITH THE SAME SPECIFICATIONS, THE CALLS TO THE
-- SUBPROGRAMS ARE NOT AMBIGIOUS WITHIN THE GENERIC BODY.

-- SPS 5/7/82
-- SPS 2/7/83

WITH REPORT; USE REPORT;

PROCEDURE CC3011D IS
BEGIN
     TEST ("CC3011D", "SUBPROGRAMS WITH SAME SPECIFICATIONS NOT"
          & " AMBIGIOUS WITHIN GENERIC BODY");

     DECLARE
          TYPE FLAG IS (PRT,PRS);
          XX : FLAG;

          GENERIC
               TYPE S IS PRIVATE;
               TYPE T IS PRIVATE;
               V1 : S;
               V2 : T;
          PACKAGE P1 IS
               PROCEDURE PR(X : S);
               PROCEDURE PR(X : T);
          END P1;

          PACKAGE BODY P1 IS
               PROCEDURE PR (X : S) IS
               BEGIN
                    XX := PRS;
               END;

               PROCEDURE PR (X : T ) IS
               BEGIN
                    XX := PRT;
               END;

          BEGIN
               XX := PRT;
               PR (V1);
               IF XX /= PRS THEN
                    FAILED ("WRONG BINDING FOR PR WITH TYPE S");
               END IF;
               XX := PRS;
               PR (V2);
               IF XX /= PRT THEN
                    FAILED ("WRONG BINDING FOR PR WITH TYPE T");
               END IF;
          END P1;

          PACKAGE PAK IS NEW P1 (INTEGER, INTEGER, 1, 2);

     BEGIN
          NULL;
     END;

     RESULT;
END CC3011D;
