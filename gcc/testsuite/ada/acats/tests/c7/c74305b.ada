-- C74305B.ADA

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
--    CHECK THAT A DEFERRED CONSTANT CAN BE USED AS A DEFAULT
--    INITIALIZATION FOR A PARAMETER OR AS A DEFAULT INITIA-
--    LIZATION FOR A COMPONENT (GENERIC CASE).

-- EG  12/20/83

WITH REPORT;

PROCEDURE C74305B IS

     USE REPORT;

     PACKAGE PK IS
          TYPE TD IS PRIVATE;
          CD : CONSTANT TD;
          DD : CONSTANT TD;

          GENERIC
               TYPE T1 IS PRIVATE;
               C1 : T1;
               WITH PROCEDURE P2 (A1 : T1 := C1; A2 : TD := CD);
          PROCEDURE P1 (A1 : TD := CD);

     PRIVATE
          TYPE TD IS NEW INTEGER;
          CD : CONSTANT TD := 2;
          DD : CONSTANT TD := 3;
     END PK;

     USE PK;

     PACKAGE BODY PK IS

          PROCEDURE P1 (A1 : TD := CD) IS
          BEGIN
              IF ( A1 /= 2 ) THEN
                   FAILED ("WRONG ACTUAL PARAMETER RECEIVED (1)");
              END IF;
              P2;
          END P1;

          PROCEDURE P3 (X : TD := DD; Y : TD := DD) IS
          BEGIN
              IF ( X /= 2 ) THEN
                   FAILED ("WRONG ACTUAL PARAMETER RECEIVED (2)");
              END IF;
              IF ( Y /= 2 ) THEN
                   FAILED ("WRONG ACTUAL PARAMETER RECEIVED (3)");
              END IF;
          END P3;

          PROCEDURE P4 IS NEW P1 (TD,CD,P3);

     BEGIN
          TEST ("C74305B","CHECK THAT A DEFERRED CONSTANT CAN BE "    &
                          "USED AS A DEFAULT INITIALIZATION FOR A "   &
                          "PARAMETER OR AS A DEFAULT INITIALIZATION " &
                          "FOR A COMPONENT (GENERIC CASE)");
          P4;
     END PK;

     PROCEDURE P5 (X : TD := DD; Y : TD := DD) IS
     BEGIN
          IF ( X /= CD ) THEN
               FAILED ("WRONG ACTUAL PARAMETER RECEIVED (4)");
          END IF;
          IF ( Y /= CD ) THEN
               FAILED ("WRONG ACTUAL PARAMETER RECEIVED (5)");
          END IF;
     END P5;

     PROCEDURE P6 IS NEW P1 (TD,CD,P5);

BEGIN
     P6;
     RESULT;
END C74305B;
