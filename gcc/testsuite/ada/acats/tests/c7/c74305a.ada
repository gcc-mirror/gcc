-- C74305A.ADA

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
--    LIZATION FOR A COMPONENT (NON GENERIC CASE).

-- DAT  4/06/81
-- RM   5/21/81
-- SPS  8/23/82
-- SPS  2/10/83
-- SPS 10/20/83
-- EG  12/20/83
-- GJD 11/15/95  REMOVED ADA 95 INCOMPATIBILITY.

WITH REPORT;

PROCEDURE C74305A IS

     USE REPORT;

     PACKAGE PK IS
          TYPE T1 IS PRIVATE;
          TYPE T2 IS PRIVATE; 
          C1 : CONSTANT T1;                   -- OK.

          PROCEDURE P1 (P : T1 := C1);        -- OK.                

          TYPE R1 IS RECORD
               C : T1 := C1;                  -- OK.                
          END RECORD;
     PRIVATE
          PROCEDURE PROC2 (P : T1 := C1);     -- OK.                

          TYPE R2 IS RECORD
               C : T1 := C1;                  -- OK.                
               D : INTEGER := C1'SIZE;        -- OK.                
          END RECORD;

          FUNCTION F1 (P : T1) RETURN T1;

          TYPE T1 IS NEW INTEGER;
          TYPE T2 IS ARRAY (1..2) OF INTEGER; -- OK.

          FUNCTION F2 (P : T1) RETURN T1;

          PROCEDURE P3 (P : T1 := C1+1);      -- OK.

          PROCEDURE P4 (P : T1 := F1(C1));
  
          TYPE R5 IS RECORD
               C : T1 := F2(C1);
          END RECORD;

          PROCEDURE P5 (P : T1 := C1+2) RENAMES P3;

          TYPE R3 IS RECORD
               C : T1 := C1;                  -- OK.
          END RECORD;

          C1 : CONSTANT T1 := 1;              -- OK.
          C2 : CONSTANT T2 := (1,1);          -- OK. 
     END PK;

     USE PK;

     PACKAGE BODY PK IS

          R11 : R1;

          PROCEDURE P1 (P : T1 := C1) IS
          BEGIN
               IF ( P /= 1 ) THEN
                    FAILED ("PARAMETER DEFAULT OF P1 NOT PROPERLY " &
                            "INITIALIZED");
               END IF;
          END P1;

          PROCEDURE PROC2 (P : T1 := C1) IS
          BEGIN NULL; END PROC2;

          PROCEDURE P3 (P : T1 := C1+1) IS
          BEGIN
               IF ( P /= 3 ) THEN
                    FAILED ("PARAMETER DEFAULT OF P5 NOT PROPERLY " &
                            "INITIALIZED");
               END IF;
          END P3;

          FUNCTION F1 (P : T1) RETURN T1 IS
          BEGIN
               RETURN P+10;
          END F1;

          PROCEDURE P4 (P : T1 := F1(C1)) IS
          BEGIN
               IF ( P /= 11 ) THEN
                    FAILED ("WRONG ACTUAL PARAMETER RECEIVED");
               END IF;
          END P4;

          FUNCTION F2 (P : T1) RETURN T1 IS
          BEGIN
               RETURN P+20;
          END F2;

     BEGIN -- PK BODY.

          DECLARE

               R55 : R5;

          BEGIN
               TEST ("C74305A","CHECK THAT A DEFERRED CONSTANT CAN " &
                               "BE USED AS A DEFAULT INITIALIZATION " &
                               "FOR A PARAMETER OR AS A DEFAULT " &
                               "INITIALIZATION FOR A COMPONENT (NON " &
                               "GENERIC CASE)");

               IF ( R11.C /= 1 ) THEN
                    FAILED ("RECORD R11 NOT PROPERLY INITIALIZED");
               END IF;

               P4;

               IF ( R55.C /= 21 ) THEN
                    FAILED ("RECORD R55 NOT PROPERLY INITIALIZED");
               END IF;

               P5;
          END;
     END PK;

BEGIN

     P1;

     RESULT;
END C74305A;
