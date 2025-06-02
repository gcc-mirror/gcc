-- C41101D.ADA

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
-- FOR INDEXED COMPONENTS OF THE FORM F(...), CHECK THAT 
--   THE NUMBER OF INDEX VALUES, THE TYPE OF THE INDEX
--   VALUES, AND THE REQUIRED TYPE OF THE INDEXED COMPONENT
--   ARE USED TO RESOLVE AN OVERLOADING OF F.

-- WKB 8/12/81
-- JBG 10/12/81
-- SPS 11/1/82

WITH REPORT;
PROCEDURE C41101D IS

     USE REPORT;

     TYPE T1 IS ARRAY (1..10) OF INTEGER;
     TYPE T2 IS ARRAY (1..10, 1..10) OF INTEGER;
     I : INTEGER;

     TYPE U1 IS (MON,TUE,WED,THU,FRI);
     TYPE U2 IS ARRAY (U1 RANGE MON..THU) OF INTEGER;

     TYPE V1 IS ARRAY (1..10) OF BOOLEAN;
     B : BOOLEAN;

     FUNCTION F RETURN T1 IS
     BEGIN
          RETURN (1..10 => 1);
     END F;

     FUNCTION F RETURN T2 IS
     BEGIN
          RETURN (1..10 => (1..10 => 2));
     END F;

     FUNCTION G RETURN U2 IS
     BEGIN
          RETURN (MON..THU => 3);
     END G;

     FUNCTION G RETURN T1 IS
     BEGIN
          RETURN (1..10 => 4);
     END G;

     FUNCTION H RETURN T1 IS
     BEGIN
          RETURN (1..10 => 5);
     END H;

     FUNCTION H RETURN V1 IS
     BEGIN
          RETURN (1..10 => FALSE);
     END H;

BEGIN

     TEST ("C41101D", "WHEN INDEXING FUNCTION RESULTS, INDEX TYPE, " &
                      "NUMBER OF INDICES, AND COMPONENT TYPE ARE " &
                      "USED FOR OVERLOADING RESOLUTION");

     I := F(7);              -- NUMBER OF INDEX VALUES.
     IF I /= IDENT_INT(1) THEN
          FAILED ("WRONG VALUE - 1");
     END IF;

     I := G(3);              -- INDEX TYPE.
     IF I /= IDENT_INT(4) THEN
          FAILED ("WRONG VALUE - 2");
     END IF;

     B := H(5);              -- COMPONENT TYPE.
     IF B /= IDENT_BOOL(FALSE) THEN
          FAILED ("WRONG VALUE - 3");
     END IF;

     RESULT;

END C41101D;
