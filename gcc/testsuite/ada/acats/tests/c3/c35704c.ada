-- C35704C.ADA

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
-- CHECK THAT DIFFERENT FLOATING POINT TYPES FROM DIFFERENT PARENTS
-- CAN BE USE IN FLOATING POINT RANGE CONSTRAINTS IN TYPE DEFINITIONS.

-- JCR 4/7/82

WITH REPORT;
PROCEDURE C35704C IS

     USE REPORT;

BEGIN
     TEST ("C35704C", "DIFFERENT FLOATING POINT TYPES " &
           "FROM DIFFERENT PARENTS IN FLOATING POINT RANGE " &
           "CONSTRAINT IN TYPE DEFINITION");

     DECLARE

          TYPE F IS DIGITS 5 RANGE -5.0 .. 5.0;
          TYPE F1 IS DIGITS 5 RANGE -5.0 .. 5.0;
     
          TYPE G1 IS DIGITS 5 RANGE  F'FIRST..F1'LAST;
          TYPE G2 IS DIGITS 5 RANGE F1'FIRST..F'LAST;
     
     BEGIN


          IF G1'FIRST /= G1(F'FIRST) OR G1'FIRST /= G1(G2'FIRST) OR
          G1'FIRST /= G1(F1'FIRST) OR G1'LAST /= G1(F'LAST) OR
          G1'LAST /= G1(G2'LAST) OR G1'LAST /= G1(F1'LAST)

          THEN FAILED ("USING FLOAT FROM DIFF PARENTS");

          END IF;
     END;

     RESULT;

END C35704C;
