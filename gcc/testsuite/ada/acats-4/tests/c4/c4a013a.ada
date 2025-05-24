-- C4A013A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A NONSTATIC
-- UNIVERSAL_REAL EXPRESSION IF THE VALUE WOULD LIE OUTSIDE THE RANGE OF
-- THE BASE TYPE OF THE MOST ACCURATE PREDEFINED FLOATING POINT TYPE AND
-- MACHINE_OVERFLOWS IS TRUE FOR THAT TYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- BAW 29 SEPT 80
-- TBN 10/30/85     RENAMED FROM C4A013A.ADA.
-- JRK 1/13/86      COMPLETELY REVISED TO CHECK NONSTATIC UNIVERSAL_REAL
--                  EXPRESSIONS WHOSE RESULTS OVERFLOW.  REVISED
--                  NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--                  AI-00387.
-- MRM 03/30/93     REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH SYSTEM, REPORT;
USE SYSTEM, REPORT;

PROCEDURE C4A013A IS

     TYPE F IS DIGITS MAX_DIGITS;

     B : BOOLEAN;

BEGIN
     TEST ("C4A013A", "CHECK NONSTATIC UNIVERSAL_REAL EXPRESSIONS " &
                      "WHOSE RESULTS OVERFLOW");

     BEGIN
          B := 1.0 < 1.0 / (1.0 * INTEGER'POS (IDENT_INT (0)));

          IF F'MACHINE_OVERFLOWS THEN
               FAILED ("MACHINE_OVERFLOWS IS TRUE, BUT NO EXCEPTION " &
                       "WAS RAISED");
          ELSE COMMENT ("MACHINE_OVERFLOWS IS FALSE AND NO EXCEPTION " &
                        "WAS RAISED");
          END IF;

          IF NOT B THEN  -- USE B TO PREVENT DEAD VARIABLE OPTIMIZATION.
               COMMENT ("1.0 < 1.0 / 0.0 YIELDS FALSE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ("CONSTRAINT_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");
     END;

     RESULT;
END C4A013A;
