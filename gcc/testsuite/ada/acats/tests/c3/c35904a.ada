-- C35904A.ADA
 
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
-- OBJECTIVE:
--     CHECK THAT INCOMPATIBLE FIXED POINT CONSTRAINTS RAISE
--     APPROPRIATE EXCEPTIONS.
 
 
-- HISTORY:
--     RJK 05/17/83  CREATED ORIGINAL TEST.
--     PWB 02/03/86  CORRECTED TEST ERROR:
--                   ADDED POSSIBLITY OF NUMERIC_ERROR
--                   IN DECLARATIONS OF SFX3 AND SFX4.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  CHANGED RANGE
--                   CONSTRAINTS OF SUBTYPE SFX1.  CHANGED UPPER BOUND
--                   OF THE CONSTRAINT OF SFX4.  CHANGED RANGE
--                   CONSTRAINTS OF FIX.
--     JRL 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS.
--     EDS 07/16/98  AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C35904A IS

     TYPE FIX IS DELTA 0.5 RANGE -3.0 .. 3.0;

BEGIN

     TEST ("C35904A", "CHECK THAT INCOMPATIBLE FIXED POINT " &
                      "CONSTRAINTS RAISE APPROPRIATE EXCEPTION");
 
-- TEST FOR CORRECT SUBTYPE DEFINITION FOR COMPATIBILITY BETWEEN TYPE
-- AND SUBTYPE CONSTRAINTS.
 
     BEGIN
 
          DECLARE
 
               SUBTYPE SFX1 IS FIX DELTA 1.0 RANGE 0.0 .. 2.0;   -- OK.
               SFX1_VAR : SFX1;
 
          BEGIN
               SFX1_VAR := 1.0;
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
                FAILED ("FIXED POINT CONSTRAINTS ARE NOT IN ERROR");
          WHEN OTHERS =>
                FAILED ("EXCEPTION SHOULD NOT BE RAISED WHILE " &
                        "CHECKING DELTA CONSTRAINT");
     END;

-- TEST FOR INCORRECT SUBTYPE DEFINITION ON ACCURACY BETWEEN TYPE AND
-- SUBTYPE DEFINITIONS.

     BEGIN

          DECLARE

               SUBTYPE SFX IS FIX DELTA 0.1;  -- DELTA IS SMALLER FOR
                                              -- SUBTYPE THAN FOR TYPE.
               -- DEFINE AN OBJECT OF SUBTYPE SFX AND USE IT TO AVOID 
               -- OPTIMIZATION OF SUBTYPE
               SFX_VAR : SFX := FIX(IDENT_INT(1));

          BEGIN
               FAILED ("NO EXCEPTION RAISED FOR INCOMPATABLE DELTA " & 
                       FIX'IMAGE(SFX_VAR) );  --USE SFX_VAR
          END;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("INCORRECT EXCEPTION RAISED WHILE CHECKING " &
                       "DELTA CONSTRAINT");
     END;

     RESULT;

END C35904A;
