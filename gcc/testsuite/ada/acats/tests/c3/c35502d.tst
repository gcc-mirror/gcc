-- C35502D.TST

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
-- CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULT FOR THE
-- LONGEST POSSIBLE ENUMERATION LITERAL.

-- RJW 2/21/86

WITH REPORT; USE REPORT;

PROCEDURE C35502D IS

BEGIN
     TEST ("C35502D", "CHECK THAT 'IMAGE' AND 'VALUE' YIELD " &
                      "CORRECT RESULTS FOR THE LONGEST POSSIBLE " &
                      "ENUMERATION LITERAL");

     -- BIG_ID1 IS A MAXIMUM LENGTH IDENTIFIER. BIG_STRING1 AND 
     -- BIG_STRING2 ARE TWO STRING LITERALS WHICH WHEN CONCATENATED 
     -- FORM THE IMAGE OF BIG_ID1;


     DECLARE
          TYPE ENUM IS (
$BIG_ID1
                        );                             

     BEGIN
          BEGIN
               IF ENUM'VALUE (
$BIG_STRING1  
&
$BIG_STRING2
) /= 
$BIG_ID1
                    THEN
                    FAILED ( "INCORRECT RESULTS FOR 'VALUE'" );
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED FOR 'VALUE'" );
          END;
          BEGIN           
               IF ENUM'IMAGE( 
$BIG_ID1
) /=
(
$BIG_STRING1
&
$BIG_STRING2
)                   THEN
                    FAILED ( "INCORRECT RESULTS FOR 'IMAGE'" );
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FAILED ( "CONSTRAINT_ERROR RAISED FOR 'IMAGE'" );
               WHEN OTHERS =>
                    FAILED ( "OTHER EXCEPTION RAISED FOR 'IMAGE'" );
          END;
     END;

     RESULT;
END C35502D;
