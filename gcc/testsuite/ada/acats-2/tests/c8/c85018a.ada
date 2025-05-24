-- C85018A.ADA

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
-- CHECK THAT AN ENTRY FAMILY MEMBER CAN BE RENAMED WITH:
--           1) DIFFERENT PARAMETER NAMES;
--           2) DIFFERENT DEFAULT VALUES;
--      AND THAT THE NEW NAMES/DEFAULTS ARE USED WHEN THE NEW NAME
--      IS USED IN A CALL.

-- RJW 6/3/86

WITH REPORT; USE REPORT;

PROCEDURE C85018A IS

BEGIN

     TEST( "C85018A", "CHECK THAT AN ENTRY FAMILY MEMBER CAN BE " &
                      "RENAMED AND THAT THE NEW NAMES/DEFAULTS ARE "  &
                      "THOSE ASSOCIATED WITH THE RENAMED ENTITY" );

     DECLARE

          RESULTS : INTEGER;

          TYPE TA IS ARRAY(1 .. 5) OF INTEGER;

          TASK T IS 
               ENTRY ENT1 (BOOLEAN)
                    (A : INTEGER := 1; B : TA := (1 .. 5 => 1));
          END T;

          PROCEDURE ENTA (C : INTEGER := 1; D : TA := (1 .. 5 => 1))
                          RENAMES T.ENT1 (TRUE);

          PROCEDURE ENTB (B : INTEGER := 1; A : TA := (1 .. 5 => 1))
                          RENAMES T.ENT1 (TRUE);

          PROCEDURE ENTC (A : INTEGER := 2; B : TA := (1, 2, 3, 4, 5))
                          RENAMES T.ENT1 (TRUE);

          PROCEDURE ENTD (C : INTEGER := 2; D : TA := (1, 2, 3, 4, 5))
                          RENAMES T.ENT1 (TRUE);

          TASK BODY T IS 
          BEGIN
               LOOP
                    SELECT
                         ACCEPT ENT1 (IDENT_BOOL (TRUE)) 
                                     (A : INTEGER := 1; 
                                      B : TA := (1 .. 5 => 1)) DO
                              IF A IN 1 .. 5 THEN
                                   RESULTS := B(A);
                              ELSE
                                   RESULTS := 0;
                              END IF;
                         END;
                    OR 
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END T;

     BEGIN

          T.ENT1 (TRUE);
          IF RESULTS /= 1 THEN
               FAILED ( "PARAMETERS NOT PROPERLY INITIALIZED" );
          END IF;

          T.ENT1 (TRUE) (A => 6);
          IF RESULTS /= 0 THEN
               FAILED ( "INCORRECT RESULTS" );
          END IF;

          ENTA;
          IF RESULTS /= 1 THEN
               FAILED ( "CASE 1 : INCORRECT RESULTS (DEFAULT)" );
          END IF;

          ENTA(D => (5, 4, 3, 2, 1));
          IF RESULTS /= 5 THEN
               FAILED ( "CASE 1 : INCORRECT RESULTS" );
          END IF;

          ENTB;
          IF RESULTS /= 1 THEN
               FAILED ( "CASE 1 : INCORRECT RESULTS (DEFAULT)" );
          END IF;

          ENTB(A => (5, 4, 3, 2, 1), B => 2);
          IF RESULTS /= 4 THEN
               FAILED ( "CASE 1 : INCORRECT RESULTS " );
          END IF;

          ENTC;
          IF RESULTS /= 2 THEN
               FAILED ( "CASE 2 : INCORRECT RESULTS (DEFAULT)" );
          END IF;

          ENTC(3);
          IF RESULTS /= 3 THEN
               FAILED ( "CASE 2 : INCORRECT RESULTS " );
          END IF;

          ENTD;
          IF RESULTS /= 2 THEN
               FAILED ( "CASE 2 : INCORRECT RESULTS (DEFAULT)" );
          END IF;

          ENTD(4);
          IF RESULTS /= 4 THEN
               FAILED ( "CASE 2 : INCORRECT RESULTS " );
          END IF;

     END;
     RESULT;

END C85018A;
