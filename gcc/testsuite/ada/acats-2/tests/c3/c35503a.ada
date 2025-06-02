-- C35503A.ADA

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
-- CHECK THAT 'WIDTH' YIELDS THE CORRECT RESULT WHEN THE PREFIX IS AN 
-- INTEGER TYPE.

-- RJW 3/12/86

WITH REPORT; USE REPORT;

PROCEDURE C35503A IS

BEGIN
     TEST ("C35503A", "CHECK THAT 'WIDTH' YIELDS THE CORRECT " &
                      "RESULT WHEN THE PREFIX IS AN INTEGER TYPE" );

     DECLARE
          SUBTYPE SINTEGER IS INTEGER;
          
          TYPE INT IS RANGE -1000 .. 1000;
          TYPE INT2 IS NEW INT RANGE 1E2 .. 1E2;

          SUBTYPE SINT1 IS INT RANGE 00000 .. 100;
          SUBTYPE SINT2 IS INT RANGE 16#E#E1 .. 2#1111_1111#;
          SUBTYPE SINT3 IS INT RANGE -100 .. 9;
          SUBTYPE NOINT IS INT RANGE 1 .. -1;

     BEGIN
          IF IDENT_INT(SINTEGER'WIDTH) /= INTEGER'WIDTH THEN
               FAILED ( "WRONG WIDTH FOR 'SINTEGER'" );
          END IF;

          IF IDENT_INT(INT'WIDTH) /= 5 THEN
               FAILED ( "WRONG WIDTH FOR 'INT'" );
          END IF;

          IF IDENT_INT(INT2'WIDTH) /= 4 THEN
               FAILED ( "WRONG WIDTH FOR 'INT2'");
          END IF;

          IF IDENT_INT(SINT1'WIDTH) /= 4 THEN 
               FAILED ( "WRONG WIDTH FOR 'SINT1'" );
          END IF;

          IF IDENT_INT(SINT2'WIDTH) /= 4 THEN 
               FAILED ( "WRONG WIDTH FOR 'SINT2'" );
          END IF;

          IF IDENT_INT(SINT3'WIDTH) /= 4 THEN
               FAILED ( "WRONG WIDTH FOR 'SINT3'" );
          END IF;

          IF IDENT_INT(NOINT'WIDTH) /= 0 THEN
               FAILED ( "WRONG WIDTH FOR 'NOINT'" );
          END IF;
     END;

     RESULT;
END C35503A;
