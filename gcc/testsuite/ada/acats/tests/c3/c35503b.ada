-- C35503B.ADA

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
-- CHECK THAT 'WIDTH' YIELDS THE CORRECT RESULT WHEN THE PREFIX IS A
-- GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS AN INTEGER 
-- TYPE.

-- RJW 3/17/86

WITH REPORT; USE REPORT;

PROCEDURE C35503B IS

BEGIN
     TEST ("C35503B", "CHECK THAT 'WIDTH' YIELDS THE CORRECT " &
                      "RESULT WHEN THE PREFIX IS A GENERIC FORMAL "  &
                      "DISCRETE TYPE WHOSE ACTUAL PARAMETER IS AN " &
                      "INTEGER TYPE" );

     DECLARE

          TYPE INT IS RANGE -1000 .. 1000;
          TYPE INT2 IS NEW INT RANGE 0E8 .. 1E3;
          SUBTYPE SINT1 IS INT RANGE 00000 .. 300;
          SUBTYPE SINT2 IS INT RANGE 16#E#E1 .. 2#1111_1111#;

          GENERIC
               TYPE I IS (<>);
               W : INTEGER;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE SUBI IS I 
                    RANGE I'VAL (IDENT_INT(224)) .. I'VAL (255);
               SUBTYPE NORANGE IS I 
                    RANGE I'VAL (255) .. I'VAL (IDENT_INT(224));
          BEGIN
               IF IDENT_INT(I'WIDTH) /= W THEN 
                    FAILED ( "INCORRECT I'WIDTH FOR " & STR );
               END IF;

               IF IDENT_INT(SUBI'WIDTH) /= 4 THEN
                    FAILED ( "INCORRECT SUBI'WIDTH FOR " & STR );
               END IF;

               IF IDENT_INT(NORANGE'WIDTH) /= 0 THEN
                    FAILED ( "INCORRECT NORANGE'WIDTH FOR " & STR );
               END IF;
          END P;

          PROCEDURE P_INTEGER IS NEW P (INTEGER, INTEGER'WIDTH);
          PROCEDURE P_INT     IS NEW P (INT, 5);
          PROCEDURE P_INT2    IS NEW P (INT2, 5);
          PROCEDURE P_SINT1   IS NEW P (SINT1, 4);
          PROCEDURE P_SINT2   IS NEW P (SINT2, 4);

     BEGIN
          P_INTEGER ("'INTEGER'");
          P_INT     ("'INT'");
          P_INT2    ("'INT2'");
          P_SINT1   ("'SINT1'");
          P_SINT2   ("'SINT2'");
     END;

     RESULT;
END C35503B;
