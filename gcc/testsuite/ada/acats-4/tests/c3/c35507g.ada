-- C35507G.ADA

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
--     CHECK THAT THE ATTRIBUTES 'PRED' AND 'SUCC' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.

-- HISTORY:
--     RJW 06/03/86  CREATED ORIGINAL TEST.
--     JET 08/13/87  REMOVED TESTS INTENDED FOR C35505F.

WITH REPORT; USE REPORT;

PROCEDURE  C35507G  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;

     FUNCTION IDENT (CH : CHAR) RETURN CHAR IS
     BEGIN
          RETURN CHAR'VAL (IDENT_INT (CHAR'POS (CH)));
     END;

     FUNCTION IDENT (CH : NEWCHAR) RETURN NEWCHAR IS
     BEGIN
          RETURN NEWCHAR'VAL (IDENT_INT (NEWCHAR'POS (CH)));
     END;

BEGIN

     TEST( "C35507G" , "CHECK THAT THE ATTRIBUTES 'PRED' AND " &
                       "'SUCC' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE" );

     BEGIN
          IF CHAR'SUCC ('A') /= B THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'SUCC('A')" );
          END IF;

          IF CHAR'PRED (IDENT (B)) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'PRED (IDENT (B))" );
          END IF;
     END;

     BEGIN
          IF NEWCHAR'SUCC (IDENT ('A')) /= B THEN
               FAILED ( "INCORRECT VALUE FOR " &
                        "IDENT (NEWCHAR'SUCC('A'))" );
          END IF;

          IF NEWCHAR'PRED (B) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'PRED(B)" );
          END IF;
     END;

     FOR CH IN CHARACTER'VAL (1) .. CHARACTER'VAL (127) LOOP
          IF CHARACTER'PRED (CH) /=
             CHARACTER'VAL (CHARACTER'POS (CH) - 1) THEN
               FAILED ( "INCORRECT VALUE FOR CHARACTER'PRED OF " &
                         CHARACTER'IMAGE (CH) );
          END IF;
     END LOOP;

     FOR CH IN CHARACTER'VAL (0) .. CHARACTER'VAL (126) LOOP
          IF CHARACTER'SUCC (CH) /=
             CHARACTER'VAL (CHARACTER'POS (CH) + 1) THEN
               FAILED ( "INCORRECT VALUE FOR CHARACTER'SUCC OF " &
                         CHARACTER'IMAGE (CH) );
          END IF;
     END LOOP;

     RESULT;

END C35507G;
