-- C35507O.ADA

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
-- CHECK THAT THE ATTRIBUTES 'FIRST' AND 'LAST' YIELD THE CORRECT 
-- RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.   

-- RJW 6/03/86
-- PWN 11/30/94 SUBTYPE QUALIFIED LITERALS FOR ADA 9X.
--              REMOVED PART OF TEST INVALID FOR ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE  C35507O  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;     

     SPACE : CONSTANT CHARACTER := CHARACTER'(' ');

     SUBTYPE NOCHAR IS CHARACTER RANGE CHARACTER'('Z') .. CHARACTER'('A');
     SUBTYPE GRAPHIC IS CHARACTER RANGE SPACE .. ASCII.TILDE;
     SUBTYPE NONGRAPHIC IS CHARACTER RANGE ASCII.NUL .. ASCII.US;

     FUNCTION IDENT (CH : CHAR) RETURN CHAR IS
     BEGIN
          RETURN CHAR'VAL (IDENT_INT (CHAR'POS (CH)));
     END IDENT;

     FUNCTION IDENT (CH : NEWCHAR) RETURN NEWCHAR IS
     BEGIN
          RETURN NEWCHAR'VAL (IDENT_INT (NEWCHAR'POS (CH)));
     END IDENT;

BEGIN

     TEST( "C35507O" , "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
                       "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE" );

     BEGIN 
          IF IDENT (CHAR'FIRST) /= 'A' THEN 
               FAILED ( "INCORRECT VALUE FOR CHAR'FIRST" );
          END IF;

          IF CHAR'LAST /= B THEN 
                FAILED ( "INCORRECT VALUE FOR CHAR'LAST" );
          END IF;
     END;

     BEGIN 
          IF NEWCHAR'FIRST /= 'A' THEN 
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'FIRST" );
          END IF;

          IF NEWCHAR'LAST /= IDENT (B) THEN 
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'LAST" );
          END IF;
     END;

     BEGIN 
          IF NOCHAR'FIRST /= CHARACTER'('Z') THEN 
               FAILED ( "INCORRECT VALUE FOR NOCHAR'FIRST" );
          END IF;

          IF NOCHAR'LAST /= CHARACTER'('A') THEN 
               FAILED ( "INCORRECT VALUE FOR NOCHAR'LAST" );
          END IF;
     END;

     BEGIN 
          IF CHARACTER'FIRST /= ASCII.NUL THEN 
               FAILED ( "INCORRECT VALUE FOR CHARACTER'FIRST" );
          END IF;

     END;

     BEGIN 
          IF NONGRAPHIC'FIRST /= IDENT_CHAR (ASCII.NUL) THEN 
               FAILED ( "INCORRECT VALUE FOR NONGRAPHIC'FIRST" );
          END IF;

          IF NONGRAPHIC'LAST /= ASCII.US THEN 
               FAILED ( "INCORRECT VALUE FOR NONGRAPHIC'LAST" );
          END IF;
     END;

     BEGIN 
          IF GRAPHIC'FIRST /= SPACE THEN 
               FAILED ( "INCORRECT VALUE FOR GRAPHIC'FIRST" );
          END IF;

          IF GRAPHIC'LAST /= ASCII.TILDE THEN 
               FAILED ( "INCORRECT VALUE FOR GRAPHIC'LAST" );
          END IF;
     END;

     RESULT;
END C35507O;
