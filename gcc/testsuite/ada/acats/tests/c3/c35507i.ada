-- C35507I.ADA

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
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE WITH AN ENUMERATION
--     REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 06/03/86  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     DTN 11/26/91  DELETED CONSTRAINT_ERROR FOR ATTRIBUTES PRED AND
--                   SUCC SUBTESTS.

WITH REPORT; USE REPORT;

PROCEDURE  C35507I  IS

     TYPE CHAR IS ('A', B);
     FOR CHAR USE ('A' => 2, B => 5);

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

     TEST( "C35507I" , "CHECK THAT THE ATTRIBUTES 'PRED' AND " &
                       "'SUCC' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE WITH AN " &
                       "ENUMERATION REPRESENTATION CLAUSE" );

     BEGIN
          IF CHAR'SUCC ('A') /= B THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'SUCC('A')" );
          END IF;

          IF CHAR'PRED (IDENT (B)) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'PRED (IDENT (B))" );
          END IF;
     END;

     BEGIN
          IF IDENT (NEWCHAR'SUCC ('A')) /= B THEN
               FAILED ( "INCORRECT VALUE FOR " &
                        "IDENT (NEWCHAR'SUCC('A'))" );
          END IF;

          IF NEWCHAR'PRED (B) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'PRED(B)" );
          END IF;
     END;

     RESULT;
END C35507I;
