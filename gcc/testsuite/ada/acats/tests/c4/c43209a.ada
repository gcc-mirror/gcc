-- C43209A.ADA

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
--     CHECK THAT A STRING LITERAL IS ALLOWED IN A MULTIDIMENSIONAL
--     ARRAY AGGREGATE AT THE PLACE OF A ONE DIMENSIONAL ARRAY OF
--     CHARACTER TYPE.

-- HISTORY:
--     DHH 08/12/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43209A IS

     TYPE MULTI_ARRAY IS ARRAY(1 .. 2, 1 .. 3, 1 .. 6) OF CHARACTER;

BEGIN
     TEST("C43209A", "CHECK THAT A STRING LITERAL IS ALLOWED IN A " &
                     "MULTIDIMENSIONAL ARRAY AGGREGATE AT THE PLACE " &
                     "OF A ONE DIMENSIONAL ARRAY OF CHARACTER TYPE");

     DECLARE
          X : MULTI_ARRAY := ((('A', 'B', 'C', 'D', 'E', 'F'),
                              ('G', 'H', 'I', 'J', 'K', 'L'),
                              ('M', 'N', 'O', 'P', 'Q', 'R')),
                              (('S', 'T', 'U', 'V', 'W', 'X'),
                              ('W', 'Z', 'A', 'B', 'C', 'D'),
                              "WHOZAT"));

          Y : MULTI_ARRAY := (("WHOZAT",
                              ('A', 'B', 'C', 'D', 'E', 'F'),
                              ('G', 'H', 'I', 'J', 'K', 'L')),
                              (('M', 'N', 'O', 'P', 'Q', 'R'),
                              ('S', 'T', 'U', 'V', 'W', 'X'),
                              ('W', 'Z', 'A', 'B', 'C', 'D')));

     BEGIN
          IF X(IDENT_INT(2), IDENT_INT(3), IDENT_INT(6)) /=
                  Y(IDENT_INT(1), IDENT_INT(1), IDENT_INT(6)) THEN
               FAILED("INITIALIZATION FAILURE");
          END IF;
     END;

     DECLARE
          PROCEDURE FIX_AGG(T : MULTI_ARRAY) IS
          BEGIN
               IF T(IDENT_INT(2), IDENT_INT(2), IDENT_INT(5)) /=
                       T(IDENT_INT(1), IDENT_INT(1), IDENT_INT(1)) THEN
                    FAILED("SUBPROGRAM FAILURE");
               END IF;
          END;
     BEGIN
          FIX_AGG((("WHOZAT", ('A', 'B', 'C', 'D', 'E', 'F'),
                              ('G', 'H', 'I', 'J', 'K', 'L')),
                              (('M', 'N', 'O', 'P', 'Q', 'R'),
                              ('S', 'T', 'U', 'V', 'W', 'X'),
                              ('W', 'Z', 'A', 'B', 'C', 'D'))));

     END;

     DECLARE

          Y : CONSTANT MULTI_ARRAY := (("WHOZAT",
                              ('A', 'B', 'C', 'D', 'E', 'F'),
                              ('G', 'H', 'I', 'J', 'K', 'L')),
                              (('M', 'N', 'O', 'P', 'Q', 'R'),
                              ('S', 'T', 'U', 'V', 'W', 'X'),
                              ('W', 'Z', 'A', 'B', 'C', 'D')));

     BEGIN
          IF Y(IDENT_INT(2), IDENT_INT(2), IDENT_INT(5)) /=
             Y(IDENT_INT(1), IDENT_INT(1), IDENT_INT(1)) THEN
               FAILED("CONSTANT FAILURE");
          END IF;
     END;

     DECLARE
     BEGIN
          IF MULTI_ARRAY'((1 =>(('A', 'B', 'C', 'D', 'E', 'F'),
               ('G', 'H', 'I', 'J', 'K', 'L'),
               ('M', 'N', 'O', 'P', 'Q', 'R')),
              2 => (('S', 'T', 'U', 'V', 'W', 'X'),
               ('W', 'Z', 'A', 'B', 'C', 'D'),
               "WHOZAT"))) = MULTI_ARRAY'((1 =>(1 =>"WHOZAT",
                             2 =>('A', 'B', 'C', 'D', 'E', 'F'),
                             3 =>('G', 'H', 'I', 'J', 'K', 'L')),
                            2 => (1 =>('M', 'N', 'O', 'P', 'Q', 'R'),
                             2 =>('S', 'T', 'U', 'V', 'W', 'X'),
                             3 => ('W', 'Z', 'A', 'B', 'C', 'D')))) THEN
               FAILED("EQUALITY OPERATOR FAILURE");
          END IF;
     END;

     DECLARE
          SUBTYPE SM IS INTEGER RANGE 1 .. 10;
          TYPE UNCONSTR IS ARRAY(SM RANGE <>, SM RANGE<>) OF CHARACTER;

          FUNCTION FUNC(X : SM) RETURN UNCONSTR IS
          BEGIN
               IF EQUAL(X,X) THEN
                    RETURN (1 => "WHEN", 2 => "WHAT");
               ELSE
                    RETURN ("    ", "    ");
               END IF;
          END FUNC;

     BEGIN
          IF FUNC(1) /= FUNC(2) THEN
               FAILED("UNCONSTRAINED FUNCTION RETURN FAILURE");
          END IF;
     END;

     RESULT;
END C43209A;
