-- CC1221C.ADA

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
--     FOR A FORMAL INTEGER TYPE, CHECK THAT THE FOLLOWING BASIC
--     OPERATIONS ARE IMPLICITLY DECLARED AND ARE THEREFORE AVAILABLE
--     WITHIN THE GENERIC UNIT:  ATTRIBUTES 'POS, 'VAL, 'PRED, 'SUCC,
--     'IMAGE, AND 'VALUE.

-- HISTORY:
--     BCB 11/12/87  CREATED ORIGINAL TEST FROM SPLIT OF CC1221A.ADA

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CC1221C IS

     SUBTYPE SUBINT IS INTEGER RANGE -100 .. 100;
     TYPE NEWINT IS NEW INTEGER;
     TYPE INT IS RANGE -300 .. 300;
     SUBTYPE SINT1 IS INT
          RANGE INT (IDENT_INT (-4)) .. INT (IDENT_INT (4));
     TYPE INT1 IS RANGE -6 .. 6;

BEGIN
     TEST ( "CC1221C", "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
                       "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
                       "DECLARED AND ARE THEREFORE AVAILABLE " &
                       "WITHIN THE GENERIC UNIT:  ATTRIBUTES 'POS, " &
                       "'VAL, 'PRED, 'SUCC, 'IMAGE, AND 'VALUE");

     DECLARE -- (C1) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
             --      PART III.

          GENERIC
               TYPE T IS RANGE <>;
               F : INTEGER;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               I : INTEGER;
               Y : T;

               FUNCTION IDENT (X : T) RETURN T IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN X;
                    ELSE
                         RETURN T'SUCC (T'FIRST);
                    END IF;
               END IDENT;

          BEGIN
               I := F;
               FOR X IN T LOOP
                    IF T'VAL (I) /= X THEN
                         FAILED ( "WRONG VALUE FOR " & STR &
                                  "'VAL OF " & INTEGER'IMAGE (I));
                    END IF;

                    IF T'POS (X) /= I THEN
                         FAILED ( "WRONG VALUE FOR " & STR &
                                  "'POS OF " & T'IMAGE (X));
                    END IF;

                    I := I + 1;
               END LOOP;

               FOR X IN T LOOP
                    IF T'SUCC (X) /= T'VAL (T'POS (X) + 1) THEN
                         FAILED ( "WRONG VALUE FOR " & STR &
                                  "'SUCC OF " & T'IMAGE (X));
                    END IF;

                    IF T'PRED (X) /= T'VAL (T'POS (X) - 1) THEN
                         FAILED ( "WRONG VALUE FOR " & STR &
                                  "'PRED OF " & T'IMAGE (X));
                    END IF;
               END LOOP;

               BEGIN
                    Y := T'SUCC (IDENT (T'BASE'LAST));
                    FAILED ( "NO EXCEPTION RAISED FOR " &
                              STR & "'SUCC (IDENT (" & STR &
                             "'BASE'LAST))" );
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                   STR & "'SUCC (IDENT (" & STR &
                                  "'BASE'LAST))" );
               END;

               BEGIN
                    Y := T'PRED (IDENT (T'BASE'FIRST));
                    FAILED ( "NO EXCEPTION RAISED FOR " &
                              STR & "'PRED (IDENT (" & STR &
                             "'BASE'FIRST))" );
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                   STR & "'PRED (IDENT (" & STR &
                                  "'BASE'FIRST))" );
               END;

          END P;

          PROCEDURE P1 IS NEW P (SUBINT, -100);
          PROCEDURE P2 IS NEW P (SINT1, -4);
          PROCEDURE P3 IS NEW P (INT1, -6);

     BEGIN
           P1 ( "SUBINT" );
           P2 ( "SINT" );
           P3 ( "INT1" );
     END; -- (C1).

     DECLARE -- (C2) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
             --      PART IV.

          GENERIC
               TYPE T IS RANGE <>;
               STR : STRING;
          PACKAGE PKG IS END PKG;

          PACKAGE BODY PKG IS
               PROCEDURE P (IM : STRING; VA : T) IS
               BEGIN
                    IF T'IMAGE (VA) /= IM THEN
                         FAILED ( "INCORRECT RESULTS FOR " & STR &
                                  "'IMAGE OF " &
                                   INTEGER'IMAGE (INTEGER (VA)));
                    END IF;
               END P;

               PROCEDURE Q (IM : STRING; VA : T) IS
               BEGIN
                    IF T'VALUE (IM) /= VA THEN
                         FAILED ( "INCORRECT RESULTS FOR " & STR &
                                  "'VALUE OF " & IM);
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         FAILED ( "CONSTRAINT_ERROR RAISED FOR " &
                                   STR &"'VALUE OF " & IM);
                    WHEN OTHERS =>
                         FAILED ( "OTHER EXCEPTION RAISED FOR " &
                                   STR &"'VALUE OF " & IM);

               END Q;

          BEGIN
               P (" 2", 2);
               P ("-1", -1);

               Q (" 2", 2);
               Q ("-1", -1);
               Q ("        2", 2);
               Q ("-1     ", -1);
          END PKG;

          PACKAGE PKG1 IS NEW PKG (SUBINT, "SUBINT");
          PACKAGE PKG2 IS NEW PKG (SINT1, "SINT1");
          PACKAGE PKG3 IS NEW PKG (INT1, "INT1");
          PACKAGE PKG4 IS NEW PKG (NEWINT, "NEWINT");

     BEGIN
          NULL;
     END; -- (C2).

     RESULT;
END CC1221C;
