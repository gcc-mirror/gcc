-- CC1221A.ADA

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
--     WITHIN THE GENERIC UNIT:  ASSIGNMENT, MEMBERSHIP, QUALIFICATION,
--     AND EXPLICIT CONVERSION TO AND FROM OTHER INTEGER TYPES.

-- HISTORY:
--     RJW 09/26/86  CREATED ORIGINAL TEST.
--     BCB 11/12/87  CHANGED HEADER TO STANDARD FORMAT.  SPLIT TEST
--                   INTO PARTS A, B, C, AND D.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CC1221A IS

     SUBTYPE SUBINT IS INTEGER RANGE -100 .. 100;
     TYPE NEWINT IS NEW INTEGER;
     TYPE INT IS RANGE -300 .. 300;

BEGIN
     TEST ( "CC1221A", "FOR A FORMAL INTEGER TYPE, CHECK THAT THE " &
                       "FOLLOWING BASIC OPERATIONS ARE IMPLICITLY " &
                       "DECLARED AND ARE THEREFORE AVAILABLE " &
                       "WITHIN THE GENERIC UNIT:  ASSIGNMENT, " &
                       "MEMBERSHIP, QUALIFICATION, AND EXPLICIT " &
                       "CONVERSION TO AND FROM OTHER INTEGER TYPES");

     DECLARE -- (A) CHECKS FOR BASIC OPERATIONS OF A DISCRETE TYPE.
             --     PART I.

          GENERIC
               TYPE T IS RANGE <>;
               TYPE T1 IS RANGE <>;
               I  : T;
               I1 : T1;
          PROCEDURE P (J : T; STR : STRING);

          PROCEDURE P (J : T; STR : STRING) IS
               SUBTYPE ST IS T RANGE T'VAL (-1) .. T'VAL (1);
               K, L  : T;

               FUNCTION F (X : T) RETURN BOOLEAN IS
               BEGIN
                    RETURN IDENT_BOOL (TRUE);
               END F;

               FUNCTION F (X : T1) RETURN BOOLEAN IS
               BEGIN
                    RETURN IDENT_BOOL (FALSE);
               END F;

          BEGIN
               K := I;
               L := J;
               K := L;

               IF K /= J THEN
                    FAILED ( "INCORRECT RESULTS FOR ASSIGNMENT " &
                             "WITH TYPE - " & STR);
               END IF;

               IF I IN ST THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR ""IN"" WITH " &
                             "TYPE  - " & STR);
               END IF;

               IF J NOT IN ST THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR ""NOT IN"" WITH " &
                             "TYPE  - " & STR);
               END IF;

               IF T'(I) /= I THEN
                    FAILED ( "INCORRECT RESULTS FOR QUALIFICATION " &
                             "WITH TYPE - " & STR & " - 1" );
               END IF;

               IF F (T'(1)) THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR QUALIFICATION " &
                             "WITH TYPE - " & STR & " - 2" );
               END IF;

               IF T (I1) /= I THEN
                    FAILED ( "INCORRECT RESULTS FOR EXPLICIT " &
                             "CONVERSION WITH TYPE - " & STR &
                             " - 1" );
               END IF;

               IF F (T (I1)) THEN
                    NULL;
               ELSE
                    FAILED ( "INCORRECT RESULTS FOR EXPLICIT  " &
                             "CONVERSION WITH TYPE - " & STR &
                             " - 2" );
               END IF;

          END P;

          PROCEDURE NP1 IS NEW P (SUBINT,  SUBINT,  0, 0);
          PROCEDURE NP2 IS NEW P (NEWINT,  NEWINT,  0, 0);
          PROCEDURE NP3 IS NEW P (INT,     INT,     0, 0);
          PROCEDURE NP4 IS NEW P (INTEGER, INTEGER, 0, 0);

     BEGIN
          NP1 (2, "SUBINT");
          NP2 (2, "NEWINT");
          NP3 (2, "INT");
          NP4 (2, "INTEGER");
     END; -- (A).

     RESULT;
END CC1221A;
