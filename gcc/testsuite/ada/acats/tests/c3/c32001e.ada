-- C32001E.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR PRIVATE TYPES, THE 
-- SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED
-- ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE 
-- INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE EVALUATIONS 
-- YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT DECLARATIONS.

-- RJW 7/18/86

WITH REPORT; USE REPORT;

PROCEDURE C32001E IS

     BUMP : ARRAY (1 .. 10) OF INTEGER := (OTHERS => 0);
     G1 : ARRAY (5 .. 6) OF INTEGER;

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          RETURN BUMP (I);
     END F;

     FUNCTION G (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          G1 (I) := BUMP (I);
          RETURN BUMP (I);
     END G;

BEGIN
     TEST ("C32001E", "CHECK THAT IN MULTIPLE OBJECT DECLARATIONS " & 
                      "FOR PRIVATE TYPES, THE SUBTYPE INDICATION " & 
                      "AND THE INITIALIZATION EXPRESSIONS ARE " &
                      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
                      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
                      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
                      "EVALUATIONS YIELD THE SAME RESULT AS A " &
                      "SEQUENCE OF SINGLE OBJECT DECLARATIONS" );
     
     DECLARE
          PACKAGE PKG1 IS
               TYPE PBOOL IS PRIVATE;
               TYPE PINT IS PRIVATE;
               TYPE PREC (D : INTEGER) IS PRIVATE;
               TYPE PARR IS PRIVATE;
               TYPE PACC IS PRIVATE;
               
               FUNCTION INIT1 (I : INTEGER) RETURN PBOOL;
               FUNCTION INIT2 (I : INTEGER) RETURN PINT;
               FUNCTION INIT3 (I : INTEGER) RETURN PREC;
               FUNCTION INIT4 (I : INTEGER) RETURN PARR;
               FUNCTION INIT5 (I : INTEGER) RETURN PACC;

               PROCEDURE CHECK1 (B : PBOOL; I : INTEGER; S : STRING);
               PROCEDURE CHECK2 (I : PINT;  J : INTEGER; S : STRING);
               PROCEDURE CHECK3 (R : PREC;  I, J : INTEGER; 
                                 S : STRING);
               PROCEDURE CHECK4 (A : PARR;  I, J : INTEGER; 
                                 S : STRING);
               PROCEDURE CHECK5 (V : PACC; S : STRING);
               PROCEDURE CHECK6 (V : PACC; S : STRING);

          PRIVATE
               TYPE PBOOL IS NEW BOOLEAN;
               TYPE PINT IS NEW INTEGER;

               TYPE PREC (D : INTEGER) IS 
                    RECORD 
                         VALUE : INTEGER;
                    END RECORD;

               TYPE PARR IS ARRAY (1 .. 2) OF INTEGER;

               TYPE VECTOR IS ARRAY (NATURAL RANGE <>) OF INTEGER;
               TYPE PACC IS ACCESS VECTOR;
          END PKG1;

          PACKAGE BODY PKG1 IS
               FUNCTION INIT1 (I : INTEGER) RETURN PBOOL IS
               BEGIN
                    RETURN PBOOL'VAL (F (I) - 1);
               END INIT1;

               FUNCTION INIT2 (I : INTEGER) RETURN PINT IS
               BEGIN
                    RETURN PINT'VAL (F (I));
               END INIT2;

               FUNCTION INIT3 (I : INTEGER) RETURN PREC IS
                    PR : PREC (G1 (I)) := (G1 (I), F (I));
               BEGIN
                    RETURN PR;
               END INIT3;

               FUNCTION INIT4 (I : INTEGER) RETURN PARR IS
                    PA : PARR := (1 .. 2 => F (I));
               BEGIN
                    RETURN PA;
               END INIT4;

               FUNCTION INIT5 (I : INTEGER) RETURN PACC IS
                    ACCV : PACC := NEW VECTOR'(1 .. F (I) => F (I));
               BEGIN
                    RETURN ACCV;
               END INIT5;

               PROCEDURE CHECK1 (B : PBOOL; I : INTEGER; S : STRING) IS
               BEGIN
                    IF B /= PBOOL'VAL (I) THEN 
                         FAILED ( S & " HAS AN INCORRECT VALUE OF " &
                                  PBOOL'IMAGE (B));
                    END IF;
               END CHECK1;

               PROCEDURE CHECK2 (I : PINT; J : INTEGER; S : STRING) IS 
               BEGIN
                    IF I /= PINT'VAL (J) THEN 
                         FAILED ( S & " HAS AN INCORRECT VALUE OF " &
                                  PINT'IMAGE (I));
                    END IF;
               END CHECK2;

               PROCEDURE CHECK3 (R : PREC; I, J : INTEGER; 
                                 S : STRING) IS
               BEGIN
                    IF R.D /= I THEN 
                         FAILED ( S & ".D HAS AN INCORRECT VALUE OF " 
                                  & INTEGER'IMAGE (R.D));
                    END IF;

                    IF R.VALUE /= J THEN
                         FAILED ( S & ".VALUE HAS AN INCORRECT " &
                                  "VALUE OF " & 
                                  INTEGER'IMAGE (R.VALUE));
                    END IF;
               END CHECK3;

               PROCEDURE CHECK4 (A : PARR; I, J : INTEGER; 
                                 S : STRING) IS
               BEGIN
                    IF A /= (I, J) AND A /= (J, I) THEN
                         FAILED ( S & " HAS AN INCORRECT VALUE" );
                    END IF;
               END CHECK4;

               PROCEDURE CHECK5 (V : PACC; S : STRING) IS
               BEGIN
                    IF V'LAST /= 1 THEN
                         FAILED ( S & " HAS AN INCORRECT UPPER BOUND " 
                                  & "OF " & INTEGER'IMAGE (V'LAST));
                    END IF;

                    IF V (1) /= 2 THEN 
                         FAILED ( S & " HAS AN INCORRECT COMPONENT " &
                                  "VALUE" );
                    END IF;
               END CHECK5;
               
               PROCEDURE CHECK6 (V : PACC; S : STRING) IS
               BEGIN
                    IF V'LAST /= 3 THEN
                         FAILED ( S & " HAS AN INCORRECT UPPER BOUND " 
                                  & "OF " & INTEGER'IMAGE (V'LAST));
                    END IF;

                    IF V.ALL = (4, 5, 6) OR V.ALL = (5, 4, 6) OR 
                       V.ALL = (4, 6, 5) OR V.ALL = (6, 4, 5) OR
                       V.ALL = (5, 6, 4) OR V.ALL = (6, 5, 4) THEN
                         NULL;
                    ELSE
                         FAILED ( S & " HAS AN INCORRECT COMPONENT " &
                                  "VALUE" );
                    END IF;
               END CHECK6;

          END PKG1;
          
          PACKAGE PKG2 IS END PKG2;
     
          PACKAGE BODY PKG2 IS
               USE PKG1;

               B1, B2   : PBOOL := INIT1 (1);
               CB1, CB2 : CONSTANT PBOOL := INIT1 (2);
            
               I1, I2   : PINT := INIT2 (3);
               CI1, CI2 : CONSTANT PINT := INIT2 (4);

               R1, R2   : PREC (G (5)) := INIT3 (5);
               CR1, CR2 : CONSTANT PREC (G (6)) := INIT3 (6);

               A1, A2   : PARR := INIT4 (7);
               CA1, CA2 : CONSTANT PARR := INIT4 (8);

               V1, V2   : PACC  := INIT5 (9);
               CV1, CV2 : CONSTANT PACC  := INIT5 (10);

          BEGIN
               CHECK1 (B1, 0, "B1");
               CHECK1 (B2, 1, "B2");
               CHECK1 (CB1, 0, "CB1");
               CHECK1 (CB2, 1, "CB2");

               CHECK2 (I1, 1, "I1");
               CHECK2 (I2, 2, "I2");
               CHECK2 (CI1, 1, "CI1");
               CHECK2 (CI2, 2, "CI2");

               CHECK3 (R1, 1, 2, "R1");
               CHECK3 (R2, 3, 4, "R2");
               CHECK3 (CR1, 1, 2, "CR1");
               CHECK3 (CR2, 3, 4, "CR2");

               CHECK4 (A1, 1, 2, "A1");
               CHECK4 (A2, 3, 4, "A2");
               CHECK4 (CA1, 1, 2, "CA1");
               CHECK4 (CA2, 3, 4, "CA2");

               CHECK5 (V1, "V1");
               CHECK6 (V2, "V2");
               CHECK5 (CV1, "CV1");
               CHECK6 (CV2, "CV2");
      END PKG2;

     BEGIN
          NULL;          
     END;

     RESULT;
END C32001E;
