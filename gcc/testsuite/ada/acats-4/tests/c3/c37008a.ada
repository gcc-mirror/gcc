-- C37008A.ADA

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
-- CHECK THAT SPECIFYING AN INVALID DEFAULT INITIALIZATION
-- RAISES CONSTRAINT_ERROR WHEN AN OBJECT IS DECLARED.

-- DAT 3/6/81
-- SPS 10/26/82
-- RJW 1/9/86 -  REVISED COMMENTS. ADDED 'IDENT_INT'.
-- EDS 7/22/98   AVOID OPTIMIZATION

WITH REPORT;
USE REPORT;
PROCEDURE C37008A IS
BEGIN
     TEST ("C37008A", "CHECK THAT INVALID DEFAULT RECORD"
                    & " COMPONENT INITIALIZATIONS RAISE"
                    & " CONSTRAINT_ERROR");

     BEGIN
          DECLARE
               TYPE R1 IS RECORD
                    C1 : INTEGER RANGE 1 .. 5 := IDENT_INT (0);
               END RECORD;
               REC1 : R1;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 1 " & INTEGER'IMAGE(REC1.C1));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 1");
     END;

     BEGIN
          DECLARE
               TYPE R IS RECORD
                    C : CHARACTER RANGE 'A' .. 'Y' := 'Z';
               END RECORD;
               REC2 : R;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 1A " & (REC2.C));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 1A");
     END;

     BEGIN
          DECLARE
               TYPE R2 IS RECORD
                    C2 : BOOLEAN RANGE FALSE .. FALSE := TRUE;
               END RECORD;
               REC3 : R2;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 2 " & BOOLEAN'IMAGE(REC3.C2));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 2");
     END;

     BEGIN
          DECLARE
               TYPE E IS (E1, E2, E3);
               TYPE R IS RECORD
                    C : E RANGE E2 .. E3 := E1;
               END RECORD;
               REC4 : R;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 2A " & E'IMAGE(REC4.C));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 2A");
     END;

     BEGIN
          DECLARE
               TYPE R3 IS RECORD
                    C3 : INTEGER RANGE 1 .. 5;
               END RECORD;
               REC5 : R3;
               TYPE R3A IS RECORD
                    C3A : R3 := (OTHERS => IDENT_INT (6));
               END RECORD;
               REC6 : R3A;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 3 " & 
                       INTEGER'IMAGE(REC6.C3A.C3));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 3");
     END;

     BEGIN
          DECLARE
               TYPE ARR IS ARRAY (1..3) OF INTEGER RANGE 8..9;
               TYPE R4 IS RECORD
                    C4 : ARR
                         := (1 => 8, 2 => 9, 3 => 10);
               END RECORD;
               REC7 : R4;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 4 " & 
                       INTEGER'IMAGE(REC7.C4(1)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 4");
     END;

     BEGIN
          DECLARE
               TYPE A IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 1 .. 5;

               TYPE AA IS ACCESS A;

               TYPE R5 IS RECORD
                    C5 : AA := NEW A' (4, 5, 6);
               END RECORD;
               REC8 : R5;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 5 " & 
                       INTEGER'IMAGE(REC8.C5(1)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 5");
     END;

     BEGIN
          DECLARE
               TYPE A IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 1 .. 5;

               TYPE AA IS ACCESS A (1 .. 3);

               TYPE R6 IS RECORD
                    C6 : AA := NEW A' (4, 4, 4, 4);
               END RECORD;
               REC9 : R6;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 6 " & 
                       INTEGER'IMAGE(REC9.C6(1)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 6");
     END;

     BEGIN
          DECLARE
               TYPE AI IS ACCESS INTEGER RANGE 6 .. 8;

               TYPE R7 IS RECORD
                    C7 : AI := NEW INTEGER' (5);
               END RECORD;
               REC10 : R7;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 7 " & 
                       INTEGER'IMAGE(REC10.C7.ALL));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 7");
     END;

     BEGIN
          DECLARE
               TYPE UA IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 3 .. 5;

               SUBTYPE CA IS UA (7 .. 8);

               TYPE R8 IS RECORD
                    C8 : CA := (6 .. 8 => 4);
               END RECORD;
               REC11 : R8;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 8 " & 
                       INTEGER'IMAGE(REC11.C8(7)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 8");
     END;

     BEGIN
          DECLARE
               TYPE UA IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 3 .. IDENT_INT(5);

               TYPE R9 IS RECORD
                    C9 : UA (11 .. 11) := (11 => 6);
               END RECORD;
               REC12 : R9;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 9 " & 
                       INTEGER'IMAGE(REC12.C9(11)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 9");
     END;

     BEGIN
          DECLARE
               TYPE A IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 1 .. IDENT_INT (5);

               TYPE AA IS ACCESS A;

               TYPE R10 IS RECORD
                    C10 : AA := NEW A '(4, 5, 6);
               END RECORD;
               REC13 : R10;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 10 " & 
                       INTEGER'IMAGE(REC13.C10(1)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 10");
     END;

     BEGIN
          DECLARE
               TYPE A IS ARRAY (NATURAL RANGE <> )
                    OF INTEGER RANGE 1 .. 5;

               TYPE AA IS ACCESS A (IDENT_INT (1) .. IDENT_INT (3));

               TYPE R11 IS RECORD
                    C11 : AA := NEW A '(4, 4, 4, 4);
               END RECORD;
               REC14 : R11;
          BEGIN
               FAILED ("NO EXCEPTION RAISED 11 " & 
                       INTEGER'IMAGE(REC14.C11(1)));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 11");
     END;

     RESULT;
END C37008A;
