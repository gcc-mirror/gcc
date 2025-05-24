-- C36180A.ADA

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
--     CHECK THAT AN INDEX CONSTRAINT CAN HAVE THE FORM A'RANGE,
--     WHERE A IS A PREVIOUSLY DECLARED ARRAY OBJECT OR CONSTRAINED
--     ARRAY SUBTYPE.

-- HISTORY:
--     BCB 01/21/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C36180A IS

     TYPE J IS ARRAY (INTEGER RANGE <>) OF INTEGER;

     TYPE K IS ARRAY (1..10) OF INTEGER;

     SUBTYPE A IS J (0 .. 50);

     SUBTYPE W IS J (A'RANGE);

     SUBTYPE X IS J (K'RANGE);

     TYPE Y IS ACCESS J;

     TYPE Z IS ACCESS J;

     TYPE F IS NEW J (A'RANGE);

     TYPE G IS NEW J (K'RANGE);

     B : ARRAY (A'RANGE) OF INTEGER;

     C : ARRAY (K'RANGE) OF INTEGER;

     D : ARRAY (1 .. 10) OF INTEGER;

     E : ARRAY (D'RANGE) OF INTEGER;

     H : J (A'RANGE);

     I : J (K'RANGE);

     L : J (D'RANGE);

     V1 : W;

     V2 : X;

     V3 : Y := NEW J (A'RANGE);

     V4 : Z := NEW J (K'RANGE);

     V5 : F;

     V6 : G;

BEGIN
     TEST ("C36180A", "CHECK THAT AN INDEX CONSTRAINT CAN HAVE THE " &
                      "FORM A'RANGE, WHERE A IS A PREVIOUSLY " &
                      "DECLARED ARRAY OBJECT OR CONSTRAINED ARRAY " &
                      "SUBTYPE");

     IF B'FIRST /= IDENT_INT (0) OR B'LAST /= IDENT_INT (50)
          THEN FAILED ("IMPROPER VALUE FOR B'FIRST OR B'LAST");
     END IF;

     IF C'FIRST /= IDENT_INT (1) OR C'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR C'FIRST OR C'LAST");
     END IF;

     IF E'FIRST /= IDENT_INT (1) OR E'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR E'FIRST OR E'LAST");
     END IF;

     IF H'FIRST /= IDENT_INT (0) OR H'LAST /= IDENT_INT (50)
          THEN FAILED ("IMPROPER VALUE FOR H'FIRST OR H'LAST");
     END IF;

     IF I'FIRST /= IDENT_INT (1) OR I'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR I'FIRST OR I'LAST");
     END IF;

     IF L'FIRST /= IDENT_INT (1) OR L'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR L'FIRST OR L'LAST");
     END IF;

     IF V1'FIRST /= IDENT_INT (0) OR V1'LAST /= IDENT_INT (50)
          THEN FAILED ("IMPROPER VALUE FOR V1'FIRST OR V1'LAST");
     END IF;

     IF V2'FIRST /= IDENT_INT (1) OR V2'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR V2'FIRST OR V2'LAST");
     END IF;

     IF V3.ALL'FIRST /= IDENT_INT (0) OR V3.ALL'LAST /= IDENT_INT (50)
          THEN FAILED ("IMPROPER VALUE FOR V3'FIRST OR V3'LAST");
     END IF;

     IF V4.ALL'FIRST /= IDENT_INT (1) OR V4.ALL'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR V4'FIRST OR V4'LAST");
     END IF;

     IF V5'FIRST /= IDENT_INT (0) OR V5'LAST /= IDENT_INT (50)
          THEN FAILED ("IMPROPER VALUE FOR V5'FIRST OR V5'LAST");
     END IF;

     IF V6'FIRST /= IDENT_INT (1) OR V6'LAST /= IDENT_INT (10)
          THEN FAILED ("IMPROPER VALUE FOR V6'FIRST OR V6'LAST");
     END IF;

     RESULT;
END C36180A;
