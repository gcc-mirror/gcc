-- C25001B.ADA

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
-- CHECK THAT ALL CHARACTER LITERALS CAN BE WRITTEN.

--      CASE B: THE LOWER CASE LETTERS AND THE OTHER
--              SPECIAL CHARACTERS.

-- TBN  8/1/86

WITH REPORT; USE REPORT;
PROCEDURE C25001B IS

BEGIN
     TEST ("C25001B", "CHECK THAT EACH CHARACTER IN THE LOWER CASE " &
                      "LETTERS AND THE OTHER SPECIAL CHARACTERS CAN " &
                      "BE WRITTEN");

     IF CHARACTER'POS('a') /= 97 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'a'");
     END IF;
     IF CHARACTER'POS('b') /= 98 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'b'");
     END IF;
     IF CHARACTER'POS('c') /= 99 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'c'");
     END IF;
     IF CHARACTER'POS('d') /= 100 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'd'");
     END IF;
     IF CHARACTER'POS('e') /= 101 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'e'");
     END IF;
     IF CHARACTER'POS('f') /= 102 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'f'");
     END IF;
     IF CHARACTER'POS('g') /= 103 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'g'");
     END IF;
     IF CHARACTER'POS('h') /= 104 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'h'");
     END IF;
     IF CHARACTER'POS('i') /= 105 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'i'");
     END IF;
     IF CHARACTER'POS('j') /= 106 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'j'");
     END IF;
     IF CHARACTER'POS('k') /= 107 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'k'");
     END IF;
     IF CHARACTER'POS('l') /= 108 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'l'");
     END IF;
     IF CHARACTER'POS('m') /= 109 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'm'");
     END IF;
     IF CHARACTER'POS('n') /= 110 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'n'");
     END IF;
     IF CHARACTER'POS('o') /= 111 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'o'");
     END IF;
     IF CHARACTER'POS('p') /= 112 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'p'");
     END IF;
     IF CHARACTER'POS('q') /= 113 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'q'");
     END IF;
     IF CHARACTER'POS('r') /= 114 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'r'");
     END IF;
     IF CHARACTER'POS('s') /= 115 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 's'");
     END IF;
     IF CHARACTER'POS('t') /= 116 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 't'");
     END IF;
     IF CHARACTER'POS('u') /= 117 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'u'");
     END IF;
     IF CHARACTER'POS('v') /= 118 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'v'");
     END IF;
     IF CHARACTER'POS('w') /= 119 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'w'");
     END IF;
     IF CHARACTER'POS('x') /= 120 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'x'");
     END IF;
     IF CHARACTER'POS('y') /= 121 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'y'");
     END IF;
     IF CHARACTER'POS('z') /= 122 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR 'z'");
     END IF;

     IF CHARACTER'POS('!') /= 33 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '!'");
     END IF;
     IF CHARACTER'POS('$') /= 36 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '$'");
     END IF;
     IF CHARACTER'POS('%') /= 37 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '%'");
     END IF;
     IF CHARACTER'POS('?') /= 63 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '?'");
     END IF;
     IF CHARACTER'POS('@') /= 64 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '@'");
     END IF;
     IF CHARACTER'POS('[') /= 91 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '['");
     END IF;
     IF CHARACTER'POS('\') /= 92 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '\'");
     END IF;
     IF CHARACTER'POS(']') /= 93 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR ']'");
     END IF;
     IF CHARACTER'POS('^') /= 94 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '^'");
     END IF;
     IF CHARACTER'POS('`') /= 96 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '`'");
     END IF;
     IF CHARACTER'POS('{') /= 123 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '{'");
     END IF;
     IF CHARACTER'POS('}') /= 125 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '}'");
     END IF;
     IF CHARACTER'POS('~') /= 126 THEN
          FAILED ("INCORRECT POSITION NUMBER FOR '~'");
     END IF;
 
     RESULT;
END C25001B;
