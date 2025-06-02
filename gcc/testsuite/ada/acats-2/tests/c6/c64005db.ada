-- C64005DB.ADA

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
-- JRK 7/30/84

SEPARATE (C64005D0M.C64005DA)

PROCEDURE C64005DB (L : LEVEL; C : CALL; T : IN OUT TRACE) IS

     V : STRING (1..2);

     M : CONSTANT NATURAL := LEVEL'POS (L) -
                             LEVEL'POS (LEVEL'FIRST) + 1;
     N : CONSTANT NATURAL := 2 * M + 1;

     PROCEDURE C64005DC (L : LEVEL; C : CALL; T : IN OUT TRACE) IS
          SEPARATE;

BEGIN

     V (1) := IDENT_CHAR (ASCII.LC_B);
     V (2) := C;

     -- APPEND ALL V TO T.
     T.S (T.E+1 .. T.E+N) := C64005D0M.V & C64005DA.V & C64005DB.V;
     T.E := T.E + N;

     CASE C IS

          WHEN '1' =>
               C64005DC (LEVEL'SUCC(L), IDENT_CHAR('1'), T);

          WHEN '2' =>
               C64005DB (L, IDENT_CHAR('3'), T);

          WHEN '3' =>
               C64005DC (LEVEL'SUCC(L), IDENT_CHAR('2'), T);
     END CASE;

     -- APPEND ALL L AND C TO T IN REVERSE ORDER.
     T.S (T.E+1 .. T.E+N) := C64005DB.L & C64005DB.C &
                             C64005DA.L & C64005DA.C &
                             C64005D0M.L;
     T.E := T.E + N;

END C64005DB;
