------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . P O W T E N _ L F L T                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2020-2021, Free Software Foundation, Inc.      --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a powers of ten table used for real conversions

package System.Powten_LFlt is
   pragma Pure;

   Maxpow_Exact : constant := 22;
   --  Largest power of ten exactly representable with Long_Float. It is equal
   --  to floor (M * log 2 / log 5), when M is the size of the mantissa (53).

   Maxpow : constant := Maxpow_Exact * 2;
   --  Largest power of ten exactly representable with a double Long_Float

   Powten : constant array (0 .. Maxpow, 1 .. 2) of Long_Float :=
     (00 => (1.0E+00, 0.0),
      01 => (1.0E+01, 0.0),
      02 => (1.0E+02, 0.0),
      03 => (1.0E+03, 0.0),
      04 => (1.0E+04, 0.0),
      05 => (1.0E+05, 0.0),
      06 => (1.0E+06, 0.0),
      07 => (1.0E+07, 0.0),
      08 => (1.0E+08, 0.0),
      09 => (1.0E+09, 0.0),
      10 => (1.0E+10, 0.0),
      11 => (1.0E+11, 0.0),
      12 => (1.0E+12, 0.0),
      13 => (1.0E+13, 0.0),
      14 => (1.0E+14, 0.0),
      15 => (1.0E+15, 0.0),
      16 => (1.0E+16, 0.0),
      17 => (1.0E+17, 0.0),
      18 => (1.0E+18, 0.0),
      19 => (1.0E+19, 0.0),
      20 => (1.0E+20, 0.0),
      21 => (1.0E+21, 0.0),
      22 => (1.0E+22, 0.0),
      23 => (1.0E+23, 1.0E+23 - Long_Float'Machine (1.0E+23)),
      24 => (1.0E+24, 1.0E+24 - Long_Float'Machine (1.0E+24)),
      25 => (1.0E+25, 1.0E+25 - Long_Float'Machine (1.0E+25)),
      26 => (1.0E+26, 1.0E+26 - Long_Float'Machine (1.0E+26)),
      27 => (1.0E+27, 1.0E+27 - Long_Float'Machine (1.0E+27)),
      28 => (1.0E+28, 1.0E+28 - Long_Float'Machine (1.0E+28)),
      29 => (1.0E+29, 1.0E+29 - Long_Float'Machine (1.0E+29)),
      30 => (1.0E+30, 1.0E+30 - Long_Float'Machine (1.0E+30)),
      31 => (1.0E+31, 1.0E+31 - Long_Float'Machine (1.0E+31)),
      32 => (1.0E+32, 1.0E+32 - Long_Float'Machine (1.0E+32)),
      33 => (1.0E+33, 1.0E+33 - Long_Float'Machine (1.0E+33)),
      34 => (1.0E+34, 1.0E+34 - Long_Float'Machine (1.0E+34)),
      35 => (1.0E+35, 1.0E+35 - Long_Float'Machine (1.0E+35)),
      36 => (1.0E+36, 1.0E+36 - Long_Float'Machine (1.0E+36)),
      37 => (1.0E+37, 1.0E+37 - Long_Float'Machine (1.0E+37)),
      38 => (1.0E+38, 1.0E+38 - Long_Float'Machine (1.0E+38)),
      39 => (1.0E+39, 1.0E+39 - Long_Float'Machine (1.0E+39)),
      40 => (1.0E+40, 1.0E+40 - Long_Float'Machine (1.0E+40)),
      41 => (1.0E+41, 1.0E+41 - Long_Float'Machine (1.0E+41)),
      42 => (1.0E+42, 1.0E+42 - Long_Float'Machine (1.0E+42)),
      43 => (1.0E+43, 1.0E+43 - Long_Float'Machine (1.0E+43)),
      44 => (1.0E+44, 1.0E+44 - Long_Float'Machine (1.0E+44)));

end System.Powten_LFlt;
