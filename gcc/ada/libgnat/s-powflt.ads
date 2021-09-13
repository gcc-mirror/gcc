------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P O W T E N _ F L T                     --
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

package System.Powten_Flt is
   pragma Pure;

   Maxpow_Exact : constant := 10;
   --  Largest power of ten exactly representable with Float. It is equal to
   --  floor (M * log 2 / log 5), when M is the size of the mantissa (24).

   Maxpow : constant := Maxpow_Exact * 2;
   --  Largest power of ten exactly representable with a double Float

   Powten : constant array (0 .. Maxpow, 1 .. 2) of Float :=
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
      11 => (1.0E+11, 1.0E+11 - Float'Machine (1.0E+11)),
      12 => (1.0E+12, 1.0E+12 - Float'Machine (1.0E+12)),
      13 => (1.0E+13, 1.0E+13 - Float'Machine (1.0E+13)),
      14 => (1.0E+14, 1.0E+14 - Float'Machine (1.0E+14)),
      15 => (1.0E+15, 1.0E+15 - Float'Machine (1.0E+15)),
      16 => (1.0E+16, 1.0E+16 - Float'Machine (1.0E+16)),
      17 => (1.0E+17, 1.0E+17 - Float'Machine (1.0E+17)),
      18 => (1.0E+18, 1.0E+18 - Float'Machine (1.0E+18)),
      19 => (1.0E+19, 1.0E+19 - Float'Machine (1.0E+19)),
      20 => (1.0E+20, 1.0E+20 - Float'Machine (1.0E+20)));

end System.Powten_Flt;
