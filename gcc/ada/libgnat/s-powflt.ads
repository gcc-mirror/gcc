------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P O W T E N _ F L T                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2020-2024, Free Software Foundation, Inc.      --
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

--  This package provides tables of powers used for real conversions

package System.Powten_Flt is
   pragma Pure;

   Maxpow_Exact : constant := 10;
   --  Largest power of five exactly representable with Float. It is equal to
   --  floor (M * log 2 / log 5), when M is the size of the mantissa (24).
   --  It also works for any number of the form 5*(2**N) and in particular 10.

   Maxpow : constant := Maxpow_Exact * 2;
   --  Largest power of five exactly representable with double Float

   Powfive : constant array (0 .. Maxpow, 1 .. 2) of Float :=
     [00 => [5.0**00, 0.0],
      01 => [5.0**01, 0.0],
      02 => [5.0**02, 0.0],
      03 => [5.0**03, 0.0],
      04 => [5.0**04, 0.0],
      05 => [5.0**05, 0.0],
      06 => [5.0**06, 0.0],
      07 => [5.0**07, 0.0],
      08 => [5.0**08, 0.0],
      09 => [5.0**09, 0.0],
      10 => [5.0**10, 0.0],
      11 => [5.0**11, 5.0**11 - Float'Machine (5.0**11)],
      12 => [5.0**12, 5.0**12 - Float'Machine (5.0**12)],
      13 => [5.0**13, 5.0**13 - Float'Machine (5.0**13)],
      14 => [5.0**14, 5.0**14 - Float'Machine (5.0**14)],
      15 => [5.0**15, 5.0**15 - Float'Machine (5.0**15)],
      16 => [5.0**16, 5.0**16 - Float'Machine (5.0**16)],
      17 => [5.0**17, 5.0**17 - Float'Machine (5.0**17)],
      18 => [5.0**18, 5.0**18 - Float'Machine (5.0**18)],
      19 => [5.0**19, 5.0**19 - Float'Machine (5.0**19)],
      20 => [5.0**20, 5.0**20 - Float'Machine (5.0**20)]];

   Powten : constant array (0 .. Maxpow, 1 .. 2) of Float :=
     [00 => [1.0E+00, 0.0],
      01 => [1.0E+01, 0.0],
      02 => [1.0E+02, 0.0],
      03 => [1.0E+03, 0.0],
      04 => [1.0E+04, 0.0],
      05 => [1.0E+05, 0.0],
      06 => [1.0E+06, 0.0],
      07 => [1.0E+07, 0.0],
      08 => [1.0E+08, 0.0],
      09 => [1.0E+09, 0.0],
      10 => [1.0E+10, 0.0],
      11 => [1.0E+11, 1.0E+11 - Float'Machine (1.0E+11)],
      12 => [1.0E+12, 1.0E+12 - Float'Machine (1.0E+12)],
      13 => [1.0E+13, 1.0E+13 - Float'Machine (1.0E+13)],
      14 => [1.0E+14, 1.0E+14 - Float'Machine (1.0E+14)],
      15 => [1.0E+15, 1.0E+15 - Float'Machine (1.0E+15)],
      16 => [1.0E+16, 1.0E+16 - Float'Machine (1.0E+16)],
      17 => [1.0E+17, 1.0E+17 - Float'Machine (1.0E+17)],
      18 => [1.0E+18, 1.0E+18 - Float'Machine (1.0E+18)],
      19 => [1.0E+19, 1.0E+19 - Float'Machine (1.0E+19)],
      20 => [1.0E+20, 1.0E+20 - Float'Machine (1.0E+20)]];

end System.Powten_Flt;
