------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . P O W T E N _ T A B L E                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

package System.Powten_Table is
   pragma Pure;

   Maxpow : constant := 22;
   --  The number of entries in this table is chosen to include powers of ten
   --  that are exactly representable with long_long_float. Assuming that on
   --  all targets we have 53 bits of mantissa for the type, the upper bound is
   --  given by 53/(log 5). If the scaling factor for a string is greater than
   --  Maxpow, it can be obtained by several multiplications, which is less
   --  efficient than with a bigger table, but avoids anomalies at end points.

   Powten : constant array (0 .. Maxpow) of Long_Long_Float :=
      (00 => 1.0E+00,
       01 => 1.0E+01,
       02 => 1.0E+02,
       03 => 1.0E+03,
       04 => 1.0E+04,
       05 => 1.0E+05,
       06 => 1.0E+06,
       07 => 1.0E+07,
       08 => 1.0E+08,
       09 => 1.0E+09,
       10 => 1.0E+10,
       11 => 1.0E+11,
       12 => 1.0E+12,
       13 => 1.0E+13,
       14 => 1.0E+14,
       15 => 1.0E+15,
       16 => 1.0E+16,
       17 => 1.0E+17,
       18 => 1.0E+18,
       19 => 1.0E+19,
       20 => 1.0E+20,
       21 => 1.0E+21,
       22 => 1.0E+22);

end System.Powten_Table;
