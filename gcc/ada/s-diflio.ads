------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . D I M . F L O A T _ I O                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2012, Free Software Foundation, Inc.         --
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

--  This package provides output routines for float dimensioned types. All Put
--  routines are modelled after those in package Ada.Text_IO.Float_IO with the
--  addition of an extra default parameter.

--  Parameter Symbol may be used in the following manner (all the examples are
--  based on the MKS system of units as defined in package System.Dim.Mks):

--  Case 1. A value is supplied for Symbol

--    The string appears as a suffix of Item

--      Obj : Mks_Type := 2.6;
--      Put (Obj, 1, 1, 0, " dimensionless");

--      The corresponding output is: 2.6 dimensionless

--  Case 2. No value is supplied for Symbol and Item is dimensionless

--    Item appears without a suffix

--      Obj : Mks_Type := 2.6;
--      Put (Obj, 1, 1, 0);

--      The corresponding output is: 2.6

--  Case 3. No value is supplied for Symbol and Item has a dimension

--    If the type of Item is a dimensioned subtype whose symbolic name is not
--    empty, then the symbolic name appears as a suffix.

--      subtype Length is Mks_Type
--        with
--         Dimension => ('m',
--           Meter =>  1,
--           others => 0);

--      Obj : Length := 2.3 * dm;
--      Put (Obj, 1, 2, 0);

--      The corresponding output is: 0.23 m

--    Otherwise, a new string is created and appears as a suffix of Item.
--    This string results in the successive concatanations between each
--    dimension symbolic name raised by its corresponding dimension power from
--    the dimensions of Item.

--      subtype Random is Mks_Type
--        with
--         Dimension => ("",
--         Meter =>   3,
--         Candela => -1,
--         others =>  0);

--      Obj : Random := 5.0;
--      Put (Obj);

--      The corresponding output is: 5.0 m**3.cd**(-1)

--      Put (3.3 * km * dm * min, 5, 1, 0);

--      The corresponding output is: 19800.0 m**2.s

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Num_Dim_Float is digits <>;

package System.Dim.Float_IO is

   Default_Fore : Field := 2;
   Default_Aft  : Field := Num_Dim_Float'Digits - 1;
   Default_Exp  : Field := 3;

   procedure Put
     (File    : File_Type;
      Item    : Num_Dim_Float;
      Fore    : Field  := Default_Fore;
      Aft     : Field  := Default_Aft;
      Exp     : Field  := Default_Exp;
      Symbols : String := "");

   procedure Put
     (Item    : Num_Dim_Float;
      Fore    : Field  := Default_Fore;
      Aft     : Field  := Default_Aft;
      Exp     : Field  := Default_Exp;
      Symbols : String := "");

   procedure Put
     (To      : out String;
      Item    : Num_Dim_Float;
      Aft     : Field  := Default_Aft;
      Exp     : Field  := Default_Exp;
      Symbols : String := "");

   pragma Inline (Put);

end System.Dim.Float_IO;
