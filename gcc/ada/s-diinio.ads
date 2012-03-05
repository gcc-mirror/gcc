------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . D I M . I N T E G E R _ I O               --
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

--  This package provides output routines for integer dimensioned types. All
--  Put routines are modelled after those in package Ada.Text_IO.Integer_IO
--  with the addition of an extra default parameter.

--  All the examples in this package are based on the MKS system of units:

--    type Mks_Type is new Integer
--      with
--       Dimension_System => ((Meter, 'm'),
--         (Kilogram, "kg"),
--         (Second,   's'),
--         (Ampere,   'A'),
--         (Kelvin,   'K'),
--         (Mole,     "mol"),
--         (Candela,  "cd"));

--  Parameter Symbol may be used in the following manner:

--  Case 1. A value is supplied for Symbol

--    The string appears as a suffix of Item

--      Obj : Mks_Type := 2;
--      Put (Obj, Symbols => " dimensionless");

--      The corresponding output is: 2 dimensionless

--  Case 2. No value is supplied for Symbol and Item is dimensionless

--    Item appears without a suffix

--      Obj : Mks_Type := 2;
--      Put (Obj);

--      The corresponding output is: 2

--  Case 3. No value is supplied for Symbol and Item has a dimension

--    If the type of Item is a dimensioned subtype whose symbolic name is not
--    empty, then the symbolic name appears as a suffix.

--      subtype Length is Mks_Type
--        with
--         Dimension => ('m',
--           Meter =>  1,
--           others => 0);

--      Obj : Length := 2;
--      Put (Obj);

--      The corresponding output is: 2 m

--    Otherwise, a new string is created and appears as a suffix of Item.
--    This string results in the successive concatanations between each
--    dimension symbolic name raised by its corresponding dimension power from
--    the dimensions of Item.

--      subtype Random is Mks_Type
--        with
--         Dimension => ("",
--         Meter =>   3,
--         Candela => 2,
--         others =>  0);

--      Obj : Random := 5;
--      Put (Obj);

--      The corresponding output is: 5 m**3.cd**2

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Num_Dim_Integer is range <>;

package System.Dim.Integer_IO is

   Default_Width : Field       := Num_Dim_Integer'Width;
   Default_Base  : Number_Base := 10;

   procedure Put
     (File    : File_Type;
      Item    : Num_Dim_Integer;
      Width   : Field       := Default_Width;
      Base    : Number_Base := Default_Base;
      Symbols : String      := "");

   procedure Put
     (Item    : Num_Dim_Integer;
      Width   : Field       := Default_Width;
      Base    : Number_Base := Default_Base;
      Symbols : String      := "");

   procedure Put
     (To      : out String;
      Item    : Num_Dim_Integer;
      Base    : Number_Base := Default_Base;
      Symbols : String      := "");

   pragma Inline (Put);

end System.Dim.Integer_IO;
