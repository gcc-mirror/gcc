------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . D I M . I N T E G E R _ I O               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2011-2025, Free Software Foundation, Inc.         --
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
--  Put routines are modeled after those in package Ada.Text_IO.Integer_IO
--  with the addition of an extra default parameter. All Put_Dim_Of routines
--  output the dimension of Item in a symbolic manner.

--  Parameter Symbol may be used in the following manner (all the examples are
--  based on the MKS system of units as defined in package System.Dim.Mks):

--    type Mks_Type is new Integer
--      with
--       Dimension_System => (
--        (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
--        (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
--        (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
--        (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
--        (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => "Θ"),
--        (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
--        (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J'));

--  Case 1. A value is supplied for Symbol

--   * Put        : The string appears as a suffix of Item

--   * Put_Dim_Of : The string appears alone

--      Obj : Mks_Type := 2;
--      Put (Obj, Symbols => "dimensionless");
--      Put_Dim_Of (Obj, Symbols => "dimensionless");

--      The corresponding outputs are:
--      $2 dimensionless
--      $dimensionless

--  Case 2. No value is supplied for Symbol and Item is dimensionless

--   * Put        : Item appears without a suffix

--   * Put_Dim_Of : the output is []

--      Obj : Mks_Type := 2;
--      Put (Obj);
--      Put_Dim_Of (Obj);

--      The corresponding outputs are:
--      $2
--      $[]

--  Case 3. No value is supplied for Symbol and Item has a dimension

--   * Put        : If the type of Item is a dimensioned subtype whose
--                  symbol is not empty, then the symbol appears as a suffix.
--                  Otherwise, a new string is created and appears as a
--                  suffix of Item. This string results in the successive
--                  concatenations between each unit symbol raised by its
--                  corresponding dimension power from the dimensions of Item.

--   * Put_Dim_Of : The output is a new string resulting in the successive
--                  concatenations between each dimension symbol raised by its
--                  corresponding dimension power from the dimensions of Item.

--      subtype Length is Mks_Type
--        with
--         Dimension => ('m',
--           Meter =>  1,
--           others => 0);

--      Obj : Length := 2;
--      Put (Obj);
--      Put_Dim_Of (Obj);

--      The corresponding outputs are:
--      $2 m
--      $[L]

--      subtype Random is Mks_Type
--        with
--         Dimension => ("",
--           Meter =>   3,
--           Candela => 2,
--           others =>  0);

--      Obj : Random := 5;
--      Put (Obj);
--      Put_Dim_Of (Obj);

--      The corresponding outputs are:
--      $5 m**3.cd**2
--      $[L**3.J**2]

with Ada.Text_IO; use Ada.Text_IO;

generic
   type Num_Dim_Integer is range <>;

package System.Dim.Integer_IO is

   Default_Width : Field       := Num_Dim_Integer'Width;
   Default_Base  : Number_Base := 10;

   procedure Put
     (File   : File_Type;
      Item   : Num_Dim_Integer;
      Width  : Field       := Default_Width;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "");

   procedure Put
     (Item   : Num_Dim_Integer;
      Width  : Field       := Default_Width;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "");

   procedure Put
     (To     : out String;
      Item   : Num_Dim_Integer;
      Base   : Number_Base := Default_Base;
      Symbol : String      := "");

   procedure Put_Dim_Of
     (File   : File_Type;
      Item   : Num_Dim_Integer;
      Symbol : String := "");

   procedure Put_Dim_Of
     (Item   : Num_Dim_Integer;
      Symbol : String := "");

   procedure Put_Dim_Of
     (To     : out String;
      Item   : Num_Dim_Integer;
      Symbol : String := "");

   pragma Inline (Put);
   pragma Inline (Put_Dim_Of);

end System.Dim.Integer_IO;
