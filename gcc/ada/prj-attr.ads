------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . A T T R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2001-2002 Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines allowed packages and attributes in GNAT project files

with Types; use Types;
with Table;

package Prj.Attr is

   --  Define the allowed attributes

   --  All these declarations are uncommented, they all need comments ???

   Attributes_Initial   : constant := 50;
   Attributes_Increment : constant := 50;

   Attribute_Node_Low_Bound  : constant := 0;
   Attribute_Node_High_Bound : constant := 099_999_999;

   type Attribute_Node_Id is
     range Attribute_Node_Low_Bound .. Attribute_Node_High_Bound;

   First_Attribute_Node_Id : constant Attribute_Node_Id :=
                               Attribute_Node_Low_Bound + 1;

   Empty_Attribute : constant Attribute_Node_Id :=
                       Attribute_Node_Low_Bound;

   type Attribute_Kind is
     (Single,
      Associative_Array,
      Case_Insensitive_Associative_Array);

   type Attribute_Record is record
      Name     : Name_Id;
      Kind_1   : Variable_Kind;
      Kind_2   : Attribute_Kind;
      Next     : Attribute_Node_Id;
   end record;

   package Attributes is
      new Table.Table (Table_Component_Type => Attribute_Record,
                       Table_Index_Type     => Attribute_Node_Id,
                       Table_Low_Bound      => First_Attribute_Node_Id,
                       Table_Initial        => Attributes_Initial,
                       Table_Increment      => Attributes_Increment,
                       Table_Name           => "Prj.Attr.Attributes");

   Attribute_First : constant Attribute_Node_Id := First_Attribute_Node_Id;

   --  Define the allowed packages

   Packages_Initial   : constant := 10;
   Packages_Increment : constant := 50;

   Package_Node_Low_Bound  : constant := 0;
   Package_Node_High_Bound : constant := 099_999_999;

   type Package_Node_Id is
     range Package_Node_Low_Bound .. Package_Node_High_Bound;

   First_Package_Node_Id : constant Package_Node_Id :=
                             Package_Node_Low_Bound + 1;

   Empty_Package : constant Package_Node_Id := Package_Node_Low_Bound;

   type Package_Record is record
      Name            : Name_Id;
      First_Attribute : Attribute_Node_Id;
   end record;

   package Package_Attributes is
      new Table.Table (Table_Component_Type => Package_Record,
                       Table_Index_Type     => Package_Node_Id,
                       Table_Low_Bound      => First_Package_Node_Id,
                       Table_Initial        => Packages_Initial,
                       Table_Increment      => Packages_Increment,
                       Table_Name           => "Prj.Attr.Packages");

   Package_First : constant Package_Node_Id := First_Package_Node_Id;

   procedure Initialize;
   --  Initialize the two tables above (Attributes and Package_Attributes).
   --  This procedure should be called by Prj.Initialize.

end Prj.Attr;
