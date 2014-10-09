------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          P R J . A T T R . P M                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2014, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Prj.Attr.PM is

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (To_Package     : Package_Node_Id;
      Attribute_Name : Name_Id;
      Attribute_Node : out Attribute_Node_Id)
   is
   begin
      --  Only add attribute if package is already defined and is not unknown

      if To_Package /= Empty_Package   and then
         To_Package /= Unknown_Package
      then
         Attrs.Append (
           (Name           => Attribute_Name,
            Var_Kind       => Undefined,
            Optional_Index => False,
            Attr_Kind      => Unknown,
            Read_Only      => False,
            Others_Allowed => False,
            Default        => Empty_Value,
            Next           =>
              Package_Attributes.Table (To_Package.Value).First_Attribute));

         Package_Attributes.Table (To_Package.Value).First_Attribute :=
           Attrs.Last;
         Attribute_Node := (Value => Attrs.Last);
      end if;
   end Add_Attribute;

   -------------------------
   -- Add_Unknown_Package --
   -------------------------

   procedure Add_Unknown_Package (Name : Name_Id; Id : out Package_Node_Id) is
   begin
      Package_Attributes.Increment_Last;
      Id := (Value => Package_Attributes.Last);
      Package_Attributes.Table (Id.Value) :=
        (Name             => Name,
         Known            => False,
         First_Attribute  => Empty_Attr);
   end Add_Unknown_Package;

end Prj.Attr.PM;
