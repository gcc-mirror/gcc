------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          P R J . A T T R . P M                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
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
      --  Only add the attribute if the package is already defined

      if To_Package /= Empty_Package then
         Attrs.Increment_Last;
         Attrs.Table (Attrs.Last) :=
           (Name              => Attribute_Name,
            Var_Kind          => Undefined,
            Optional_Index    => False,
            Attr_Kind         => Unknown,
            Next              =>
              Package_Attributes.Table (To_Package.Value).First_Attribute);
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
        (Name => Name, Known => False, First_Attribute => Empty_Attr);
   end Add_Unknown_Package;

end Prj.Attr.PM;
