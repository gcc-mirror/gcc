------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          P R J . A T T R . P M                           --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;

package Prj.Attr.PM is
   --  The following procedures are not secure and should only be used by the
   --  Project Manager, that is the packages of the Prj or MLib hierarchies.

   procedure Add_Unknown_Package (Name : Name_Id; Id : out Package_Node_Id);
   --  Add a new unknown package. The Name cannot be the name of a predefined
   --  or already registered package, but this is not checked.

   procedure Add_Attribute
     (To_Package     : Package_Node_Id;
      Attribute_Name : Name_Id;
      Attribute_Node : out Attribute_Node_Id);
   --  Add an attribute to the list for package To_Package. Attribute_Name
   --  cannot be the name of an existing attribute of the package, but this is
   --  not checked. Does nothing if To_Package is Empty_Package.

end Prj.Attr.PM;
