------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C S T A N D                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package contains the procedure that is used to create the tree for
--  package Standard and initialize the entities in package Stand.

with Types; use Types;

package CStand is

   procedure Create_Standard;
   --  This procedure creates the tree for package standard, and initializes
   --  the Standard_Entities array and Standard_Package_Node. First the
   --  syntactic representation is created (as though the parser had parsed
   --  a copy of the source of Standard) and then semantic information is
   --  added as it would be by the semantic phases of the compiler. The
   --  tree is in the standard format defined by Syntax_Info, except that
   --  all Sloc values are set to Standard_Location except for nodes that
   --  are part of package ASCII, which have Sloc = Standard_ASCII_Location.
   --  The semantics info is in the format given by Entity_Info. The global
   --  variables Last_Standard_Node_Id and Last_Standard_List_Id are also set.

   procedure Set_Float_Bounds (Id  : Entity_Id);
   --  Procedure to set bounds for float type or subtype. Id is the entity
   --  whose bounds and type are to be set (a floating-point type).

end CStand;
