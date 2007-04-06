------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains routines involved in the frontend expansion of
--  subprograms of package Ada.Tags

with Types; use Types;

package Exp_Atag is

   function Build_CW_Membership
     (Loc          : Source_Ptr;
      Obj_Tag_Node : Node_Id;
      Typ_Tag_Node : Node_Id) return Node_Id;
   --  Build code that returns true if Obj_Tag is in Typ_Tag'Class. Each
   --  dispatch table contains a reference to a table of ancestors (stored
   --  in the first part of the Tags_Table) and a count of the level of
   --  inheritance "Idepth". Obj is in Typ'Class if Typ'Tag is in the table
   --  of ancestors that are contained in the dispatch table referenced by
   --  Obj'Tag. Knowing the level of inheritance of both types, this can be
   --  computed in constant time by the formula:
   --
   --   TSD (Obj'tag).Tags_Table (TSD (Obj'tag).Idepth - TSD (Typ'tag).Idepth)
   --     = Typ'tag

   function Build_Get_Access_Level
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the accessibility level of the tagged type.
   --
   --  Generates: TSD (Tag).Access_Level

   function Build_Get_Predefined_Prim_Op_Address
     (Loc           : Source_Ptr;
      Tag_Node      : Node_Id;
      Position_Node : Node_Id) return Node_Id;
   --  Given a pointer to a dispatch table (T) and a position in the DT, build
   --  code that gets the address of the predefined virtual function stored in
   --  it (used for dispatching calls).
   --
   --  Generates: Predefined_DT (Tag).D (Position);

   function Build_Get_Prim_Op_Address
     (Loc           : Source_Ptr;
      Tag_Node      : Node_Id;
      Position_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the address of the virtual function stored in
   --  a given position of the dispatch table (used for dispatching calls).
   --
   --  Generates: To_Tag (Tag).D (Position);

   function Build_Get_RC_Offset
     (Loc        : Source_Ptr;
      Tag_Node   : Node_Id) return Node_Id;
   --  Build code that retrieves the Offset of the implicit record controller
   --  when the object has controlled components. O otherwise.
   --
   --  Generates: TSD (T).RC_Offset;

   function Build_Get_Remotely_Callable
     (Loc        : Source_Ptr;
      Tag_Node   : Node_Id) return Node_Id;
   --  Build code that retrieves the value previously saved by Set_Remotely
   --  Callable
   --
   --  Generates: TSD (Tag).Remotely_Callable

   function Build_Inherit_Predefined_Prims
     (Loc              : Source_Ptr;
      Old_Tag_Node     : Node_Id;
      New_Tag_Node     : Node_Id) return Node_Id;
   --  Build code that inherits the predefined primitives of the parent.
   --
   --  Generates: Predefined_DT (New_T).D (All_Predefined_Prims) :=
   --               Predefined_DT (Old_T).D (All_Predefined_Prims);

   function Build_Inherit_Prims
     (Loc          : Source_Ptr;
      Old_Tag_Node : Node_Id;
      New_Tag_Node : Node_Id;
      Num_Prims    : Nat) return Node_Id;
   --  Build code that inherits Num_Prims user-defined primitives from the
   --  dispatch table of the parent type.
   --
   --  Generates:
   --    New_Tag.Prims_Ptr (1 .. Num_Prims) :=
   --      Old_Tag.Prims_Ptr (1 .. Num_Prims);

   function Build_Inherit_TSD
     (Loc               : Source_Ptr;
      Old_Tag_Node      : Node_Id;
      New_Tag_Node      : Node_Id;
      I_Depth           : Nat;
      Parent_Num_Ifaces : Nat) return Node_Id;
   --  Generates code that initializes the TSD of a type knowing the tag,
   --  inheritance depth, and number of interface types of the parent type.
   --
   --  Generates:
   --     --  Copy the table of ancestors of the parent
   --
   --     TSD (New_Tag).Tags_Table (1 .. I_Depth) :=
   --       TSD (Old_Tag).Tags_Table (0 .. I_Depth - 1);
   --
   --     --  Copy the table of interfaces of the parent
   --
   --     if TSD (Old_Tag).Ifaces_Table_Ptr /= null then
   --        New_Iface_Table_Ptr.Table (1 .. Parent_Num_Ifaces):=
   --          Old_Iface_Table_Ptr.Table (1 .. Parent_Num_Ifaces);
   --     end if;
   --
   --     TSD (New_Tag).Tags_Table (0) := New_Tag;

   function Build_New_TSD
     (Loc          : Source_Ptr;
      New_Tag_Node : Node_Id) return List_Id;
   --  Build code that initializes the TSD of a root type.
   --  Generates: TSD (New_Tag).Tags_Table (0) := New_Tag;

   function Build_Set_External_Tag
     (Loc        : Source_Ptr;
      Tag_Node   : Node_Id;
      Value_Node : Node_Id) return Node_Id;
   --  Build code that saves the address of the string containing the external
   --  tag in the dispatch table.
   --
   --  Generates: TSD (Tag).External_Tag := Cstring_Ptr! (Value);

   function Build_Set_Predefined_Prim_Op_Address
     (Loc           : Source_Ptr;
      Tag_Node      : Node_Id;
      Position_Node : Node_Id;
      Address_Node  : Node_Id) return Node_Id;
   --  Build code that saves the address of a virtual function in a given
   --  Position of the portion of the dispatch table associated with the
   --  predefined primitives of Tag (used for overriding).
   --
   --  Generates: Predefined_DT (Tag).D (Position) := Value

   function Build_Set_Prim_Op_Address
     (Loc           : Source_Ptr;
      Tag_Node      : Node_Id;
      Position_Node : Node_Id;
      Address_Node  : Node_Id) return Node_Id;
   --  Build code that saves the address of a virtual function in a given
   --  Position of the dispatch table associated with the Tag (used for
   --  overriding).
   --
   --  Generates: Tag.D (Position) := Value

   function Build_Set_TSD
     (Loc        : Source_Ptr;
      Tag_Node   : Node_Id;
      Value_Node : Node_Id) return Node_Id;
   --  Build code that saves the address of the record containing the Type
   --  Specific Data generated by GNAT.
   --
   --  Generates: To_Addr_Ptr (To_Address (Tag) - K_Typeinfo).all := Value

end Exp_Atag;
