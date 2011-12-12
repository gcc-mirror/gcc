------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2011, Free Software Foundation, Inc.         --
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

--  This package contains routines involved in the frontend expansion of
--  subprograms of package Ada.Tags

with Types; use Types;
with Uintp; use Uintp;

package Exp_Atag is

   --  Note: In all the subprograms of this package formal 'Loc' is the source
   --  location used in constructing the corresponding nodes.

   procedure Build_Common_Dispatching_Select_Statements
     (Typ   : Entity_Id;
      Stmts : List_Id);
   --  Ada 2005 (AI-345): Build statements that are common to the expansion of
   --  timed, asynchronous, and conditional select and append them to Stmts.
   --  Typ is the tagged type used for dispatching calls.

   procedure Build_CW_Membership
     (Loc          : Source_Ptr;
      Obj_Tag_Node : in out Node_Id;
      Typ_Tag_Node : Node_Id;
      Related_Nod  : Node_Id;
      New_Node     : out Node_Id);
   --  Build code that returns true if Obj_Tag is in Typ_Tag'Class. Each DT
   --  has a table of ancestors and its inheritance level (Idepth). Obj is in
   --  Typ'Class if Typ'Tag is found in the table of ancestors referenced by
   --  Obj'Tag. Knowing the level of inheritance of both types, this can be
   --  computed in constant time by the formula:
   --
   --   Index := TSD (Obj'Tag).Idepth - TSD (Typ'Tag).Idepth;
   --   Index > 0 and then TSD (Obj'Tag).Tags_Table (Index) = Typ'Tag
   --
   --  Related_Nod is the node where the implicit declaration of variable Index
   --  is inserted. Obj_Tag_Node is relocated.

   function Build_Get_Access_Level
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the accessibility level of the tagged type.
   --
   --  Generates: TSD (Tag).Access_Level

   function Build_Get_Alignment
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the alignment of the tagged type.
   --
   --  Generates: TSD (Tag).Alignment

   procedure Build_Get_Predefined_Prim_Op_Address
     (Loc      : Source_Ptr;
      Position : Uint;
      Tag_Node : in out Node_Id;
      New_Node : out Node_Id);
   --  Given a pointer to a dispatch table (T) and a position in the DT, build
   --  code that gets the address of the predefined virtual function stored in
   --  it (used for dispatching calls). Tag_Node is relocated.
   --
   --  Generates: Predefined_DT (Tag).D (Position);

   procedure Build_Get_Prim_Op_Address
     (Loc      : Source_Ptr;
      Typ      : Entity_Id;
      Position : Uint;
      Tag_Node : in out Node_Id;
      New_Node : out Node_Id);
   --  Build code that retrieves the address of the virtual function stored in
   --  a given position of the dispatch table (used for dispatching calls).
   --  Tag_Node is relocated.
   --
   --  Generates: To_Tag (Tag).D (Position);

   function Build_Get_Transportable
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the value of the Transportable flag for
   --  the given Tag.
   --
   --  Generates: TSD (Tag).Transportable;

   function Build_Inherit_CPP_Prims (Typ : Entity_Id) return List_Id;
   --  Build code that copies from Typ's parent the dispatch table slots of
   --  inherited primitives and updates slots of overridden primitives. The
   --  generated code handles primary and secondary dispatch tables of Typ.

   function Build_Inherit_Predefined_Prims
     (Loc          : Source_Ptr;
      Old_Tag_Node : Node_Id;
      New_Tag_Node : Node_Id) return Node_Id;
   --  Build code that inherits the predefined primitives of the parent.
   --
   --  Generates: Predefined_DT (New_T).D (All_Predefined_Prims) :=
   --               Predefined_DT (Old_T).D (All_Predefined_Prims);
   --
   --  Required to build non-library level dispatch tables. Also required
   --  when compiling without static dispatch tables support.

   function Build_Inherit_Prims
     (Loc          : Source_Ptr;
      Typ          : Entity_Id;
      Old_Tag_Node : Node_Id;
      New_Tag_Node : Node_Id;
      Num_Prims    : Nat) return Node_Id;
   --  Build code that inherits Num_Prims user-defined primitives from the
   --  dispatch table of the parent type of tagged type Typ. It is used to
   --  copy the dispatch table of the parent in the following cases:
   --    a) case of derivations of CPP_Class types
   --    b) tagged types whose dispatch table is not statically allocated
   --
   --  Generates:
   --    New_Tag.Prims_Ptr (1 .. Num_Prims) :=
   --      Old_Tag.Prims_Ptr (1 .. Num_Prims);

   function Build_Offset_To_Top
     (Loc       : Source_Ptr;
      This_Node : Node_Id) return Node_Id;
   --  Build code that references the Offset_To_Top component of the primary
   --  or secondary dispatch table associated with This_Node. This subprogram
   --  provides a subset of the functionality provided by the function
   --  Offset_To_Top of package Ada.Tags, and is only called by the frontend
   --  when such routine is not available in a configurable runtime.
   --
   --  Generates:
   --    Offset_To_Top_Ptr
   --      (Address!(Tag_Ptr!(This).all) - Offset_To_Top_Offset)

   function Build_Set_Predefined_Prim_Op_Address
     (Loc          : Source_Ptr;
      Tag_Node     : Node_Id;
      Position     : Uint;
      Address_Node : Node_Id) return Node_Id;
   --  Build code that saves the address of a virtual function in a given
   --  Position of the portion of the dispatch table associated with the
   --  predefined primitives of Tag. Called from Exp_Disp.Fill_DT_Entry
   --  and Exp_Disp.Fill_Secondary_DT_Entry. It is used for:
   --   1) Filling the dispatch table of CPP_Class types.
   --   2) Late overriding (see Check_Dispatching_Operation).
   --
   --  Generates: Predefined_DT (Tag).D (Position) := Value

   function Build_Set_Prim_Op_Address
     (Loc          : Source_Ptr;
      Typ          : Entity_Id;
      Tag_Node     : Node_Id;
      Position     : Uint;
      Address_Node : Node_Id) return Node_Id;
   --  Build code that saves the address of a virtual function in a given
   --  Position of the dispatch table associated with the Tag. Called from
   --  Exp_Disp.Fill_DT_Entry and Exp_Disp.Fill_Secondary_DT_Entry. Used for:
   --   1) Filling the dispatch table of CPP_Class types.
   --   2) Late overriding (see Check_Dispatching_Operation).
   --
   --  Generates: Tag.D (Position) := Value

   function Build_Set_Size_Function
     (Loc       : Source_Ptr;
      Tag_Node  : Node_Id;
      Size_Func : Entity_Id) return Node_Id;
   --  Build code that saves in the TSD the address of the function
   --  calculating _size of the object.

   function Build_Set_Static_Offset_To_Top
     (Loc          : Source_Ptr;
      Iface_Tag    : Node_Id;
      Offset_Value : Node_Id) return Node_Id;
   --  Build code that initialize the Offset_To_Top component of the
   --  secondary dispatch table referenced by Iface_Tag.
   --
   --  Generates:
   --    Offset_To_Top_Ptr
   --      (Address!(Tag_Ptr!(This).all) - Offset_To_Top_Offset).all
   --     := Offset_Value

end Exp_Atag;
