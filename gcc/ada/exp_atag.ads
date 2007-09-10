------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T A G                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2006-2007, Free Software Foundation, Inc.         --
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
     (Loc    : Source_Ptr;
      DT_Ptr : Entity_Id;
      Stmts  : List_Id);
   --  Ada 2005 (AI-345): Generate statements that are common between timed,
   --  asynchronous, and conditional select expansion.

   function Build_CW_Membership
     (Loc          : Source_Ptr;
      Obj_Tag_Node : Node_Id;
      Typ_Tag_Node : Node_Id) return Node_Id;
   --  Build code that returns true if Obj_Tag is in Typ_Tag'Class. Each DT
   --  has a table of ancestors and its inheritance level (Idepth). Obj is in
   --  Typ'Class if Typ'Tag is found in the table of ancestors referenced by
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
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id;
      Position : Uint) return Node_Id;
   --  Given a pointer to a dispatch table (T) and a position in the DT, build
   --  code that gets the address of the predefined virtual function stored in
   --  it (used for dispatching calls).
   --
   --  Generates: Predefined_DT (Tag).D (Position);

   function Build_Get_Prim_Op_Address
     (Loc      : Source_Ptr;
      Typ      : Entity_Id;
      Tag_Node : Node_Id;
      Position : Uint) return Node_Id;
   --  Build code that retrieves the address of the virtual function stored in
   --  a given position of the dispatch table (used for dispatching calls).
   --
   --  Generates: To_Tag (Tag).D (Position);

   function Build_Get_Transportable
     (Loc      : Source_Ptr;
      Tag_Node : Node_Id) return Node_Id;
   --  Build code that retrieves the value of the Transportable flag for
   --  the given Tag.
   --
   --  Generates: TSD (Tag).Transportable;

   function Build_Inherit_Predefined_Prims
     (Loc              : Source_Ptr;
      Old_Tag_Node     : Node_Id;
      New_Tag_Node     : Node_Id) return Node_Id;
   --  Build code that inherits the predefined primitives of the parent.
   --
   --  Generates: Predefined_DT (New_T).D (All_Predefined_Prims) :=
   --               Predefined_DT (Old_T).D (All_Predefined_Prims);
   --
   --  Required to build the dispatch tables with the 3.4 backend.

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

end Exp_Atag;
