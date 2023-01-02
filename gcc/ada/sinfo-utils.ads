------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           S I N F O . U T I L S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2020-2023, Free Software Foundation, Inc.        --
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

with Sinfo.Nodes; use Sinfo.Nodes;

package Sinfo.Utils is

   -------------------------------
   -- Parent-related operations --
   -------------------------------

   procedure Copy_Parent (To, From : Node_Or_Entity_Id);
   --  Does Set_Parent (To, Parent (From)), except that if To or From are
   --  empty, does nothing. If From is empty but To is not, then Parent (To)
   --  should already be Empty.

   function Parent_Kind (N : Node_Id) return Node_Kind;
   --  Same as Nkind (Parent (N)), except if N is Empty, return N_Empty

   -------------------------
   -- Iterator Procedures --
   -------------------------

   --  The call to Next_xxx (N) is equivalent to N := Next_xxx (N)

   procedure Next_Entity       (N : in out Node_Id);
   procedure Next_Named_Actual (N : in out Node_Id);
   procedure Next_Rep_Item     (N : in out Node_Id);
   procedure Next_Use_Clause   (N : in out Node_Id);

   -------------------------------------------
   -- Miscellaneous Tree Access Subprograms --
   -------------------------------------------

   function First_Real_Statement -- ????
     (Ignored : N_Handled_Sequence_Of_Statements_Id) return Node_Id is (Empty);
   --  The First_Real_Statement field is going away, but it is referenced in
   --  codepeer and gnat-llvm. This is a temporary version, always returning
   --  Empty, to ease the transition.

   function End_Location (N : Node_Id) return Source_Ptr;
   --  N is an N_If_Statement or N_Case_Statement node, and this function
   --  returns the location of the IF token in the END IF sequence by
   --  translating the value of the End_Span field.

   --  WARNING: There is a matching C declaration of this subprogram in fe.h

   procedure Set_End_Location (N : Node_Id; S : Source_Ptr);
   --  N is an N_If_Statement or N_Case_Statement node. This procedure sets
   --  the End_Span field to correspond to the given value S. In other words,
   --  End_Span is set to the difference between S and Sloc (N), the starting
   --  location.

   function Get_Pragma_Arg (Arg : Node_Id) return Node_Id;
   --  Given an argument to a pragma Arg, this function returns the expression
   --  for the argument. This is Arg itself, or, in the case where Arg is a
   --  pragma argument association node, the expression from this node.

   -----------------------
   -- Utility Functions --
   -----------------------

   procedure Map_Pragma_Name (From, To : Name_Id);
   --  Used in the implementation of pragma Rename_Pragma. Maps pragma name
   --  From to pragma name To, so From can be used as a synonym for To.

   Too_Many_Pragma_Mappings : exception;
   --  Raised if Map_Pragma_Name is called too many times. We expect that few
   --  programs will use it at all, and those that do will use it approximately
   --  once or twice.

   function Pragma_Name (N : Node_Id) return Name_Id;
   --  Obtain the name of pragma N from the Chars field of its identifier. If
   --  the pragma has been renamed using Rename_Pragma, this routine returns
   --  the name of the renaming.

   function Pragma_Name_Unmapped (N : Node_Id) return Name_Id;
   --  Obtain the name of pragma N from the Chars field of its identifier. This
   --  form of name extraction does not take into account renamings performed
   --  by Rename_Pragma.

   generic
      with procedure Action (U : Union_Id);
   procedure Walk_Sinfo_Fields (N : Node_Id);
   --  Walk the Sinfo fields of N, for all field types that Union_Id includes,
   --  and call Action on each one. However, skip the Link field, which is the
   --  Parent, and would cause us to wander off into the weeds.

   generic
      with function Transform (U : Union_Id) return Union_Id;
   procedure Walk_Sinfo_Fields_Pairwise (N1, N2 : Node_Id);
   --  Walks the Sinfo fields of N1 and N2 pairwise, calls Tranform on each N2
   --  field, copying the resut into the corresponding field of N1. The Nkinds
   --  must match. Link is skipped.

   -------------------------------------------
   -- Aliases for Entity_Or_Associated_Node --
   -------------------------------------------

   --  Historically, the Entity, Associated_Node, and Entity_Or_Associated_Node
   --  fields shared the same slot. A further complication is that there is an
   --  N_Has_Entity that does not include all node types that have the Entity
   --  field. N_Inclusive_Has_Entity are the node types that have the Entity
   --  field.

   subtype N_Inclusive_Has_Entity is Node_Id with Predicate =>
     N_Inclusive_Has_Entity in
       N_Has_Entity_Id
       | N_Attribute_Definition_Clause_Id
       | N_Aspect_Specification_Id
       | N_Freeze_Entity_Id
       | N_Freeze_Generic_Entity_Id;

   subtype N_Has_Associated_Node is Node_Id with Predicate =>
     N_Has_Associated_Node in
       N_Has_Entity_Id
       | N_Aggregate_Id
       | N_Extension_Aggregate_Id
       | N_Selected_Component_Id
       | N_Use_Package_Clause_Id;

   function Associated_Node
     (N : N_Has_Associated_Node) return Node_Id
      renames Entity_Or_Associated_Node;

   function Entity
     (N : N_Inclusive_Has_Entity) return Node_Id
      renames Entity_Or_Associated_Node;

   procedure Set_Associated_Node
     (N : N_Has_Associated_Node; Val : Node_Id)
      renames Set_Entity_Or_Associated_Node;

   procedure Set_Entity
     (N : N_Inclusive_Has_Entity; Val : Node_Id)
      renames Set_Entity_Or_Associated_Node;

   ---------------
   -- Debugging --
   ---------------

   procedure New_Node_Debugging_Output (N : Node_Id);
   pragma Inline (New_Node_Debugging_Output);
   --  See package body for documentation

end Sinfo.Utils;
