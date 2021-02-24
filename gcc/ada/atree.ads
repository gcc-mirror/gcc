------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
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

--  This package defines the low-level representation of the tree used to
--  represent the Ada program internally. Syntactic and semantic information
--  is combined in this tree. There is no separate symbol table structure.

--  WARNING: There is a C++ version of this package. Any changes to this source
--  file must be properly reflected in the C++ header file atree.h.

--  Package Atree defines the basic structure of the tree and its nodes and
--  provides the basic abstract interface for manipulating the tree. Two other
--  packages use this interface to define the representation of Ada programs
--  using this tree format. The package Sinfo defines the basic representation
--  of the syntactic structure of the program, as output by the parser. The
--  package Einfo defines the semantic information that is added to the tree
--  nodes that represent declared entities (i.e. the information that is
--  described in a separate symbol table structure in some other compilers).

--  The front end of the compiler first parses the program and generates a
--  tree that is simply a syntactic representation of the program in abstract
--  syntax tree format. Subsequent processing in the front end traverses the
--  tree, transforming it in various ways and adding semantic information.

with Alloc;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Einfo.Entities; use Einfo.Entities;
with Types;          use Types;
with System;         use System;
with Table;
with Unchecked_Conversion;

package Atree is

   --  Access to node fields is generally done through the getters and setters
   --  in packages Sinfo.Nodes and Einfo.Entities, which are automatically
   --  generated (see Gen_IL.Gen). However, in specialized circumstances
   --  (examples are the circuit in generic instantiation to copy trees, and in
   --  the tree dump routine), it is useful to be able to do untyped
   --  traversals, and an internal package in Atree allows for direct untyped
   --  accesses in such cases.

   function Last_Node_Id return Node_Id;
   --  Returns Id of last allocated node Id

   function Node_Offsets_Address return System.Address;
   function Slots_Address return System.Address;
   --  Address of Node_Offsets.Table and Slots.Table. Used in Back_End for Gigi
   --  call.

   function Approx_Num_Nodes_And_Entities return Nat;
   --  This is an approximation to the number of nodes and entities allocated,
   --  used to determine sizes of hash tables.

   -----------------------
   -- Use of Empty Node --
   -----------------------

   --  The special Node_Id Empty is used to mark missing fields, similar to
   --  "null" in Ada. Whenever the syntax has an optional component, then the
   --  corresponding field will be set to Empty if the component is missing.

   --  Note: Empty is not used to describe an empty list. Instead in this
   --  case the node field contains a list which is empty, and these cases
   --  should be distinguished (essentially from a type point of view, Empty
   --  is a Node, not a list).

   --  Note: Empty does in fact correspond to an allocated node. The Nkind
   --  field of this node may be referenced. It contains N_Empty, which
   --  uniquely identifies the empty case. This allows the Nkind field to be
   --  dereferenced before the check for Empty which is sometimes useful. We
   --  also access certain other fields of Empty; see comments in
   --  Gen_IL.Gen.Gen_Nodes.

   -----------------------
   -- Use of Error Node --
   -----------------------

   --  The Error node is used during syntactic and semantic analysis to
   --  indicate that the corresponding piece of syntactic structure or
   --  semantic meaning cannot properly be represented in the tree because
   --  of an illegality in the program.

   --  If an Error node is encountered, then you know that a previous
   --  illegality has been detected. The proper reaction should be to
   --  avoid posting related cascaded error messages, and to propagate
   --  the Error node if necessary.

   ------------------------
   -- Current_Error_Node --
   ------------------------

   --  Current_Error_Node is a global variable indicating the current node
   --  that is being processed for the purposes of placing a compiler
   --  abort message. This is not necessarily perfectly accurate, it is
   --  just a reasonably accurate best guess. It is used to output the
   --  source location in the abort message by Comperr, and also to
   --  implement the d3 debugging flag.

   --  There are two ways this gets set. During parsing, when new source
   --  nodes are being constructed by calls to New_Node and New_Entity,
   --  either one of these calls sets Current_Error_Node to the newly
   --  created node. During semantic analysis, this mechanism is not
   --  used, and instead Current_Error_Node is set by the subprograms in
   --  Debug_A that mark the start and end of analysis/expansion of a
   --  node in the tree.

   --  Current_Error_Node is also used for other purposes. See, for example,
   --  Rtsfind.

   Current_Error_Node : Node_Id;
   --  Node to place compiler abort messages

   ------------------
   -- Error Counts --
   ------------------

   --  The following variables denote the count of errors of various kinds
   --  detected in the tree. Note that these might be more logically located in
   --  Err_Vars, but we put it here to deal with licensing issues (we need this
   --  to have the GPL exception licensing, since Check_Error_Detected can be
   --  called from units with this licensing).

   Serious_Errors_Detected : Nat := 0;
   --  This is a count of errors that are serious enough to stop expansion,
   --  and hence to prevent generation of an object file even if the
   --  switch -gnatQ is set. Initialized to zero at the start of compilation.
   --  Initialized for -gnatVa use, see comment above.

   --  WARNING: There is a matching C declaration of this variable in fe.h

   Total_Errors_Detected : Nat := 0;
   --  Number of errors detected so far. Includes count of serious errors and
   --  non-serious errors, so this value is always greater than or equal to the
   --  Serious_Errors_Detected value. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above.

   Warnings_Detected : Nat := 0;
   --  Number of warnings detected. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above. This
   --  count includes the count of style and info messages.

   Warning_Info_Messages : Nat := 0;
   --  Number of info messages generated as warnings. Info messages are never
   --  treated as errors (whether from use of the pragma, or the compiler
   --  switch -gnatwe).

   Report_Info_Messages : Nat := 0;
   --  Number of info messages generated as reports. Info messages are never
   --  treated as errors (whether from use of the pragma, or the compiler
   --  switch -gnatwe). Used under Spark_Mode to report proved checks.

   Check_Messages : Nat := 0;
   --  Number of check messages generated. Check messages are neither warnings
   --  nor errors.

   Warnings_Treated_As_Errors : Nat := 0;
   --  Number of warnings changed into errors as a result of matching a pattern
   --  given in a Warning_As_Error configuration pragma.

   Configurable_Run_Time_Violations : Nat := 0;
   --  Count of configurable run time violations so far. This is used to
   --  suppress certain cascaded error messages when we know that we may not
   --  have fully expanded some items, due to high integrity violations (e.g.
   --  the use of constructs not permitted by the library in use, or improper
   --  constructs in No_Run_Time mode).

   procedure Check_Error_Detected;
   --  When an anomaly is found in the tree, many semantic routines silently
   --  bail out, assuming that the anomaly was caused by a previously detected
   --  serious error (or configurable run time violation). This routine should
   --  be called in these cases, and will raise an exception if no such error
   --  has been detected. This ensures that the anomaly is never allowed to go
   --  unnoticed in legal programs.

   --------------------------------------------------
   -- Node Allocation and Modification Subprograms --
   --------------------------------------------------

   --  The following subprograms are used for constructing the tree in the
   --  first place, and then for subsequent modifications as required.

   procedure Initialize;
   --  Called at the start of compilation to make the entries for Empty and
   --  Error.

   procedure Lock;
   --  Called before the back end is invoked to lock the nodes table.
   --  Also called after Unlock to relock.

   procedure Unlock;
   --  Unlocks nodes table, in cases where the back end needs to modify it

   procedure Lock_Nodes;
   --  Called to lock node modifications when assertions are enabled; without
   --  assertions calling this subprogram has no effect. The initial state of
   --  the lock is unlocked.

   procedure Unlock_Nodes;
   --  Called to unlock node modifications when assertions are enabled; if
   --  assertions are not enabled calling this subprogram has no effect.

   function New_Node
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Node_Id;
   --  Allocates a completely new node with the given node type and source
   --  location values. All other fields are set to their standard defaults:
   --
   --    Empty for all FieldN fields
   --    False for all FlagN fields
   --
   --  The usual approach is to build a new node using this function and
   --  then, using the value returned, use the Set_xxx functions to set
   --  fields of the node as required. New_Node can only be used for
   --  non-entity nodes, i.e. it never generates an extended node.
   --
   --  If we are currently parsing, as indicated by a previous call to
   --  Set_Comes_From_Source_Default (True), then this call also resets
   --  the value of Current_Error_Node.

   function New_Entity
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Entity_Id;
   --  Similar to New_Node, except that it is used only for entity nodes
   --  and returns an extended node.

   procedure Set_Comes_From_Source_Default (Default : Boolean);
   --  Sets value of Comes_From_Source flag to be used in all subsequent
   --  New_Node and New_Entity calls until another call to this procedure
   --  changes the default. This value is set True during parsing and
   --  False during semantic analysis. This is also used to determine
   --  if New_Node and New_Entity should set Current_Error_Node.

   function Get_Comes_From_Source_Default return Boolean;
   pragma Inline (Get_Comes_From_Source_Default);
   --  Gets the current value of the Comes_From_Source flag

   procedure Preserve_Comes_From_Source (NewN, OldN : Node_Id);
   pragma Inline (Preserve_Comes_From_Source);
   --  When a node is rewritten, it is sometimes appropriate to preserve the
   --  original comes from source indication. This is true when the rewrite
   --  essentially corresponds to a transformation corresponding exactly to
   --  semantics in the reference manual. This procedure copies the setting
   --  of Comes_From_Source from OldN to NewN.

   procedure Change_Node (N : Node_Id; New_Kind : Node_Kind);
   --  This procedure replaces the given node by setting its Nkind field to the
   --  indicated value and resetting all other fields to their default values
   --  except for certain fields that are preserved (see body for details).

   procedure Copy_Node (Source, Destination : Node_Or_Entity_Id);
   --  Copy the entire contents of the source node to the destination node.
   --  The contents of the source node is not affected. If the source node
   --  has an extension, then the destination must have an extension also.
   --  The parent pointer of the destination and its list link, if any, are
   --  not affected by the copy. Note that parent pointers of descendants
   --  are not adjusted, so the descendants of the destination node after
   --  the Copy_Node is completed have dubious parent pointers. Note that
   --  this routine does NOT copy aspect specifications, the Has_Aspects
   --  flag in the returned node will always be False. The caller must deal
   --  with copying aspect specifications where this is required.

   function New_Copy (Source : Node_Id) return Node_Id;
   --  This function allocates a completely new node, and then initializes
   --  it by copying the contents of the source node into it. The contents of
   --  the source node is not affected. The target node is always marked as
   --  not being in a list (even if the source is a list member), and not
   --  overloaded. The new node will have an extension if the source has
   --  an extension. New_Copy (Empty) returns Empty, and New_Copy (Error)
   --  returns Error. Note that, unlike Copy_Separate_Tree, New_Copy does not
   --  recursively copy any descendants, so in general parent pointers are not
   --  set correctly for the descendants of the copied node. Both normal and
   --  extended nodes (entities) may be copied using New_Copy.

   function Relocate_Node (Source : Node_Id) return Node_Id;
   --  Source is a non-entity node that is to be relocated. A new node is
   --  allocated, and the contents of Source are copied to this node, using
   --  New_Copy. The parent pointers of descendants of the node are then
   --  adjusted to point to the relocated copy. The original node is not
   --  modified, but the parent pointers of its descendants are no longer
   --  valid. The new copy is always marked as not overloaded. This routine is
   --  used in conjunction with the tree rewrite routines (see descriptions of
   --  Replace/Rewrite).
   --
   --  Note that the resulting node has the same parent as the source node, and
   --  is thus still attached to the tree. It is valid for Source to be Empty,
   --  in which case Relocate_Node simply returns Empty as the result.

   function Copy_Separate_Tree (Source : Node_Id) return Node_Id;
   --  Given a node that is the root of a subtree, Copy_Separate_Tree copies
   --  the entire syntactic subtree, including recursively any descendants
   --  whose parent field references a copied node (descendants not linked to
   --  a copied node by the parent field are also copied.) The parent pointers
   --  in the copy are properly set. Copy_Separate_Tree (Empty/Error) returns
   --  Empty/Error. The new subtree does not share entities with the source,
   --  but has new entities with the same name.
   --
   --  Most of the time this routine is called on an unanalyzed tree, and no
   --  semantic information is copied. However, to ensure that no entities
   --  are shared between the two when the source is already analyzed, and
   --  that the result looks like an unanalyzed tree from the parser, Entity
   --  fields and Etype fields are set to Empty, and Analyzed flags set False.
   --
   --  In addition, Expanded_Name nodes are converted back into the original
   --  parser form (where they are Selected_Components), so that reanalysis
   --  does the right thing.

   function Copy_Separate_List (Source : List_Id) return List_Id;
   --  Applies Copy_Separate_Tree to each element of the Source list, returning
   --  a new list of the results of these copy operations.

   procedure Exchange_Entities (E1 : Entity_Id; E2 : Entity_Id);
   --  Exchange the contents of two entities. The parent pointers are switched
   --  as well as the Defining_Identifier fields in the parents, so that the
   --  entities point correctly to their original parents. The effect is thus
   --  to leave the tree completely unchanged in structure, except that the
   --  entity ID values of the two entities are interchanged. Neither of the
   --  two entities may be list members. Note that entities appear on two
   --  semantic chains: Homonym and Next_Entity: the corresponding links must
   --  be adjusted by the caller, according to context.

   procedure Extend_Node (Source : Node_Id);
   --  This turns a node into an entity; it function is used only by Sinfo.CN.

   type Ignored_Ghost_Record_Proc is access procedure (N : Node_Or_Entity_Id);

   procedure Set_Ignored_Ghost_Recording_Proc
     (Proc : Ignored_Ghost_Record_Proc);
   --  Register a procedure that is invoked when an ignored Ghost node or
   --  entity is created.

   type Report_Proc is access procedure (Target : Node_Id; Source : Node_Id);

   procedure Set_Reporting_Proc (Proc : Report_Proc);
   --  Register a procedure that is invoked when a node is allocated, replaced
   --  or rewritten.

   type Rewrite_Proc is access procedure (Target : Node_Id; Source : Node_Id);

   procedure Set_Rewriting_Proc (Proc : Rewrite_Proc);
   --  Register a procedure that is invoked when a node is rewritten

   type Traverse_Result is (Abandon, OK, OK_Orig, Skip);
   --  This is the type of the result returned by the Process function passed
   --  to Traverse_Func and Traverse_Proc. See below for details.

   subtype Traverse_Final_Result is Traverse_Result range Abandon .. OK;
   --  This is the type of the final result returned Traverse_Func, based on
   --  the results of Process calls. See below for details.

   generic
      with function Process (N : Node_Id) return Traverse_Result is <>;
   function Traverse_Func (Node : Node_Id) return Traverse_Final_Result;
   --  This is a generic function that, given the parent node for a subtree,
   --  traverses all syntactic nodes of this tree, calling the given function
   --  Process on each one, in pre order (i.e. top-down). The order of
   --  traversing subtrees is arbitrary. The traversal is controlled as follows
   --  by the result returned by Process:

   --    OK       The traversal continues normally with the syntactic
   --             children of the node just processed.

   --    OK_Orig  The traversal continues normally with the syntactic
   --             children of the original node of the node just processed.

   --    Skip     The children of the node just processed are skipped and
   --             excluded from the traversal, but otherwise processing
   --             continues elsewhere in the tree.

   --    Abandon  The entire traversal is immediately abandoned, and the
   --             original call to Traverse returns Abandon.

   --  The result returned by Traverse is Abandon if processing was terminated
   --  by a call to Process returning Abandon, otherwise it is OK (meaning that
   --  all calls to process returned either OK, OK_Orig, or Skip).

   generic
      with function Process (N : Node_Id) return Traverse_Result is <>;
   procedure Traverse_Proc (Node : Node_Id);
   pragma Inline (Traverse_Proc);
   --  This is the same as Traverse_Func except that no result is returned,
   --  i.e. Traverse_Func is called and the result is simply discarded.

   ---------------------------
   -- Node Access Functions --
   ---------------------------

   --  The following functions return the contents of the indicated field of
   --  the node referenced by the argument, which is a Node_Id.

   function No                           (N : Node_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with the Empty node. This allows notations
   --  like "if No (Variant_Part)" as opposed to "if Variant_Part = Empty".

   function Parent                       (N : Node_Id) return Node_Id;
   pragma Inline (Parent);
   --  Returns the parent of a node if the node is not a list member, or else
   --  the parent of the list containing the node if the node is a list member.

   function Paren_Count                  (N : Node_Id) return Nat;
   pragma Inline (Paren_Count);
   --  Number of parentheses that surround an expression

   function Present                      (N : Node_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with the Empty node. This allows notations
   --  like "if Present (Statement)" as opposed to "if Statement /= Empty".

   procedure Set_Original_Node         (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Original_Node);
   --  Note that this routine is used only in very peculiar cases. In normal
   --  cases, the Original_Node link is set by calls to Rewrite.

   procedure Set_Parent                (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Parent);

   procedure Set_Paren_Count           (N : Node_Id; Val : Nat);
   pragma Inline (Set_Paren_Count);

   ---------------------------
   -- Tree Rewrite Routines --
   ---------------------------

   --  During the compilation process it is necessary in a number of situations
   --  to rewrite the tree. In some cases, such rewrites do not affect the
   --  structure of the tree, for example, when an indexed component node is
   --  replaced by the corresponding call node (the parser cannot distinguish
   --  between these two cases).

   --  In other situations, the rewrite does affect the structure of the
   --  tree. Examples are the replacement of a generic instantiation by the
   --  instantiated spec and body, and the static evaluation of expressions.

   --  If such structural modifications are done by the expander, there are
   --  no difficulties, since the form of the tree after the expander has no
   --  special significance, except as input to the backend of the compiler.
   --  However, if these modifications are done by the semantic phase, then
   --  it is important that they be done in a manner which allows the original
   --  tree to be preserved. This is because tools like pretty printers need
   --  to have this original tree structure available.

   --  The subprograms in this section allow rewriting of the tree by either
   --  insertion of new nodes in an existing list, or complete replacement of
   --  a subtree. The resulting tree for most purposes looks as though it has
   --  been really changed, and there is no trace of the original. However,
   --  special subprograms, also defined in this section, allow the original
   --  tree to be reconstructed if necessary.

   --  For tree modifications done in the expander, it is permissible to
   --  destroy the original tree, although it is also allowable to use the
   --  tree rewrite routines where it is convenient to do so.

   procedure Mark_Rewrite_Insertion (New_Node : Node_Id);
   pragma Inline (Mark_Rewrite_Insertion);
   --  This procedure marks the given node as an insertion made during a tree
   --  rewriting operation. Only the root needs to be marked. The call does
   --  not do the actual insertion, which must be done using one of the normal
   --  list insertion routines. The node is treated normally in all respects
   --  except for its response to Is_Rewrite_Insertion. The function of these
   --  calls is to be able to get an accurate original tree. This helps the
   --  accuracy of Sprint.Sprint_Node, and in particular, when stubs are being
   --  generated, it is essential that the original tree be accurate.

   function Is_Rewrite_Insertion (Node : Node_Id) return Boolean;
   pragma Inline (Is_Rewrite_Insertion);
   --  Tests whether the given node was marked using Mark_Rewrite_Insertion.
   --  This is used in reconstructing the original tree (where such nodes are
   --  to be eliminated).

   procedure Rewrite (Old_Node, New_Node : Node_Id);
   --  This is used when a complete subtree is to be replaced. Old_Node is the
   --  root of the old subtree to be replaced, and New_Node is the root of the
   --  newly constructed replacement subtree. The actual mechanism is to swap
   --  the contents of these two nodes fixing up the parent pointers of the
   --  replaced node (we do not attempt to preserve parent pointers for the
   --  original node). Neither Old_Node nor New_Node can be extended nodes.
   --
   --  Note: New_Node may not contain references to Old_Node, for example as
   --  descendants, since the rewrite would make such references invalid. If
   --  New_Node does need to reference Old_Node, then these references should
   --  be to a relocated copy of Old_Node (see Relocate_Node procedure).
   --
   --  Note: The Original_Node function applied to Old_Node (which has now
   --  been replaced by the contents of New_Node), can be used to obtain the
   --  original node, i.e. the old contents of Old_Node.

   procedure Replace (Old_Node, New_Node : Node_Id);
   --  This is similar to Rewrite, except that the old value of Old_Node
   --  is not saved. New_Node should not be used after Replace.  The flag
   --  Is_Rewrite_Substitution will be False for the resulting node, unless
   --  it was already true on entry, and Original_Node will not return the
   --  original contents of the Old_Node, but rather the New_Node value.
   --  Replace also preserves the setting of Comes_From_Source.
   --
   --  Note that New_Node must not contain references to Old_Node, for example
   --  as descendants, since the rewrite would make such references invalid. If
   --  New_Node does need to reference Old_Node, then these references should
   --  be to a relocated copy of Old_Node (see Relocate_Node procedure).
   --
   --  Replace is used in certain circumstances where it is desirable to
   --  suppress any history of the rewriting operation. Notably, it is used
   --  when the parser has mis-classified a node (e.g. a task entry call
   --  that the parser has parsed as a procedure call).

   function Is_Rewrite_Substitution (Node : Node_Id) return Boolean;
   pragma Inline (Is_Rewrite_Substitution);
   --  Return True iff Node has been rewritten (i.e. if Node is the root
   --  of a subtree which was installed using Rewrite).

   function Original_Node (Node : Node_Id) return Node_Id;
   pragma Inline (Original_Node);
   --  If Node has not been rewritten, then returns its input argument
   --  unchanged, else returns the Node for the original subtree. See section
   --  in sinfo.ads for requirements on original nodes returned by this
   --  function.
   --
   --  Note: Parents are not preserved in original tree nodes that are
   --  retrieved in this way (i.e. their children may have children whose
   --  Parent pointers reference some other node).
   --
   --  Note: there is no direct mechanism for deleting an original node (in
   --  a manner that can be reversed later). One possible approach is to use
   --  Rewrite to substitute a null statement for the node to be deleted.

   type Node_Field_Set is array (Node_Field) of Boolean with Pack;

   type Entity_Field_Set is array (Entity_Field) of Boolean with Pack;

   procedure Reinit_Field_To_Zero (N : Node_Id; Field : Node_Field);
   procedure Reinit_Field_To_Zero (N : Node_Id; Field : Entity_Field);
   --  When a node is created, all fields are initialized to zero, even if zero
   --  is not a valid value of the field type. These procedures put the field
   --  back to its initial zero value. Note that you can't just do something
   --  like Set_Some_Field (N, 0), if Some_Field is of (say) type Uintp,
   --  because Uintp is a subrange that does not include 0.
   type Entity_Kind_Set is array (Entity_Kind) of Boolean with Pack;
   procedure Reinit_Field_To_Zero
     (N : Node_Id; Field : Entity_Field; Old_Ekind : Entity_Kind_Set);
   procedure Reinit_Field_To_Zero
     (N : Node_Id; Field : Entity_Field; Old_Ekind : Entity_Kind);
   --  Same as above, but assert that the old Ekind is as specified. We might
   --  want to get rid of these, but it's useful documentation while working on
   --  this.

   function Field_Is_Initial_Zero
     (N : Node_Id; Field : Node_Field) return Boolean;
   function Field_Is_Initial_Zero
     (N : Entity_Id; Field : Entity_Field) return Boolean;
   --  True if the field value is the initial zero value

   procedure Mutate_Nkind
     (N : Node_Id; Val : Node_Kind) with Inline;
   --  There is no Set_Nkind in Sinfo.Nodes. We use this instead. This is here,
   --  and has a different name, because it does some extra checking. Nkind is
   --  like a discriminant, in that it controls which fields exist, and that
   --  set of fields can be different for the new kind. Discriminants cannot be
   --  modified in Ada for that reason. The rule here is more flexible: Nkind
   --  can be modified. However, when Nkind is modified, fields that exist for
   --  the old kind, but not for the new kind will vanish. We require that all
   --  vanishing fields be set to their initial zero value before calling
   --  Mutate_Nkind. This is necessary, because the memory occupied by the
   --  vanishing fields might be used for totally unrelated fields in the new
   --  node. See Reinit_Field_To_Zero.

   procedure Mutate_Ekind
     (N : Entity_Id; Val : Entity_Kind) with Inline;
   --  Ekind is also like a discriminant, and is mostly treated as above (see
   --  Mutate_Nkind). However, there are a few cases where we set the Ekind
   --  from its initial E_Void value to something else, then set it back to
   --  E_Void, then back to the something else, and we expect the "something
   --  else" fields to retain their value. Two two "something else"s are not
   --  always the same; for example we change from E_Void, to E_Variable, to
   --  E_Void, to E_Constant. ????This needs to be fixed.

   procedure Print_Atree_Info (N : Node_Or_Entity_Id);
   --  Called from Treepr to print out information about N that is private to
   --  Atree.

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the Atree package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package Atree_Private_Part is

      pragma Assert (Node_Kind'Pos (N_Unused_At_Start) = 0);
      pragma Assert (Empty_List_Or_Node = 0);
      pragma Assert (Entity_Kind'Pos (E_Void) = 0);
      --  We want nodes initialized to zero bits by default

      -------------------------
      -- Tree Representation --
      -------------------------

      --  The nodes of the tree are stored in two tables (i.e. growable
      --  arrays).

      --  A Node_Id points to an element of Nodes, which contains a
      --  Field_Offset that points to an element of Slots. Each slot can
      --  contain a single 32-bit field, or multiple smaller fields.
      --  An n-bit field is aligned on an n-bit boundary. The size of a node is
      --  the number of slots, which can range from 1 up to however many are
      --  needed.
      --
      --  The reason for the extra level of indirection is that Copy_Node,
      --  Exchange_Entities, and Rewrite all assume that nodes can be modified
      --  in place.

      subtype Node_Offset is Field_Offset'Base
        range 1 .. Field_Offset'Base'Last;

      package Node_Offsets is new Table.Table
        (Table_Component_Type => Node_Offset,
         Table_Index_Type     => Node_Id'Base,
         Table_Low_Bound      => First_Node_Id,
         Table_Initial        => Alloc.Node_Offsets_Initial,
         Table_Increment      => Alloc.Node_Offsets_Increment,
         Table_Name           => "Node_Offsets");

      --  We define the type Slot as a 32-bit modular integer. It is logically
      --  split into the appropriate numbers of components of appropriate size,
      --  but this splitting is not explicit because packed arrays cannot be
      --  properly interfaced in C/C++ and packed records are way too slow.

      Slot_Size : constant := 32;
      type Slot is mod 2**Slot_Size;
      for Slot'Size use Slot_Size;
      pragma Provide_Shift_Operators (Slot);

      type Field_1_Bit  is mod 2**1;
      type Field_2_Bit  is mod 2**2;
      type Field_4_Bit  is mod 2**4;
      type Field_8_Bit  is mod 2**8;
      type Field_32_Bit is mod 2**32;

      Slots_Low_Bound : constant Field_Offset := Field_Offset'First + 1;

      package Slots is new Table.Table
        (Table_Component_Type => Slot,
         Table_Index_Type     => Node_Offset'Base,
         Table_Low_Bound      => Slots_Low_Bound,
         Table_Initial        => Alloc.Slots_Initial,
         Table_Increment      => Alloc.Slots_Increment,
         Table_Name           => "Slots");
      --  Note that Table_Low_Bound is set such that if we try to access
      --  Slots.Table (0), we will get Constraint_Error.

      Noff : Node_Offsets.Table_Ptr renames Node_Offsets.Table;
      function Nlast return Node_Id'Base renames Node_Offsets.Last;
      Lots : Slots.Table_Ptr renames Slots.Table;
      function Slast return Node_Offset'Base renames Slots.Last;
      --  Work around limitations of gdb; it can't find Node_Offsets.Table,
      --  etc, without a full expanded name.

      function Alloc_Node_Id return Node_Id with Inline;

      function Alloc_Slots (Num_Slots : Field_Offset) return Node_Offset
        with Inline;

      --  Each of the following Get_N_Bit_Field functions fetches the field of
      --  the given Field_Type at the given offset. Field_Type'Size must be N.
      --  The offset is measured in units of Field_Type'Size. Likewise for the
      --  Set_N_Bit_Field procedures.

      generic
         type Field_Type is private;
      function Get_1_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;

      generic
         type Field_Type is private;
      function Get_2_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;

      generic
         type Field_Type is private;
      function Get_4_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;

      generic
         type Field_Type is private;
      function Get_8_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;

      generic
         type Field_Type is private;
      function Get_32_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;

      generic
         type Field_Type is private;
         Default_Val : Field_Type;
      function Get_32_Bit_Field_With_Default
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_Type
         with Inline;
      --  If the field has not yet been set, return Default_Val

      generic
         type Field_Type is private;
      procedure Set_1_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
         with Inline;

      generic
         type Field_Type is private;
      procedure Set_2_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
         with Inline;

      generic
         type Field_Type is private;
      procedure Set_4_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
         with Inline;

      generic
         type Field_Type is private;
      procedure Set_8_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
         with Inline;

      generic
         type Field_Type is private;
      procedure Set_32_Bit_Field
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_Type)
         with Inline;

      --  The following are similar to the above generics, but are not generic,
      --  and work with the low-level Field_n_bit types. If generics could be
      --  overloaded, we would use the same names.

      function Get_1_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_1_Bit
         with Inline;

      function Get_2_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_2_Bit
         with Inline;

      function Get_4_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_4_Bit
         with Inline;

      function Get_8_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_8_Bit
         with Inline;

      function Get_32_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset) return Field_32_Bit
         with Inline;

      procedure Set_1_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_1_Bit)
         with Inline;

      procedure Set_2_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_2_Bit)
         with Inline;

      procedure Set_4_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_4_Bit)
         with Inline;

      procedure Set_8_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_8_Bit)
         with Inline;

      procedure Set_32_Bit_Val
        (N : Node_Or_Entity_Id; Offset : Field_Offset; Val : Field_32_Bit)
         with Inline;

      procedure Validate_Node (N : Node_Or_Entity_Id);
      procedure Validate_Node_Write (N : Node_Or_Entity_Id);

      function Is_Valid_Node (U : Union_Id) return Boolean;

   end Atree_Private_Part;

end Atree;
