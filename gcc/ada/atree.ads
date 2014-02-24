------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                A T R E E                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Sinfo;  use Sinfo;
with Einfo;  use Einfo;
with Namet;  use Namet;
with Types;  use Types;
with Snames; use Snames;
with System; use System;
with Table;
with Uintp;  use Uintp;
with Urealp; use Urealp;
with Unchecked_Conversion;

package Atree is

--  This package defines the format of the tree used to represent the Ada
--  program internally. Syntactic and semantic information is combined in
--  this tree. There is no separate symbol table structure.

--  WARNING: There is a C version of this package. Any changes to this source
--  file must be properly reflected in the C header file atree.h

--  Package Atree defines the basic structure of the tree and its nodes and
--  provides the basic abstract interface for manipulating the tree. Two other
--  packages use this interface to define the representation of Ada programs
--  using this tree format. The package Sinfo defines the basic representation
--  of the syntactic structure of the program, as output by the parser. The
--  package Einfo defines the semantic information which is added to the tree
--  nodes that represent declared entities (i.e. the information which might
--  typically be described in a separate symbol table structure).

--  The front end of the compiler first parses the program and generates a
--  tree that is simply a syntactic representation of the program in abstract
--  syntax tree format. Subsequent processing in the front end traverses the
--  tree, transforming it in various ways and adding semantic information.

   ----------------------
   -- Size of Entities --
   ----------------------

   --  Currently entities are composed of 6 sequentially allocated 32-byte
   --  nodes, considered as a single record. The following definition gives
   --  the number of extension nodes.

   Num_Extension_Nodes : Node_Id := 5;
   --  This value is increased by one if debug flag -gnatd.N is set. This is
   --  for testing performance impact of adding a new extension node. We make
   --  this of type Node_Id for easy reference in loops using this value.

   ----------------------------------------
   -- Definitions of Fields in Tree Node --
   ----------------------------------------

   --  The representation of the tree is completely hidden, using a functional
   --  interface for accessing and modifying the contents of nodes. Logically
   --  a node contains a number of fields, much as though the nodes were
   --  defined as a record type. The fields in a node are as follows:

   --   Nkind         Indicates the kind of the node. This field is present
   --                 in all nodes. The type is Node_Kind, which is declared
   --                 in the package Sinfo.

   --   Sloc          Location (Source_Ptr) of the corresponding token
   --                 in the Source buffer. The individual node definitions
   --                 show which token is referenced by this pointer.

   --   In_List       A flag used to indicate if the node is a member
   --                 of a node list.

   --   Rewrite_Ins   A flag set if a node is marked as a rewrite inserted
   --                 node as a result of a call to Mark_Rewrite_Insertion.

   --   Paren_Count   A 2-bit count used in sub-expression nodes to indicate
   --                 the level of parentheses. The settings are 0,1,2 and
   --                 3 for many. If the value is 3, then an auxiliary table
   --                 is used to indicate the real value. Set to zero for
   --                 non-subexpression nodes.

   --                 Note: the required parentheses surrounding conditional
   --                 and quantified expressions count as a level of parens
   --                 for this purpose, so e.g. in X := (if A then B else C);
   --                 Paren_Count for the right side will be 1.

   --   Comes_From_Source
   --                 This flag is present in all nodes. It is set if the
   --                 node is built by the scanner or parser, and clear if
   --                 the node is built by the analyzer or expander. It
   --                 indicates that the node corresponds to a construct
   --                 that appears in the original source program.

   --   Analyzed      This flag is present in all nodes. It is set when
   --                 a node is analyzed, and is used to avoid analyzing
   --                 the same node twice. Analysis includes expansion if
   --                 expansion is active, so in this case if the flag is
   --                 set it means the node has been analyzed and expanded.

   --   Error_Posted  This flag is present in all nodes. It is set when
   --                 an error message is posted which is associated with
   --                 the flagged node. This is used to avoid posting more
   --                 than one message on the same node.

   --   Field1
   --   Field2
   --   Field3
   --   Field4
   --   Field5        Five fields holding Union_Id values

   --   ElistN        Synonym for FieldN typed as Elist_Id (Empty = No_Elist)
   --   ListN         Synonym for FieldN typed as List_Id
   --   NameN         Synonym for FieldN typed as Name_Id
   --   NodeN         Synonym for FieldN typed as Node_Id
   --   StrN          Synonym for FieldN typed as String_Id
   --   UintN         Synonym for FieldN typed as Uint (Empty = Uint_0)
   --   UrealN        Synonym for FieldN typed as Ureal

   --   Note: in the case of ElistN and UintN fields, it is common that we
   --   end up with a value of Union_Id'(0) as the default value. This value
   --   is meaningless as a Uint or Elist_Id value. We have two choices here.
   --   We could require that all Uint and Elist fields be initialized to an
   --   appropriate value, but that's error prone, since it would be easy to
   --   miss an initialization. So instead we have the retrieval functions
   --   generate an appropriate default value (Uint_0 or No_Elist). Probably
   --   it would be cleaner to generate No_Uint in the Uint case but we got
   --   stuck with representing an "unset" size value as zero early on, and
   --   it will take a bit of fiddling to change that ???

   --   Note: the actual usage of FieldN (i.e. whether it contains a Elist_Id,
   --   List_Id, Name_Id, Node_Id, String_Id, Uint or Ureal) depends on the
   --   value in Nkind. Generally the access to this field is always via the
   --   functional interface, so the field names ElistN, ListN, NameN, NodeN,
   --   StrN, UintN and UrealN are used only in the bodies of the access
   --   functions (i.e. in the bodies of Sinfo and Einfo). These access
   --   functions contain debugging code that checks that the use is
   --   consistent with Nkind and Ekind values.

   --   However, in specialized circumstances (examples are the circuit in
   --   generic instantiation to copy trees, and in the tree dump routine),
   --   it is useful to be able to do untyped traversals, and an internal
   --   package in Atree allows for direct untyped accesses in such cases.

   --   Flag0         Nineteen Boolean flags (use depends on Nkind and
   --   Flag1         Ekind, as described for FieldN). Again the access
   --   Flag2         is usually via subprograms in Sinfo and Einfo which
   --   Flag3         provide high-level synonyms for these flags, and
   --   Flag4         contain debugging code that checks that the values
   --   Flag5         in Nkind and Ekind are appropriate for the access.
   --   Flag6
   --   Flag7
   --   Flag8
   --   Flag9
   --   Flag10
   --   Flag11        Note that Flag0-3 are stored separately in the Flags
   --   Flag12        table, but that's a detail of the implementation which
   --   Flag13        is entirely hidden by the funcitonal interface.
   --   Flag14
   --   Flag15
   --   Flag16
   --   Flag17
   --   Flag18

   --   Link          For a node, points to the Parent. For a list, points
   --                 to the list header. Note that in the latter case, a
   --                 client cannot modify the link field. This field is
   --                 private to the Atree package (but is also modified
   --                 by the Nlists package).

   --  The following additional fields are present in extended nodes used
   --  for entities (Nkind in N_Entity).

   --   Ekind         Entity type. This field indicates the type of the
   --                 entity, it is of type Entity_Kind which is defined
   --                 in package Einfo.

   --   Flag19        299 additional flags
   --   ...
   --   Flag317

   --   Convention    Entity convention (Convention_Id value)

   --   Field6        Additional Union_Id value stored in tree

   --   Node6         Synonym for Field6 typed as Node_Id
   --   Elist6        Synonym for Field6 typed as Elist_Id (Empty = No_Elist)
   --   Uint6         Synonym for Field6 typed as Uint (Empty = Uint_0)

   --   Similar definitions for Field7 to Field35 (and also Node7-Node35,
   --   Elist7-Elist35, Uint7-Uint35, Ureal7-Ureal35). Note that not all
   --   these functions are defined, only the ones that are actually used.

   function Last_Node_Id return Node_Id;
   pragma Inline (Last_Node_Id);
   --  Returns Id of last allocated node Id

   function Nodes_Address return System.Address;
   --  Return address of Nodes table (used in Back_End for Gigi call)

   function Flags_Address return System.Address;
   --  Return address of Flags table (used in Back_End for Gigi call)

   function Num_Nodes return Nat;
   --  Total number of nodes allocated, where an entity counts as a single
   --  node. This count is incremented every time a node or entity is
   --  allocated, and decremented every time a node or entity is deleted.
   --  This value is used by Xref and by Treepr to allocate hash tables of
   --  suitable size for hashing Node_Id values.

   -----------------------
   -- Use of Empty Node --
   -----------------------

   --  The special Node_Id Empty is used to mark missing fields. Whenever the
   --  syntax has an optional component, then the corresponding field will be
   --  set to Empty if the component is missing.

   --  Note: Empty is not used to describe an empty list. Instead in this
   --  case the node field contains a list which is empty, and these cases
   --  should be distinguished (essentially from a type point of view, Empty
   --  is a Node, and is thus not a list).

   --  Note: Empty does in fact correspond to an allocated node. Only the
   --  Nkind field of this node may be referenced. It contains N_Empty, which
   --  uniquely identifies the empty case. This allows the Nkind field to be
   --  dereferenced before the check for Empty which is sometimes useful.

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
   --  the error node if necessary.

   ------------------------
   -- Current_Error_Node --
   ------------------------

   --  The current error node is a global location indicating the current
   --  node that is being processed for the purposes of placing a compiler
   --  abort message. This is not necessarily perfectly accurate, it is
   --  just a reasonably accurate best guess. It is used to output the
   --  source location in the abort message by Comperr, and also to
   --  implement the d3 debugging flag. This is also used by Rtsfind
   --  to generate error messages for high integrity mode.

   --  There are two ways this gets set. During parsing, when new source
   --  nodes are being constructed by calls to New_Node and New_Entity,
   --  either one of these calls sets Current_Error_Node to the newly
   --  created node. During semantic analysis, this mechanism is not
   --  used, and instead Current_Error_Node is set by the subprograms in
   --  Debug_A that mark the start and end of analysis/expansion of a
   --  node in the tree.

   Current_Error_Node : Node_Id;
   --  Node to place error messages

   ------------------
   -- Error Counts --
   ------------------

   --  The following variables denote the count of errors of various kinds
   --  detected in the tree. Note that these might be more logically located
   --  in Err_Vars, but we put it to deal with licensing issues (we need this
   --  to have the GPL exception licensing, since Check_Error_Detected can
   --  be called from units with this licensing).

   Serious_Errors_Detected : Nat := 0;
   --  This is a count of errors that are serious enough to stop expansion,
   --  and hence to prevent generation of an object file even if the
   --  switch -gnatQ is set. Initialized to zero at the start of compilation.
   --  Initialized for -gnatVa use, see comment above.

   Total_Errors_Detected : Nat := 0;
   --  Number of errors detected so far. Includes count of serious errors and
   --  non-serious errors, so this value is always greater than or equal to the
   --  Serious_Errors_Detected value. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above.

   Warnings_Detected : Nat := 0;
   --  Number of warnings detected. Initialized to zero at the start of
   --  compilation. Initialized for -gnatVa use, see comment above.

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
   --  has been detected. This ensure that the anomaly is never allowed to go
   --  unnoticed.

   -------------------------------
   -- Default Setting of Fields --
   -------------------------------

   --  Nkind is set to N_Unused_At_Start

   --  Ekind is set to E_Void

   --  Sloc is always set, there is no default value

   --  Field1-5 fields are set to Empty

   --  Field6-35 fields in extended nodes are set to Empty

   --  Parent is set to Empty

   --  All Boolean flag fields are set to False

   --  Note: the value Empty is used in Field1-Field35 to indicate a null node.
   --  The usage varies. The common uses are to indicate absence of an optional
   --  clause or a completely unused Field1-35 field.

   -------------------------------------
   -- Use of Synonyms for Node Fields --
   -------------------------------------

   --  A subpackage Atree.Unchecked_Access provides routines for reading and
   --  writing the fields defined above (Field1-35, Node1-35, Flag0-317 etc).
   --  These unchecked access routines can be used for untyped traversals.
   --  In addition they are used in the implementations of the Sinfo and
   --  Einfo packages. These packages both provide logical synonyms for
   --  the generic fields, together with an appropriate set of access routines.
   --  Normally access to information within tree nodes uses these synonyms,
   --  providing a high level typed interface to the tree information.

   --------------------------------------------------
   -- Node Allocation and Modification Subprograms --
   --------------------------------------------------

   --  Generally the parser builds the tree and then it is further decorated
   --  (e.g. by setting the entity fields), but not fundamentally modified.
   --  However, there are cases in which the tree must be restructured by
   --  adding and rearranging nodes, as a result of disambiguating cases
   --  which the parser could not parse correctly, and adding additional
   --  semantic information (e.g. making constraint checks explicit). The
   --  following subprograms are used for constructing the tree in the first
   --  place, and then for subsequent modifications as required.

   procedure Initialize;
   --  Called at the start of compilation to initialize the allocation of
   --  the node and list tables and make the standard entries for Empty,
   --  Error and Error_List. Note that Initialize must not be called if
   --  Tree_Read is used.

   procedure Lock;
   --  Called before the back end is invoked to lock the nodes table
   --  Also called after Unlock to relock???

   procedure Unlock;
   --  Unlocks nodes table, in cases where the back end needs to modify it

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using the relevant
   --  Table.Tree_Read routines. Note that Initialize should not be called if
   --  Tree_Read is used. Tree_Read includes all necessary initialization.

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using the relevant
   --  Table.Tree_Write routines.

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

   function Has_Extension (N : Node_Id) return Boolean;
   pragma Inline (Has_Extension);
   --  Returns True if the given node has an extension (i.e. was created by
   --  a call to New_Entity rather than New_Node, and Nkind is in N_Entity)

   procedure Change_Node (N : Node_Id; New_Node_Kind : Node_Kind);
   --  This procedure replaces the given node by setting its Nkind field to
   --  the indicated value and resetting all other fields to their default
   --  values except for Sloc, which is unchanged, and the Parent pointer
   --  and list links, which are also unchanged. All other information in
   --  the original node is lost. The new node has an extension if the
   --  original node had an extension.

   procedure Copy_Node (Source : Node_Id; Destination : Node_Id);
   --  Copy the entire contents of the source node to the destination node.
   --  The contents of the source node is not affected. If the source node
   --  has an extension, then the destination must have an extension also.
   --  The parent pointer of the destination and its list link, if any, are
   --  not affected by the copy. Note that parent pointers of descendents
   --  are not adjusted, so the descendents of the destination node after
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
   --  recursively copy any descendents, so in general parent pointers are not
   --  set correctly for the descendents of the copied node. Both normal and
   --  extended nodes (entities) may be copied using New_Copy.

   function Relocate_Node (Source : Node_Id) return Node_Id;
   --  Source is a non-entity node that is to be relocated. A new node is
   --  allocated, and the contents of Source are copied to this node, using
   --  New_Copy. The parent pointers of descendents of the node are then
   --  adjusted to point to the relocated copy. The original node is not
   --  modified, but the parent pointers of its descendents are no longer
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
   --  but has new entities with the same name. Most of the time this routine
   --  is called on an unanalyzed tree, and no semantic information is copied.
   --  However, to ensure that no entities are shared between the two when the
   --  source is already analyzed, entity fields in the copy are zeroed out,
   --  as well as Etype fields and the Analyzed flag.
   --  In addition, Expanded_Name nodes are converted back into the original
   --  parser form (where they are Selected_Components), so that renalysis does
   --  the right thing.

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

   function Extend_Node (Node : Node_Id) return Entity_Id;
   --  This function returns a copy of its input node with an extension added.
   --  The fields of the extension are set to Empty. Due to the way extensions
   --  are handled (as four consecutive array elements), it may be necessary
   --  to reallocate the node, so that the returned value is not the same as
   --  the input value, but where possible the returned value will be the same
   --  as the input value (i.e. the extension will occur in place). It is the
   --  caller's responsibility to ensure that any pointers to the original node
   --  are appropriately updated. This function is used only by Sinfo.CN to
   --  change nodes into their corresponding entities.

   type Report_Proc is access procedure (Target : Node_Id; Source : Node_Id);

   procedure Set_Reporting_Proc (P : Report_Proc);
   --  Register a procedure that is invoked when a node is allocated, replaced
   --  or rewritten.

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

   function Nkind             (N : Node_Id) return Node_Kind;
   pragma Inline (Nkind);

   function Analyzed          (N : Node_Id) return Boolean;
   pragma Inline (Analyzed);

   function Has_Aspects       (N : Node_Id) return Boolean;
   pragma Inline (Has_Aspects);

   function Comes_From_Source (N : Node_Id) return Boolean;
   pragma Inline (Comes_From_Source);

   function Error_Posted      (N : Node_Id) return Boolean;
   pragma Inline (Error_Posted);

   function Sloc              (N : Node_Id) return Source_Ptr;
   pragma Inline (Sloc);

   function Paren_Count       (N : Node_Id) return Nat;
   pragma Inline (Paren_Count);

   function Parent            (N : Node_Id) return Node_Id;
   pragma Inline (Parent);
   --  Returns the parent of a node if the node is not a list member, or else
   --  the parent of the list containing the node if the node is a list member.

   function No                (N : Node_Id) return Boolean;
   pragma Inline (No);
   --  Tests given Id for equality with the Empty node. This allows notations
   --  like "if No (Variant_Part)" as opposed to "if Variant_Part = Empty".

   function Present           (N : Node_Id) return Boolean;
   pragma Inline (Present);
   --  Tests given Id for inequality with the Empty node. This allows notations
   --  like "if Present (Statement)" as opposed to "if Statement /= Empty".

   ---------------------
   -- Node_Kind Tests --
   ---------------------

   --  These are like the functions in Sinfo, but the first argument is a
   --  Node_Id, and the tested field is Nkind (N).

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind) return Boolean;

   function Nkind_In
     (N  : Node_Id;
      V1 : Node_Kind;
      V2 : Node_Kind;
      V3 : Node_Kind;
      V4 : Node_Kind;
      V5 : Node_Kind;
      V6 : Node_Kind;
      V7 : Node_Kind;
      V8 : Node_Kind;
      V9 : Node_Kind) return Boolean;

   pragma Inline (Nkind_In);
   --  Inline all above functions

   -----------------------
   -- Entity_Kind_Tests --
   -----------------------

   --  Utility functions to test whether an Entity_Kind value, either given
   --  directly as the first argument, or the Ekind field of an Entity give
   --  as the first argument, matches any of the given list of Entity_Kind
   --  values. Return True if any match, False if no match.

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind) return Boolean;

   function Ekind_In
     (E  : Entity_Id;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind;
      V8 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind) return Boolean;

   function Ekind_In
     (T  : Entity_Kind;
      V1 : Entity_Kind;
      V2 : Entity_Kind;
      V3 : Entity_Kind;
      V4 : Entity_Kind;
      V5 : Entity_Kind;
      V6 : Entity_Kind;
      V7 : Entity_Kind;
      V8 : Entity_Kind) return Boolean;

   pragma Inline (Ekind_In);
   --  Inline all above functions

   -----------------------------
   -- Entity Access Functions --
   -----------------------------

   --  The following functions apply only to Entity_Id values, i.e.
   --  to extended nodes.

   function Ekind (E : Entity_Id) return Entity_Kind;
   pragma Inline (Ekind);

   function Convention (E : Entity_Id) return Convention_Id;
   pragma Inline (Convention);

   ----------------------------
   -- Node Update Procedures --
   ----------------------------

   --  The following functions set a specified field in the node whose Id is
   --  passed as the first argument. The second parameter is the new value
   --  to be set in the specified field. Note that Set_Nkind is in the next
   --  section, since its use is restricted.

   procedure Set_Sloc         (N : Node_Id; Val : Source_Ptr);
   pragma Inline (Set_Sloc);

   procedure Set_Paren_Count  (N : Node_Id; Val : Nat);
   pragma Inline (Set_Paren_Count);

   procedure Set_Parent       (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Parent);

   procedure Set_Analyzed     (N : Node_Id; Val : Boolean := True);
   pragma Inline (Set_Analyzed);

   procedure Set_Error_Posted (N : Node_Id; Val : Boolean := True);
   pragma Inline (Set_Error_Posted);

   procedure Set_Comes_From_Source (N : Node_Id; Val : Boolean);
   pragma Inline (Set_Comes_From_Source);
   --  Note that this routine is very rarely used, since usually the
   --  default mechanism provided sets the right value, but in some
   --  unusual cases, the value needs to be reset (e.g. when a source
   --  node is copied, and the copy must not have Comes_From_Source set).

   procedure Set_Has_Aspects (N : Node_Id; Val : Boolean := True);
   pragma Inline (Set_Has_Aspects);

   procedure Set_Original_Node (N : Node_Id; Val : Node_Id);
   pragma Inline (Set_Original_Node);
   --  Note that this routine is used only in very peculiar cases. In normal
   --  cases, the Original_Node link is set by calls to Rewrite. We currently
   --  use it in ASIS mode to manually set the link from pragma expressions
   --  to their aspect original source expressions, so that the original source
   --  expressions accessed by ASIS are also semantically analyzed.

   ------------------------------
   -- Entity Update Procedures --
   ------------------------------

   --  The following procedures apply only to Entity_Id values, i.e.
   --  to extended nodes.

   procedure Basic_Set_Convention (E : Entity_Id; Val : Convention_Id);
   pragma Inline (Basic_Set_Convention);
   --  Clients should use Sem_Util.Set_Convention rather than calling this
   --  routine directly, as Set_Convention also deals with the special
   --  processing required for access types.

   procedure Set_Ekind (E : Entity_Id; Val : Entity_Kind);
   pragma Inline (Set_Ekind);

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
   --  descendents, since the rewrite would make such references invalid. If
   --  New_Node does need to reference Old_Node, then these references should
   --  be to a relocated copy of Old_Node (see Relocate_Node procedure).
   --
   --  Note: The Original_Node function applied to Old_Node (which has now
   --  been replaced by the contents of New_Node), can be used to obtain the
   --  original node, i.e. the old contents of Old_Node.

   procedure Replace (Old_Node, New_Node : Node_Id);
   --  This is similar to Rewrite, except that the old value of Old_Node is
   --  not saved, and the New_Node is deleted after the replace, since it
   --  is assumed that it can no longer be legitimately needed. The flag
   --  Is_Rewrite_Substitution will be False for the resulting node, unless
   --  it was already true on entry, and Original_Node will not return the
   --  original contents of the Old_Node, but rather the New_Node value (unless
   --  Old_Node had already been rewritten using Rewrite). Replace also
   --  preserves the setting of Comes_From_Source.
   --
   --  Note, New_Node may not contain references to Old_Node, for example as
   --  descendents, since the rewrite would make such references invalid. If
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
   --  unchanged, else returns the Node for the original subtree. Note that
   --  this is used extensively by ASIS on the trees constructed in ASIS mode
   --  to reconstruct the original semantic tree. See section in sinfo.ads
   --  for requirements on original nodes returned by this function.
   --
   --  Note: Parents are not preserved in original tree nodes that are
   --  retrieved in this way (i.e. their children may have children whose
   --  pointers which reference some other node). This needs more details???
   --
   --  Note: there is no direct mechanism for deleting an original node (in
   --  a manner that can be reversed later). One possible approach is to use
   --  Rewrite to substitute a null statement for the node to be deleted.

   -----------------------------------
   -- Generic Field Access Routines --
   -----------------------------------

   --  This subpackage provides the functions for accessing and procedures for
   --  setting fields that are normally referenced by wrapper subprograms (e.g.
   --  logical synonyms defined in packages Sinfo and Einfo, or specialized
   --  routines such as Rewrite (for Original_Node), or the node creation
   --  routines (for Set_Nkind). The implementations of these wrapper
   --  subprograms use the package Atree.Unchecked_Access as do various
   --  special case accesses where no wrapper applies. Documentation is always
   --  required for such a special case access explaining why it is needed.

   package Unchecked_Access is

      --  Functions to allow interpretation of Union_Id values as Uint and
      --  Ureal values.

      function To_Union is new Unchecked_Conversion (Uint,  Union_Id);
      function To_Union is new Unchecked_Conversion (Ureal, Union_Id);

      function From_Union is new Unchecked_Conversion (Union_Id, Uint);
      function From_Union is new Unchecked_Conversion (Union_Id, Ureal);

      --  Functions to fetch contents of indicated field. It is an error to
      --  attempt to read the value of a field which is not present.

      function Field1 (N : Node_Id) return Union_Id;
      pragma Inline (Field1);

      function Field2 (N : Node_Id) return Union_Id;
      pragma Inline (Field2);

      function Field3 (N : Node_Id) return Union_Id;
      pragma Inline (Field3);

      function Field4 (N : Node_Id) return Union_Id;
      pragma Inline (Field4);

      function Field5 (N : Node_Id) return Union_Id;
      pragma Inline (Field5);

      function Field6 (N : Node_Id) return Union_Id;
      pragma Inline (Field6);

      function Field7 (N : Node_Id) return Union_Id;
      pragma Inline (Field7);

      function Field8 (N : Node_Id) return Union_Id;
      pragma Inline (Field8);

      function Field9 (N : Node_Id) return Union_Id;
      pragma Inline (Field9);

      function Field10 (N : Node_Id) return Union_Id;
      pragma Inline (Field10);

      function Field11 (N : Node_Id) return Union_Id;
      pragma Inline (Field11);

      function Field12 (N : Node_Id) return Union_Id;
      pragma Inline (Field12);

      function Field13 (N : Node_Id) return Union_Id;
      pragma Inline (Field13);

      function Field14 (N : Node_Id) return Union_Id;
      pragma Inline (Field14);

      function Field15 (N : Node_Id) return Union_Id;
      pragma Inline (Field15);

      function Field16 (N : Node_Id) return Union_Id;
      pragma Inline (Field16);

      function Field17 (N : Node_Id) return Union_Id;
      pragma Inline (Field17);

      function Field18 (N : Node_Id) return Union_Id;
      pragma Inline (Field18);

      function Field19 (N : Node_Id) return Union_Id;
      pragma Inline (Field19);

      function Field20 (N : Node_Id) return Union_Id;
      pragma Inline (Field20);

      function Field21 (N : Node_Id) return Union_Id;
      pragma Inline (Field21);

      function Field22 (N : Node_Id) return Union_Id;
      pragma Inline (Field22);

      function Field23 (N : Node_Id) return Union_Id;
      pragma Inline (Field23);

      function Field24 (N : Node_Id) return Union_Id;
      pragma Inline (Field24);

      function Field25 (N : Node_Id) return Union_Id;
      pragma Inline (Field25);

      function Field26 (N : Node_Id) return Union_Id;
      pragma Inline (Field26);

      function Field27 (N : Node_Id) return Union_Id;
      pragma Inline (Field27);

      function Field28 (N : Node_Id) return Union_Id;
      pragma Inline (Field28);

      function Field29 (N : Node_Id) return Union_Id;
      pragma Inline (Field29);

      function Field30 (N : Node_Id) return Union_Id;
      pragma Inline (Field30);

      function Field31 (N : Node_Id) return Union_Id;
      pragma Inline (Field31);

      function Field32 (N : Node_Id) return Union_Id;
      pragma Inline (Field32);

      function Field33 (N : Node_Id) return Union_Id;
      pragma Inline (Field33);

      function Field34 (N : Node_Id) return Union_Id;
      pragma Inline (Field34);

      function Field35 (N : Node_Id) return Union_Id;
      pragma Inline (Field35);

      function Node1 (N : Node_Id) return Node_Id;
      pragma Inline (Node1);

      function Node2 (N : Node_Id) return Node_Id;
      pragma Inline (Node2);

      function Node3 (N : Node_Id) return Node_Id;
      pragma Inline (Node3);

      function Node4 (N : Node_Id) return Node_Id;
      pragma Inline (Node4);

      function Node5 (N : Node_Id) return Node_Id;
      pragma Inline (Node5);

      function Node6 (N : Node_Id) return Node_Id;
      pragma Inline (Node6);

      function Node7 (N : Node_Id) return Node_Id;
      pragma Inline (Node7);

      function Node8 (N : Node_Id) return Node_Id;
      pragma Inline (Node8);

      function Node9 (N : Node_Id) return Node_Id;
      pragma Inline (Node9);

      function Node10 (N : Node_Id) return Node_Id;
      pragma Inline (Node10);

      function Node11 (N : Node_Id) return Node_Id;
      pragma Inline (Node11);

      function Node12 (N : Node_Id) return Node_Id;
      pragma Inline (Node12);

      function Node13 (N : Node_Id) return Node_Id;
      pragma Inline (Node13);

      function Node14 (N : Node_Id) return Node_Id;
      pragma Inline (Node14);

      function Node15 (N : Node_Id) return Node_Id;
      pragma Inline (Node15);

      function Node16 (N : Node_Id) return Node_Id;
      pragma Inline (Node16);

      function Node17 (N : Node_Id) return Node_Id;
      pragma Inline (Node17);

      function Node18 (N : Node_Id) return Node_Id;
      pragma Inline (Node18);

      function Node19 (N : Node_Id) return Node_Id;
      pragma Inline (Node19);

      function Node20 (N : Node_Id) return Node_Id;
      pragma Inline (Node20);

      function Node21 (N : Node_Id) return Node_Id;
      pragma Inline (Node21);

      function Node22 (N : Node_Id) return Node_Id;
      pragma Inline (Node22);

      function Node23 (N : Node_Id) return Node_Id;
      pragma Inline (Node23);

      function Node24 (N : Node_Id) return Node_Id;
      pragma Inline (Node24);

      function Node25 (N : Node_Id) return Node_Id;
      pragma Inline (Node25);

      function Node26 (N : Node_Id) return Node_Id;
      pragma Inline (Node26);

      function Node27 (N : Node_Id) return Node_Id;
      pragma Inline (Node27);

      function Node28 (N : Node_Id) return Node_Id;
      pragma Inline (Node28);

      function Node29 (N : Node_Id) return Node_Id;
      pragma Inline (Node29);

      function Node30 (N : Node_Id) return Node_Id;
      pragma Inline (Node30);

      function Node31 (N : Node_Id) return Node_Id;
      pragma Inline (Node31);

      function Node32 (N : Node_Id) return Node_Id;
      pragma Inline (Node32);

      function Node33 (N : Node_Id) return Node_Id;
      pragma Inline (Node33);

      function Node34 (N : Node_Id) return Node_Id;
      pragma Inline (Node34);

      function Node35 (N : Node_Id) return Node_Id;
      pragma Inline (Node35);

      function List1 (N : Node_Id) return List_Id;
      pragma Inline (List1);

      function List2 (N : Node_Id) return List_Id;
      pragma Inline (List2);

      function List3 (N : Node_Id) return List_Id;
      pragma Inline (List3);

      function List4 (N : Node_Id) return List_Id;
      pragma Inline (List4);

      function List5 (N : Node_Id) return List_Id;
      pragma Inline (List5);

      function List10 (N : Node_Id) return List_Id;
      pragma Inline (List10);

      function List14 (N : Node_Id) return List_Id;
      pragma Inline (List14);

      function List25 (N : Node_Id) return List_Id;
      pragma Inline (List25);

      function Elist1 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist1);

      function Elist2 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist2);

      function Elist3 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist3);

      function Elist4 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist4);

      function Elist5 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist5);

      function Elist8 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist8);

      function Elist9 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist9);

      function Elist10 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist10);

      function Elist13 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist13);

      function Elist15 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist15);

      function Elist16 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist16);

      function Elist18 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist18);

      function Elist21 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist21);

      function Elist23 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist23);

      function Elist24 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist24);

      function Elist25 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist25);

      function Elist26 (N : Node_Id) return Elist_Id;
      pragma Inline (Elist26);

      function Name1 (N : Node_Id) return Name_Id;
      pragma Inline (Name1);

      function Name2 (N : Node_Id) return Name_Id;
      pragma Inline (Name2);

      function Str3 (N : Node_Id) return String_Id;
      pragma Inline (Str3);

      --  Note: the following Uintnn functions have a special test for the
      --  Field value being Empty. If an Empty value is found then Uint_0 is
      --  returned. This avoids the rather tricky requirement of initializing
      --  all Uint fields in nodes and entities.

      function Uint2 (N : Node_Id) return Uint;
      pragma Inline (Uint2);

      function Uint3 (N : Node_Id) return Uint;
      pragma Inline (Uint3);

      function Uint4 (N : Node_Id) return Uint;
      pragma Inline (Uint4);

      function Uint5 (N : Node_Id) return Uint;
      pragma Inline (Uint5);

      function Uint8 (N : Node_Id) return Uint;
      pragma Inline (Uint8);

      function Uint9 (N : Node_Id) return Uint;
      pragma Inline (Uint9);

      function Uint10 (N : Node_Id) return Uint;
      pragma Inline (Uint10);

      function Uint11 (N : Node_Id) return Uint;
      pragma Inline (Uint11);

      function Uint12 (N : Node_Id) return Uint;
      pragma Inline (Uint12);

      function Uint13 (N : Node_Id) return Uint;
      pragma Inline (Uint13);

      function Uint14 (N : Node_Id) return Uint;
      pragma Inline (Uint14);

      function Uint15 (N : Node_Id) return Uint;
      pragma Inline (Uint15);

      function Uint16 (N : Node_Id) return Uint;
      pragma Inline (Uint16);

      function Uint17 (N : Node_Id) return Uint;
      pragma Inline (Uint17);

      function Uint22 (N : Node_Id) return Uint;
      pragma Inline (Uint22);

      function Ureal3 (N : Node_Id) return Ureal;
      pragma Inline (Ureal3);

      function Ureal18 (N : Node_Id) return Ureal;
      pragma Inline (Ureal18);

      function Ureal21 (N : Node_Id) return Ureal;
      pragma Inline (Ureal21);

      function Flag0 (N : Node_Id) return Boolean;
      pragma Inline (Flag0);

      function Flag1 (N : Node_Id) return Boolean;
      pragma Inline (Flag1);

      function Flag2 (N : Node_Id) return Boolean;
      pragma Inline (Flag2);

      function Flag3 (N : Node_Id) return Boolean;
      pragma Inline (Flag3);

      function Flag4 (N : Node_Id) return Boolean;
      pragma Inline (Flag4);

      function Flag5 (N : Node_Id) return Boolean;
      pragma Inline (Flag5);

      function Flag6 (N : Node_Id) return Boolean;
      pragma Inline (Flag6);

      function Flag7 (N : Node_Id) return Boolean;
      pragma Inline (Flag7);

      function Flag8 (N : Node_Id) return Boolean;
      pragma Inline (Flag8);

      function Flag9 (N : Node_Id) return Boolean;
      pragma Inline (Flag9);

      function Flag10 (N : Node_Id) return Boolean;
      pragma Inline (Flag10);

      function Flag11 (N : Node_Id) return Boolean;
      pragma Inline (Flag11);

      function Flag12 (N : Node_Id) return Boolean;
      pragma Inline (Flag12);

      function Flag13 (N : Node_Id) return Boolean;
      pragma Inline (Flag13);

      function Flag14 (N : Node_Id) return Boolean;
      pragma Inline (Flag14);

      function Flag15 (N : Node_Id) return Boolean;
      pragma Inline (Flag15);

      function Flag16 (N : Node_Id) return Boolean;
      pragma Inline (Flag16);

      function Flag17 (N : Node_Id) return Boolean;
      pragma Inline (Flag17);

      function Flag18 (N : Node_Id) return Boolean;
      pragma Inline (Flag18);

      function Flag19 (N : Node_Id) return Boolean;
      pragma Inline (Flag19);

      function Flag20 (N : Node_Id) return Boolean;
      pragma Inline (Flag20);

      function Flag21 (N : Node_Id) return Boolean;
      pragma Inline (Flag21);

      function Flag22 (N : Node_Id) return Boolean;
      pragma Inline (Flag22);

      function Flag23 (N : Node_Id) return Boolean;
      pragma Inline (Flag23);

      function Flag24 (N : Node_Id) return Boolean;
      pragma Inline (Flag24);

      function Flag25 (N : Node_Id) return Boolean;
      pragma Inline (Flag25);

      function Flag26 (N : Node_Id) return Boolean;
      pragma Inline (Flag26);

      function Flag27 (N : Node_Id) return Boolean;
      pragma Inline (Flag27);

      function Flag28 (N : Node_Id) return Boolean;
      pragma Inline (Flag28);

      function Flag29 (N : Node_Id) return Boolean;
      pragma Inline (Flag29);

      function Flag30 (N : Node_Id) return Boolean;
      pragma Inline (Flag30);

      function Flag31 (N : Node_Id) return Boolean;
      pragma Inline (Flag31);

      function Flag32 (N : Node_Id) return Boolean;
      pragma Inline (Flag32);

      function Flag33 (N : Node_Id) return Boolean;
      pragma Inline (Flag33);

      function Flag34 (N : Node_Id) return Boolean;
      pragma Inline (Flag34);

      function Flag35 (N : Node_Id) return Boolean;
      pragma Inline (Flag35);

      function Flag36 (N : Node_Id) return Boolean;
      pragma Inline (Flag36);

      function Flag37 (N : Node_Id) return Boolean;
      pragma Inline (Flag37);

      function Flag38 (N : Node_Id) return Boolean;
      pragma Inline (Flag38);

      function Flag39 (N : Node_Id) return Boolean;
      pragma Inline (Flag39);

      function Flag40 (N : Node_Id) return Boolean;
      pragma Inline (Flag40);

      function Flag41 (N : Node_Id) return Boolean;
      pragma Inline (Flag41);

      function Flag42 (N : Node_Id) return Boolean;
      pragma Inline (Flag42);

      function Flag43 (N : Node_Id) return Boolean;
      pragma Inline (Flag43);

      function Flag44 (N : Node_Id) return Boolean;
      pragma Inline (Flag44);

      function Flag45 (N : Node_Id) return Boolean;
      pragma Inline (Flag45);

      function Flag46 (N : Node_Id) return Boolean;
      pragma Inline (Flag46);

      function Flag47 (N : Node_Id) return Boolean;
      pragma Inline (Flag47);

      function Flag48 (N : Node_Id) return Boolean;
      pragma Inline (Flag48);

      function Flag49 (N : Node_Id) return Boolean;
      pragma Inline (Flag49);

      function Flag50 (N : Node_Id) return Boolean;
      pragma Inline (Flag50);

      function Flag51 (N : Node_Id) return Boolean;
      pragma Inline (Flag51);

      function Flag52 (N : Node_Id) return Boolean;
      pragma Inline (Flag52);

      function Flag53 (N : Node_Id) return Boolean;
      pragma Inline (Flag53);

      function Flag54 (N : Node_Id) return Boolean;
      pragma Inline (Flag54);

      function Flag55 (N : Node_Id) return Boolean;
      pragma Inline (Flag55);

      function Flag56 (N : Node_Id) return Boolean;
      pragma Inline (Flag56);

      function Flag57 (N : Node_Id) return Boolean;
      pragma Inline (Flag57);

      function Flag58 (N : Node_Id) return Boolean;
      pragma Inline (Flag58);

      function Flag59 (N : Node_Id) return Boolean;
      pragma Inline (Flag59);

      function Flag60 (N : Node_Id) return Boolean;
      pragma Inline (Flag60);

      function Flag61 (N : Node_Id) return Boolean;
      pragma Inline (Flag61);

      function Flag62 (N : Node_Id) return Boolean;
      pragma Inline (Flag62);

      function Flag63 (N : Node_Id) return Boolean;
      pragma Inline (Flag63);

      function Flag64 (N : Node_Id) return Boolean;
      pragma Inline (Flag64);

      function Flag65 (N : Node_Id) return Boolean;
      pragma Inline (Flag65);

      function Flag66 (N : Node_Id) return Boolean;
      pragma Inline (Flag66);

      function Flag67 (N : Node_Id) return Boolean;
      pragma Inline (Flag67);

      function Flag68 (N : Node_Id) return Boolean;
      pragma Inline (Flag68);

      function Flag69 (N : Node_Id) return Boolean;
      pragma Inline (Flag69);

      function Flag70 (N : Node_Id) return Boolean;
      pragma Inline (Flag70);

      function Flag71 (N : Node_Id) return Boolean;
      pragma Inline (Flag71);

      function Flag72 (N : Node_Id) return Boolean;
      pragma Inline (Flag72);

      function Flag73 (N : Node_Id) return Boolean;
      pragma Inline (Flag73);

      function Flag74 (N : Node_Id) return Boolean;
      pragma Inline (Flag74);

      function Flag75 (N : Node_Id) return Boolean;
      pragma Inline (Flag75);

      function Flag76 (N : Node_Id) return Boolean;
      pragma Inline (Flag76);

      function Flag77 (N : Node_Id) return Boolean;
      pragma Inline (Flag77);

      function Flag78 (N : Node_Id) return Boolean;
      pragma Inline (Flag78);

      function Flag79 (N : Node_Id) return Boolean;
      pragma Inline (Flag79);

      function Flag80 (N : Node_Id) return Boolean;
      pragma Inline (Flag80);

      function Flag81 (N : Node_Id) return Boolean;
      pragma Inline (Flag81);

      function Flag82 (N : Node_Id) return Boolean;
      pragma Inline (Flag82);

      function Flag83 (N : Node_Id) return Boolean;
      pragma Inline (Flag83);

      function Flag84 (N : Node_Id) return Boolean;
      pragma Inline (Flag84);

      function Flag85 (N : Node_Id) return Boolean;
      pragma Inline (Flag85);

      function Flag86 (N : Node_Id) return Boolean;
      pragma Inline (Flag86);

      function Flag87 (N : Node_Id) return Boolean;
      pragma Inline (Flag87);

      function Flag88 (N : Node_Id) return Boolean;
      pragma Inline (Flag88);

      function Flag89 (N : Node_Id) return Boolean;
      pragma Inline (Flag89);

      function Flag90 (N : Node_Id) return Boolean;
      pragma Inline (Flag90);

      function Flag91 (N : Node_Id) return Boolean;
      pragma Inline (Flag91);

      function Flag92 (N : Node_Id) return Boolean;
      pragma Inline (Flag92);

      function Flag93 (N : Node_Id) return Boolean;
      pragma Inline (Flag93);

      function Flag94 (N : Node_Id) return Boolean;
      pragma Inline (Flag94);

      function Flag95 (N : Node_Id) return Boolean;
      pragma Inline (Flag95);

      function Flag96 (N : Node_Id) return Boolean;
      pragma Inline (Flag96);

      function Flag97 (N : Node_Id) return Boolean;
      pragma Inline (Flag97);

      function Flag98 (N : Node_Id) return Boolean;
      pragma Inline (Flag98);

      function Flag99 (N : Node_Id) return Boolean;
      pragma Inline (Flag99);

      function Flag100 (N : Node_Id) return Boolean;
      pragma Inline (Flag100);

      function Flag101 (N : Node_Id) return Boolean;
      pragma Inline (Flag101);

      function Flag102 (N : Node_Id) return Boolean;
      pragma Inline (Flag102);

      function Flag103 (N : Node_Id) return Boolean;
      pragma Inline (Flag103);

      function Flag104 (N : Node_Id) return Boolean;
      pragma Inline (Flag104);

      function Flag105 (N : Node_Id) return Boolean;
      pragma Inline (Flag105);

      function Flag106 (N : Node_Id) return Boolean;
      pragma Inline (Flag106);

      function Flag107 (N : Node_Id) return Boolean;
      pragma Inline (Flag107);

      function Flag108 (N : Node_Id) return Boolean;
      pragma Inline (Flag108);

      function Flag109 (N : Node_Id) return Boolean;
      pragma Inline (Flag109);

      function Flag110 (N : Node_Id) return Boolean;
      pragma Inline (Flag110);

      function Flag111 (N : Node_Id) return Boolean;
      pragma Inline (Flag111);

      function Flag112 (N : Node_Id) return Boolean;
      pragma Inline (Flag112);

      function Flag113 (N : Node_Id) return Boolean;
      pragma Inline (Flag113);

      function Flag114 (N : Node_Id) return Boolean;
      pragma Inline (Flag114);

      function Flag115 (N : Node_Id) return Boolean;
      pragma Inline (Flag115);

      function Flag116 (N : Node_Id) return Boolean;
      pragma Inline (Flag116);

      function Flag117 (N : Node_Id) return Boolean;
      pragma Inline (Flag117);

      function Flag118 (N : Node_Id) return Boolean;
      pragma Inline (Flag118);

      function Flag119 (N : Node_Id) return Boolean;
      pragma Inline (Flag119);

      function Flag120 (N : Node_Id) return Boolean;
      pragma Inline (Flag120);

      function Flag121 (N : Node_Id) return Boolean;
      pragma Inline (Flag121);

      function Flag122 (N : Node_Id) return Boolean;
      pragma Inline (Flag122);

      function Flag123 (N : Node_Id) return Boolean;
      pragma Inline (Flag123);

      function Flag124 (N : Node_Id) return Boolean;
      pragma Inline (Flag124);

      function Flag125 (N : Node_Id) return Boolean;
      pragma Inline (Flag125);

      function Flag126 (N : Node_Id) return Boolean;
      pragma Inline (Flag126);

      function Flag127 (N : Node_Id) return Boolean;
      pragma Inline (Flag127);

      function Flag128 (N : Node_Id) return Boolean;
      pragma Inline (Flag128);

      function Flag129 (N : Node_Id) return Boolean;
      pragma Inline (Flag129);

      function Flag130 (N : Node_Id) return Boolean;
      pragma Inline (Flag130);

      function Flag131 (N : Node_Id) return Boolean;
      pragma Inline (Flag131);

      function Flag132 (N : Node_Id) return Boolean;
      pragma Inline (Flag132);

      function Flag133 (N : Node_Id) return Boolean;
      pragma Inline (Flag133);

      function Flag134 (N : Node_Id) return Boolean;
      pragma Inline (Flag134);

      function Flag135 (N : Node_Id) return Boolean;
      pragma Inline (Flag135);

      function Flag136 (N : Node_Id) return Boolean;
      pragma Inline (Flag136);

      function Flag137 (N : Node_Id) return Boolean;
      pragma Inline (Flag137);

      function Flag138 (N : Node_Id) return Boolean;
      pragma Inline (Flag138);

      function Flag139 (N : Node_Id) return Boolean;
      pragma Inline (Flag139);

      function Flag140 (N : Node_Id) return Boolean;
      pragma Inline (Flag140);

      function Flag141 (N : Node_Id) return Boolean;
      pragma Inline (Flag141);

      function Flag142 (N : Node_Id) return Boolean;
      pragma Inline (Flag142);

      function Flag143 (N : Node_Id) return Boolean;
      pragma Inline (Flag143);

      function Flag144 (N : Node_Id) return Boolean;
      pragma Inline (Flag144);

      function Flag145 (N : Node_Id) return Boolean;
      pragma Inline (Flag145);

      function Flag146 (N : Node_Id) return Boolean;
      pragma Inline (Flag146);

      function Flag147 (N : Node_Id) return Boolean;
      pragma Inline (Flag147);

      function Flag148 (N : Node_Id) return Boolean;
      pragma Inline (Flag148);

      function Flag149 (N : Node_Id) return Boolean;
      pragma Inline (Flag149);

      function Flag150 (N : Node_Id) return Boolean;
      pragma Inline (Flag150);

      function Flag151 (N : Node_Id) return Boolean;
      pragma Inline (Flag151);

      function Flag152 (N : Node_Id) return Boolean;
      pragma Inline (Flag152);

      function Flag153 (N : Node_Id) return Boolean;
      pragma Inline (Flag153);

      function Flag154 (N : Node_Id) return Boolean;
      pragma Inline (Flag154);

      function Flag155 (N : Node_Id) return Boolean;
      pragma Inline (Flag155);

      function Flag156 (N : Node_Id) return Boolean;
      pragma Inline (Flag156);

      function Flag157 (N : Node_Id) return Boolean;
      pragma Inline (Flag157);

      function Flag158 (N : Node_Id) return Boolean;
      pragma Inline (Flag158);

      function Flag159 (N : Node_Id) return Boolean;
      pragma Inline (Flag159);

      function Flag160 (N : Node_Id) return Boolean;
      pragma Inline (Flag160);

      function Flag161 (N : Node_Id) return Boolean;
      pragma Inline (Flag161);

      function Flag162 (N : Node_Id) return Boolean;
      pragma Inline (Flag162);

      function Flag163 (N : Node_Id) return Boolean;
      pragma Inline (Flag163);

      function Flag164 (N : Node_Id) return Boolean;
      pragma Inline (Flag164);

      function Flag165 (N : Node_Id) return Boolean;
      pragma Inline (Flag165);

      function Flag166 (N : Node_Id) return Boolean;
      pragma Inline (Flag166);

      function Flag167 (N : Node_Id) return Boolean;
      pragma Inline (Flag167);

      function Flag168 (N : Node_Id) return Boolean;
      pragma Inline (Flag168);

      function Flag169 (N : Node_Id) return Boolean;
      pragma Inline (Flag169);

      function Flag170 (N : Node_Id) return Boolean;
      pragma Inline (Flag170);

      function Flag171 (N : Node_Id) return Boolean;
      pragma Inline (Flag171);

      function Flag172 (N : Node_Id) return Boolean;
      pragma Inline (Flag172);

      function Flag173 (N : Node_Id) return Boolean;
      pragma Inline (Flag173);

      function Flag174 (N : Node_Id) return Boolean;
      pragma Inline (Flag174);

      function Flag175 (N : Node_Id) return Boolean;
      pragma Inline (Flag175);

      function Flag176 (N : Node_Id) return Boolean;
      pragma Inline (Flag176);

      function Flag177 (N : Node_Id) return Boolean;
      pragma Inline (Flag177);

      function Flag178 (N : Node_Id) return Boolean;
      pragma Inline (Flag178);

      function Flag179 (N : Node_Id) return Boolean;
      pragma Inline (Flag179);

      function Flag180 (N : Node_Id) return Boolean;
      pragma Inline (Flag180);

      function Flag181 (N : Node_Id) return Boolean;
      pragma Inline (Flag181);

      function Flag182 (N : Node_Id) return Boolean;
      pragma Inline (Flag182);

      function Flag183 (N : Node_Id) return Boolean;
      pragma Inline (Flag183);

      function Flag184 (N : Node_Id) return Boolean;
      pragma Inline (Flag184);

      function Flag185 (N : Node_Id) return Boolean;
      pragma Inline (Flag185);

      function Flag186 (N : Node_Id) return Boolean;
      pragma Inline (Flag186);

      function Flag187 (N : Node_Id) return Boolean;
      pragma Inline (Flag187);

      function Flag188 (N : Node_Id) return Boolean;
      pragma Inline (Flag188);

      function Flag189 (N : Node_Id) return Boolean;
      pragma Inline (Flag189);

      function Flag190 (N : Node_Id) return Boolean;
      pragma Inline (Flag190);

      function Flag191 (N : Node_Id) return Boolean;
      pragma Inline (Flag191);

      function Flag192 (N : Node_Id) return Boolean;
      pragma Inline (Flag192);

      function Flag193 (N : Node_Id) return Boolean;
      pragma Inline (Flag193);

      function Flag194 (N : Node_Id) return Boolean;
      pragma Inline (Flag194);

      function Flag195 (N : Node_Id) return Boolean;
      pragma Inline (Flag195);

      function Flag196 (N : Node_Id) return Boolean;
      pragma Inline (Flag196);

      function Flag197 (N : Node_Id) return Boolean;
      pragma Inline (Flag197);

      function Flag198 (N : Node_Id) return Boolean;
      pragma Inline (Flag198);

      function Flag199 (N : Node_Id) return Boolean;
      pragma Inline (Flag199);

      function Flag200 (N : Node_Id) return Boolean;
      pragma Inline (Flag200);

      function Flag201 (N : Node_Id) return Boolean;
      pragma Inline (Flag201);

      function Flag202 (N : Node_Id) return Boolean;
      pragma Inline (Flag202);

      function Flag203 (N : Node_Id) return Boolean;
      pragma Inline (Flag203);

      function Flag204 (N : Node_Id) return Boolean;
      pragma Inline (Flag204);

      function Flag205 (N : Node_Id) return Boolean;
      pragma Inline (Flag205);

      function Flag206 (N : Node_Id) return Boolean;
      pragma Inline (Flag206);

      function Flag207 (N : Node_Id) return Boolean;
      pragma Inline (Flag207);

      function Flag208 (N : Node_Id) return Boolean;
      pragma Inline (Flag208);

      function Flag209 (N : Node_Id) return Boolean;
      pragma Inline (Flag209);

      function Flag210 (N : Node_Id) return Boolean;
      pragma Inline (Flag210);

      function Flag211 (N : Node_Id) return Boolean;
      pragma Inline (Flag211);

      function Flag212 (N : Node_Id) return Boolean;
      pragma Inline (Flag212);

      function Flag213 (N : Node_Id) return Boolean;
      pragma Inline (Flag213);

      function Flag214 (N : Node_Id) return Boolean;
      pragma Inline (Flag214);

      function Flag215 (N : Node_Id) return Boolean;
      pragma Inline (Flag215);

      function Flag216 (N : Node_Id) return Boolean;
      pragma Inline (Flag216);

      function Flag217 (N : Node_Id) return Boolean;
      pragma Inline (Flag217);

      function Flag218 (N : Node_Id) return Boolean;
      pragma Inline (Flag218);

      function Flag219 (N : Node_Id) return Boolean;
      pragma Inline (Flag219);

      function Flag220 (N : Node_Id) return Boolean;
      pragma Inline (Flag220);

      function Flag221 (N : Node_Id) return Boolean;
      pragma Inline (Flag221);

      function Flag222 (N : Node_Id) return Boolean;
      pragma Inline (Flag222);

      function Flag223 (N : Node_Id) return Boolean;
      pragma Inline (Flag223);

      function Flag224 (N : Node_Id) return Boolean;
      pragma Inline (Flag224);

      function Flag225 (N : Node_Id) return Boolean;
      pragma Inline (Flag225);

      function Flag226 (N : Node_Id) return Boolean;
      pragma Inline (Flag226);

      function Flag227 (N : Node_Id) return Boolean;
      pragma Inline (Flag227);

      function Flag228 (N : Node_Id) return Boolean;
      pragma Inline (Flag228);

      function Flag229 (N : Node_Id) return Boolean;
      pragma Inline (Flag229);

      function Flag230 (N : Node_Id) return Boolean;
      pragma Inline (Flag230);

      function Flag231 (N : Node_Id) return Boolean;
      pragma Inline (Flag231);

      function Flag232 (N : Node_Id) return Boolean;
      pragma Inline (Flag232);

      function Flag233 (N : Node_Id) return Boolean;
      pragma Inline (Flag233);

      function Flag234 (N : Node_Id) return Boolean;
      pragma Inline (Flag234);

      function Flag235 (N : Node_Id) return Boolean;
      pragma Inline (Flag235);

      function Flag236 (N : Node_Id) return Boolean;
      pragma Inline (Flag236);

      function Flag237 (N : Node_Id) return Boolean;
      pragma Inline (Flag237);

      function Flag238 (N : Node_Id) return Boolean;
      pragma Inline (Flag238);

      function Flag239 (N : Node_Id) return Boolean;
      pragma Inline (Flag239);

      function Flag240 (N : Node_Id) return Boolean;
      pragma Inline (Flag240);

      function Flag241 (N : Node_Id) return Boolean;
      pragma Inline (Flag241);

      function Flag242 (N : Node_Id) return Boolean;
      pragma Inline (Flag242);

      function Flag243 (N : Node_Id) return Boolean;
      pragma Inline (Flag243);

      function Flag244 (N : Node_Id) return Boolean;
      pragma Inline (Flag244);

      function Flag245 (N : Node_Id) return Boolean;
      pragma Inline (Flag245);

      function Flag246 (N : Node_Id) return Boolean;
      pragma Inline (Flag246);

      function Flag247 (N : Node_Id) return Boolean;
      pragma Inline (Flag247);

      function Flag248 (N : Node_Id) return Boolean;
      pragma Inline (Flag248);

      function Flag249 (N : Node_Id) return Boolean;
      pragma Inline (Flag249);

      function Flag250 (N : Node_Id) return Boolean;
      pragma Inline (Flag250);

      function Flag251 (N : Node_Id) return Boolean;
      pragma Inline (Flag251);

      function Flag252 (N : Node_Id) return Boolean;
      pragma Inline (Flag252);

      function Flag253 (N : Node_Id) return Boolean;
      pragma Inline (Flag253);

      function Flag254 (N : Node_Id) return Boolean;
      pragma Inline (Flag254);

      function Flag255 (N : Node_Id) return Boolean;
      pragma Inline (Flag255);

      function Flag256 (N : Node_Id) return Boolean;
      pragma Inline (Flag256);

      function Flag257 (N : Node_Id) return Boolean;
      pragma Inline (Flag257);

      function Flag258 (N : Node_Id) return Boolean;
      pragma Inline (Flag258);

      function Flag259 (N : Node_Id) return Boolean;
      pragma Inline (Flag259);

      function Flag260 (N : Node_Id) return Boolean;
      pragma Inline (Flag260);

      function Flag261 (N : Node_Id) return Boolean;
      pragma Inline (Flag261);

      function Flag262 (N : Node_Id) return Boolean;
      pragma Inline (Flag262);

      function Flag263 (N : Node_Id) return Boolean;
      pragma Inline (Flag263);

      function Flag264 (N : Node_Id) return Boolean;
      pragma Inline (Flag264);

      function Flag265 (N : Node_Id) return Boolean;
      pragma Inline (Flag265);

      function Flag266 (N : Node_Id) return Boolean;
      pragma Inline (Flag266);

      function Flag267 (N : Node_Id) return Boolean;
      pragma Inline (Flag267);

      function Flag268 (N : Node_Id) return Boolean;
      pragma Inline (Flag268);

      function Flag269 (N : Node_Id) return Boolean;
      pragma Inline (Flag269);

      function Flag270 (N : Node_Id) return Boolean;
      pragma Inline (Flag270);

      function Flag271 (N : Node_Id) return Boolean;
      pragma Inline (Flag271);

      function Flag272 (N : Node_Id) return Boolean;
      pragma Inline (Flag272);

      function Flag273 (N : Node_Id) return Boolean;
      pragma Inline (Flag273);

      function Flag274 (N : Node_Id) return Boolean;
      pragma Inline (Flag274);

      function Flag275 (N : Node_Id) return Boolean;
      pragma Inline (Flag275);

      function Flag276 (N : Node_Id) return Boolean;
      pragma Inline (Flag276);

      function Flag277 (N : Node_Id) return Boolean;
      pragma Inline (Flag277);

      function Flag278 (N : Node_Id) return Boolean;
      pragma Inline (Flag278);

      function Flag279 (N : Node_Id) return Boolean;
      pragma Inline (Flag279);

      function Flag280 (N : Node_Id) return Boolean;
      pragma Inline (Flag280);

      function Flag281 (N : Node_Id) return Boolean;
      pragma Inline (Flag281);

      function Flag282 (N : Node_Id) return Boolean;
      pragma Inline (Flag282);

      function Flag283 (N : Node_Id) return Boolean;
      pragma Inline (Flag283);

      function Flag284 (N : Node_Id) return Boolean;
      pragma Inline (Flag284);

      function Flag285 (N : Node_Id) return Boolean;
      pragma Inline (Flag285);

      function Flag286 (N : Node_Id) return Boolean;
      pragma Inline (Flag286);

      function Flag287 (N : Node_Id) return Boolean;
      pragma Inline (Flag287);

      function Flag288 (N : Node_Id) return Boolean;
      pragma Inline (Flag288);

      function Flag289 (N : Node_Id) return Boolean;
      pragma Inline (Flag289);

      function Flag290 (N : Node_Id) return Boolean;
      pragma Inline (Flag290);

      function Flag291 (N : Node_Id) return Boolean;
      pragma Inline (Flag291);

      function Flag292 (N : Node_Id) return Boolean;
      pragma Inline (Flag292);

      function Flag293 (N : Node_Id) return Boolean;
      pragma Inline (Flag293);

      function Flag294 (N : Node_Id) return Boolean;
      pragma Inline (Flag294);

      function Flag295 (N : Node_Id) return Boolean;
      pragma Inline (Flag295);

      function Flag296 (N : Node_Id) return Boolean;
      pragma Inline (Flag296);

      function Flag297 (N : Node_Id) return Boolean;
      pragma Inline (Flag297);

      function Flag298 (N : Node_Id) return Boolean;
      pragma Inline (Flag298);

      function Flag299 (N : Node_Id) return Boolean;
      pragma Inline (Flag299);

      function Flag300 (N : Node_Id) return Boolean;
      pragma Inline (Flag300);

      function Flag301 (N : Node_Id) return Boolean;
      pragma Inline (Flag301);

      function Flag302 (N : Node_Id) return Boolean;
      pragma Inline (Flag302);

      function Flag303 (N : Node_Id) return Boolean;
      pragma Inline (Flag303);

      function Flag304 (N : Node_Id) return Boolean;
      pragma Inline (Flag304);

      function Flag305 (N : Node_Id) return Boolean;
      pragma Inline (Flag305);

      function Flag306 (N : Node_Id) return Boolean;
      pragma Inline (Flag306);

      function Flag307 (N : Node_Id) return Boolean;
      pragma Inline (Flag307);

      function Flag308 (N : Node_Id) return Boolean;
      pragma Inline (Flag308);

      function Flag309 (N : Node_Id) return Boolean;
      pragma Inline (Flag309);

      function Flag310 (N : Node_Id) return Boolean;
      pragma Inline (Flag310);

      function Flag311 (N : Node_Id) return Boolean;
      pragma Inline (Flag311);

      function Flag312 (N : Node_Id) return Boolean;
      pragma Inline (Flag312);

      function Flag313 (N : Node_Id) return Boolean;
      pragma Inline (Flag313);

      function Flag314 (N : Node_Id) return Boolean;
      pragma Inline (Flag314);

      function Flag315 (N : Node_Id) return Boolean;
      pragma Inline (Flag315);

      function Flag316 (N : Node_Id) return Boolean;
      pragma Inline (Flag316);

      function Flag317 (N : Node_Id) return Boolean;
      pragma Inline (Flag317);

      --  Procedures to set value of indicated field

      procedure Set_Nkind (N : Node_Id; Val : Node_Kind);
      pragma Inline (Set_Nkind);

      procedure Set_Field1 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field1);

      procedure Set_Field2 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field2);

      procedure Set_Field3 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field3);

      procedure Set_Field4 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field4);

      procedure Set_Field5 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field5);

      procedure Set_Field6 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field6);

      procedure Set_Field7 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field7);

      procedure Set_Field8 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field8);

      procedure Set_Field9 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field9);

      procedure Set_Field10 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field10);

      procedure Set_Field11 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field11);

      procedure Set_Field12 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field12);

      procedure Set_Field13 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field13);

      procedure Set_Field14 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field14);

      procedure Set_Field15 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field15);

      procedure Set_Field16 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field16);

      procedure Set_Field17 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field17);

      procedure Set_Field18 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field18);

      procedure Set_Field19 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field19);

      procedure Set_Field20 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field20);

      procedure Set_Field21 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field21);

      procedure Set_Field22 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field22);

      procedure Set_Field23 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field23);

      procedure Set_Field24 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field24);

      procedure Set_Field25 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field25);

      procedure Set_Field26 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field26);

      procedure Set_Field27 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field27);

      procedure Set_Field28 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field28);

      procedure Set_Field29 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field29);

      procedure Set_Field30 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field30);

      procedure Set_Field31 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field31);

      procedure Set_Field32 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field32);

      procedure Set_Field33 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field33);

      procedure Set_Field34 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field34);

      procedure Set_Field35 (N : Node_Id; Val : Union_Id);
      pragma Inline (Set_Field35);

      procedure Set_Node1 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node1);

      procedure Set_Node2 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node2);

      procedure Set_Node3 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node3);

      procedure Set_Node4 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node4);

      procedure Set_Node5 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node5);

      procedure Set_Node6 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node6);

      procedure Set_Node7 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node7);

      procedure Set_Node8 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node8);

      procedure Set_Node9 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node9);

      procedure Set_Node10 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node10);

      procedure Set_Node11 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node11);

      procedure Set_Node12 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node12);

      procedure Set_Node13 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node13);

      procedure Set_Node14 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node14);

      procedure Set_Node15 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node15);

      procedure Set_Node16 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node16);

      procedure Set_Node17 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node17);

      procedure Set_Node18 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node18);

      procedure Set_Node19 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node19);

      procedure Set_Node20 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node20);

      procedure Set_Node21 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node21);

      procedure Set_Node22 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node22);

      procedure Set_Node23 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node23);

      procedure Set_Node24 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node24);

      procedure Set_Node25 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node25);

      procedure Set_Node26 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node26);

      procedure Set_Node27 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node27);

      procedure Set_Node28 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node28);

      procedure Set_Node29 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node29);

      procedure Set_Node30 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node30);

      procedure Set_Node31 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node31);

      procedure Set_Node32 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node32);

      procedure Set_Node33 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node33);

      procedure Set_Node34 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node34);

      procedure Set_Node35 (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node35);

      procedure Set_List1 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List1);

      procedure Set_List2 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List2);

      procedure Set_List3 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List3);

      procedure Set_List4 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List4);

      procedure Set_List5 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List5);

      procedure Set_List10 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List10);

      procedure Set_List14 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List14);

      procedure Set_List25 (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List25);

      procedure Set_Elist1 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist1);

      procedure Set_Elist2 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist2);

      procedure Set_Elist3 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist3);

      procedure Set_Elist4 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist4);

      procedure Set_Elist5 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist5);

      procedure Set_Elist8 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist8);

      procedure Set_Elist9 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist9);

      procedure Set_Elist10 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist10);

      procedure Set_Elist13 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist13);

      procedure Set_Elist15 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist15);

      procedure Set_Elist16 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist16);

      procedure Set_Elist18 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist18);

      procedure Set_Elist21 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist21);

      procedure Set_Elist23 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist23);

      procedure Set_Elist24 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist24);

      procedure Set_Elist25 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist25);

      procedure Set_Elist26 (N : Node_Id; Val : Elist_Id);
      pragma Inline (Set_Elist26);

      procedure Set_Name1 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name1);

      procedure Set_Name2 (N : Node_Id; Val : Name_Id);
      pragma Inline (Set_Name2);

      procedure Set_Str3 (N : Node_Id; Val : String_Id);
      pragma Inline (Set_Str3);

      procedure Set_Uint2 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint2);

      procedure Set_Uint3 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint3);

      procedure Set_Uint4 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint4);

      procedure Set_Uint5 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint5);

      procedure Set_Uint8 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint8);

      procedure Set_Uint9 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint9);

      procedure Set_Uint10 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint10);

      procedure Set_Uint11 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint11);

      procedure Set_Uint12 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint12);

      procedure Set_Uint13 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint13);

      procedure Set_Uint14 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint14);

      procedure Set_Uint15 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint15);

      procedure Set_Uint16 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint16);

      procedure Set_Uint17 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint17);

      procedure Set_Uint22 (N : Node_Id; Val : Uint);
      pragma Inline (Set_Uint22);

      procedure Set_Ureal3 (N : Node_Id; Val : Ureal);
      pragma Inline (Set_Ureal3);

      procedure Set_Ureal18 (N : Node_Id; Val : Ureal);
      pragma Inline (Set_Ureal18);

      procedure Set_Ureal21 (N : Node_Id; Val : Ureal);
      pragma Inline (Set_Ureal21);

      procedure Set_Flag0 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag0);

      procedure Set_Flag1 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag1);

      procedure Set_Flag2 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag2);

      procedure Set_Flag3 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag3);

      procedure Set_Flag4 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag4);

      procedure Set_Flag5 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag5);

      procedure Set_Flag6 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag6);

      procedure Set_Flag7 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag7);

      procedure Set_Flag8 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag8);

      procedure Set_Flag9 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag9);

      procedure Set_Flag10 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag10);

      procedure Set_Flag11 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag11);

      procedure Set_Flag12 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag12);

      procedure Set_Flag13 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag13);

      procedure Set_Flag14 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag14);

      procedure Set_Flag15 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag15);

      procedure Set_Flag16 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag16);

      procedure Set_Flag17 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag17);

      procedure Set_Flag18 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag18);

      procedure Set_Flag19 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag19);

      procedure Set_Flag20 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag20);

      procedure Set_Flag21 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag21);

      procedure Set_Flag22 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag22);

      procedure Set_Flag23 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag23);

      procedure Set_Flag24 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag24);

      procedure Set_Flag25 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag25);

      procedure Set_Flag26 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag26);

      procedure Set_Flag27 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag27);

      procedure Set_Flag28 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag28);

      procedure Set_Flag29 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag29);

      procedure Set_Flag30 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag30);

      procedure Set_Flag31 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag31);

      procedure Set_Flag32 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag32);

      procedure Set_Flag33 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag33);

      procedure Set_Flag34 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag34);

      procedure Set_Flag35 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag35);

      procedure Set_Flag36 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag36);

      procedure Set_Flag37 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag37);

      procedure Set_Flag38 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag38);

      procedure Set_Flag39 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag39);

      procedure Set_Flag40 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag40);

      procedure Set_Flag41 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag41);

      procedure Set_Flag42 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag42);

      procedure Set_Flag43 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag43);

      procedure Set_Flag44 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag44);

      procedure Set_Flag45 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag45);

      procedure Set_Flag46 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag46);

      procedure Set_Flag47 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag47);

      procedure Set_Flag48 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag48);

      procedure Set_Flag49 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag49);

      procedure Set_Flag50 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag50);

      procedure Set_Flag51 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag51);

      procedure Set_Flag52 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag52);

      procedure Set_Flag53 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag53);

      procedure Set_Flag54 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag54);

      procedure Set_Flag55 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag55);

      procedure Set_Flag56 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag56);

      procedure Set_Flag57 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag57);

      procedure Set_Flag58 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag58);

      procedure Set_Flag59 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag59);

      procedure Set_Flag60 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag60);

      procedure Set_Flag61 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag61);

      procedure Set_Flag62 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag62);

      procedure Set_Flag63 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag63);

      procedure Set_Flag64 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag64);

      procedure Set_Flag65 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag65);

      procedure Set_Flag66 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag66);

      procedure Set_Flag67 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag67);

      procedure Set_Flag68 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag68);

      procedure Set_Flag69 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag69);

      procedure Set_Flag70 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag70);

      procedure Set_Flag71 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag71);

      procedure Set_Flag72 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag72);

      procedure Set_Flag73 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag73);

      procedure Set_Flag74 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag74);

      procedure Set_Flag75 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag75);

      procedure Set_Flag76 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag76);

      procedure Set_Flag77 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag77);

      procedure Set_Flag78 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag78);

      procedure Set_Flag79 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag79);

      procedure Set_Flag80 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag80);

      procedure Set_Flag81 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag81);

      procedure Set_Flag82 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag82);

      procedure Set_Flag83 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag83);

      procedure Set_Flag84 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag84);

      procedure Set_Flag85 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag85);

      procedure Set_Flag86 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag86);

      procedure Set_Flag87 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag87);

      procedure Set_Flag88 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag88);

      procedure Set_Flag89 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag89);

      procedure Set_Flag90 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag90);

      procedure Set_Flag91 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag91);

      procedure Set_Flag92 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag92);

      procedure Set_Flag93 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag93);

      procedure Set_Flag94 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag94);

      procedure Set_Flag95 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag95);

      procedure Set_Flag96 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag96);

      procedure Set_Flag97 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag97);

      procedure Set_Flag98 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag98);

      procedure Set_Flag99 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag99);

      procedure Set_Flag100 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag100);

      procedure Set_Flag101 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag101);

      procedure Set_Flag102 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag102);

      procedure Set_Flag103 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag103);

      procedure Set_Flag104 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag104);

      procedure Set_Flag105 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag105);

      procedure Set_Flag106 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag106);

      procedure Set_Flag107 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag107);

      procedure Set_Flag108 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag108);

      procedure Set_Flag109 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag109);

      procedure Set_Flag110 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag110);

      procedure Set_Flag111 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag111);

      procedure Set_Flag112 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag112);

      procedure Set_Flag113 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag113);

      procedure Set_Flag114 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag114);

      procedure Set_Flag115 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag115);

      procedure Set_Flag116 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag116);

      procedure Set_Flag117 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag117);

      procedure Set_Flag118 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag118);

      procedure Set_Flag119 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag119);

      procedure Set_Flag120 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag120);

      procedure Set_Flag121 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag121);

      procedure Set_Flag122 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag122);

      procedure Set_Flag123 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag123);

      procedure Set_Flag124 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag124);

      procedure Set_Flag125 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag125);

      procedure Set_Flag126 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag126);

      procedure Set_Flag127 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag127);

      procedure Set_Flag128 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag128);

      procedure Set_Flag129 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag129);

      procedure Set_Flag130 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag130);

      procedure Set_Flag131 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag131);

      procedure Set_Flag132 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag132);

      procedure Set_Flag133 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag133);

      procedure Set_Flag134 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag134);

      procedure Set_Flag135 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag135);

      procedure Set_Flag136 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag136);

      procedure Set_Flag137 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag137);

      procedure Set_Flag138 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag138);

      procedure Set_Flag139 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag139);

      procedure Set_Flag140 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag140);

      procedure Set_Flag141 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag141);

      procedure Set_Flag142 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag142);

      procedure Set_Flag143 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag143);

      procedure Set_Flag144 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag144);

      procedure Set_Flag145 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag145);

      procedure Set_Flag146 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag146);

      procedure Set_Flag147 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag147);

      procedure Set_Flag148 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag148);

      procedure Set_Flag149 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag149);

      procedure Set_Flag150 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag150);

      procedure Set_Flag151 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag151);

      procedure Set_Flag152 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag152);

      procedure Set_Flag153 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag153);

      procedure Set_Flag154 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag154);

      procedure Set_Flag155 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag155);

      procedure Set_Flag156 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag156);

      procedure Set_Flag157 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag157);

      procedure Set_Flag158 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag158);

      procedure Set_Flag159 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag159);

      procedure Set_Flag160 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag160);

      procedure Set_Flag161 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag161);

      procedure Set_Flag162 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag162);

      procedure Set_Flag163 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag163);

      procedure Set_Flag164 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag164);

      procedure Set_Flag165 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag165);

      procedure Set_Flag166 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag166);

      procedure Set_Flag167 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag167);

      procedure Set_Flag168 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag168);

      procedure Set_Flag169 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag169);

      procedure Set_Flag170 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag170);

      procedure Set_Flag171 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag171);

      procedure Set_Flag172 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag172);

      procedure Set_Flag173 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag173);

      procedure Set_Flag174 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag174);

      procedure Set_Flag175 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag175);

      procedure Set_Flag176 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag176);

      procedure Set_Flag177 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag177);

      procedure Set_Flag178 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag178);

      procedure Set_Flag179 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag179);

      procedure Set_Flag180 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag180);

      procedure Set_Flag181 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag181);

      procedure Set_Flag182 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag182);

      procedure Set_Flag183 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag183);

      procedure Set_Flag184 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag184);

      procedure Set_Flag185 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag185);

      procedure Set_Flag186 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag186);

      procedure Set_Flag187 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag187);

      procedure Set_Flag188 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag188);

      procedure Set_Flag189 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag189);

      procedure Set_Flag190 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag190);

      procedure Set_Flag191 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag191);

      procedure Set_Flag192 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag192);

      procedure Set_Flag193 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag193);

      procedure Set_Flag194 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag194);

      procedure Set_Flag195 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag195);

      procedure Set_Flag196 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag196);

      procedure Set_Flag197 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag197);

      procedure Set_Flag198 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag198);

      procedure Set_Flag199 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag199);

      procedure Set_Flag200 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag200);

      procedure Set_Flag201 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag201);

      procedure Set_Flag202 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag202);

      procedure Set_Flag203 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag203);

      procedure Set_Flag204 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag204);

      procedure Set_Flag205 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag205);

      procedure Set_Flag206 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag206);

      procedure Set_Flag207 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag207);

      procedure Set_Flag208 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag208);

      procedure Set_Flag209 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag209);

      procedure Set_Flag210 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag210);

      procedure Set_Flag211 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag211);

      procedure Set_Flag212 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag212);

      procedure Set_Flag213 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag213);

      procedure Set_Flag214 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag214);

      procedure Set_Flag215 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag215);

      procedure Set_Flag216 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag216);

      procedure Set_Flag217 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag217);

      procedure Set_Flag218 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag218);

      procedure Set_Flag219 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag219);

      procedure Set_Flag220 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag220);

      procedure Set_Flag221 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag221);

      procedure Set_Flag222 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag222);

      procedure Set_Flag223 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag223);

      procedure Set_Flag224 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag224);

      procedure Set_Flag225 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag225);

      procedure Set_Flag226 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag226);

      procedure Set_Flag227 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag227);

      procedure Set_Flag228 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag228);

      procedure Set_Flag229 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag229);

      procedure Set_Flag230 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag230);

      procedure Set_Flag231 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag231);

      procedure Set_Flag232 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag232);

      procedure Set_Flag233 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag233);

      procedure Set_Flag234 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag234);

      procedure Set_Flag235 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag235);

      procedure Set_Flag236 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag236);

      procedure Set_Flag237 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag237);

      procedure Set_Flag238 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag238);

      procedure Set_Flag239 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag239);

      procedure Set_Flag240 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag240);

      procedure Set_Flag241 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag241);

      procedure Set_Flag242 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag242);

      procedure Set_Flag243 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag243);

      procedure Set_Flag244 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag244);

      procedure Set_Flag245 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag245);

      procedure Set_Flag246 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag246);

      procedure Set_Flag247 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag247);

      procedure Set_Flag248 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag248);

      procedure Set_Flag249 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag249);

      procedure Set_Flag250 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag250);

      procedure Set_Flag251 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag251);

      procedure Set_Flag252 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag252);

      procedure Set_Flag253 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag253);

      procedure Set_Flag254 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag254);

      procedure Set_Flag255 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag255);

      procedure Set_Flag256 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag256);

      procedure Set_Flag257 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag257);

      procedure Set_Flag258 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag258);

      procedure Set_Flag259 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag259);

      procedure Set_Flag260 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag260);

      procedure Set_Flag261 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag261);

      procedure Set_Flag262 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag262);

      procedure Set_Flag263 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag263);

      procedure Set_Flag264 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag264);

      procedure Set_Flag265 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag265);

      procedure Set_Flag266 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag266);

      procedure Set_Flag267 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag267);

      procedure Set_Flag268 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag268);

      procedure Set_Flag269 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag269);

      procedure Set_Flag270 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag270);

      procedure Set_Flag271 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag271);

      procedure Set_Flag272 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag272);

      procedure Set_Flag273 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag273);

      procedure Set_Flag274 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag274);

      procedure Set_Flag275 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag275);

      procedure Set_Flag276 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag276);

      procedure Set_Flag277 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag277);

      procedure Set_Flag278 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag278);

      procedure Set_Flag279 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag279);

      procedure Set_Flag280 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag280);

      procedure Set_Flag281 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag281);

      procedure Set_Flag282 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag282);

      procedure Set_Flag283 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag283);

      procedure Set_Flag284 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag284);

      procedure Set_Flag285 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag285);

      procedure Set_Flag286 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag286);

      procedure Set_Flag287 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag287);

      procedure Set_Flag288 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag288);

      procedure Set_Flag289 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag289);

      procedure Set_Flag290 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag290);

      procedure Set_Flag291 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag291);

      procedure Set_Flag292 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag292);

      procedure Set_Flag293 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag293);

      procedure Set_Flag294 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag294);

      procedure Set_Flag295 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag295);

      procedure Set_Flag296 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag296);

      procedure Set_Flag297 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag297);

      procedure Set_Flag298 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag298);

      procedure Set_Flag299 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag299);

      procedure Set_Flag300 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag300);

      procedure Set_Flag301 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag301);

      procedure Set_Flag302 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag302);

      procedure Set_Flag303 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag303);

      procedure Set_Flag304 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag304);

      procedure Set_Flag305 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag305);

      procedure Set_Flag306 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag306);

      procedure Set_Flag307 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag307);

      procedure Set_Flag308 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag308);

      procedure Set_Flag309 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag309);

      procedure Set_Flag310 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag310);

      procedure Set_Flag311 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag311);

      procedure Set_Flag312 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag312);

      procedure Set_Flag313 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag313);

      procedure Set_Flag314 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag314);

      procedure Set_Flag315 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag315);

      procedure Set_Flag316 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag316);

      procedure Set_Flag317 (N : Node_Id; Val : Boolean);
      pragma Inline (Set_Flag317);

      --  The following versions of Set_Noden also set the parent pointer of
      --  the referenced node if it is not Empty.

      procedure Set_Node1_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node1_With_Parent);

      procedure Set_Node2_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node2_With_Parent);

      procedure Set_Node3_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node3_With_Parent);

      procedure Set_Node4_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node4_With_Parent);

      procedure Set_Node5_With_Parent (N : Node_Id; Val : Node_Id);
      pragma Inline (Set_Node5_With_Parent);

      --  The following versions of Set_Listn also set the parent pointer of
      --  the referenced node if it is not Empty.

      procedure Set_List1_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List1_With_Parent);

      procedure Set_List2_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List2_With_Parent);

      procedure Set_List3_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List3_With_Parent);

      procedure Set_List4_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List4_With_Parent);

      procedure Set_List5_With_Parent (N : Node_Id; Val : List_Id);
      pragma Inline (Set_List5_With_Parent);

   end Unchecked_Access;

   -----------------------------
   -- Private Part Subpackage --
   -----------------------------

   --  The following package contains the definition of the data structure
   --  used by the implementation of the Atree package. Logically it really
   --  corresponds to the private part, hence the name. The reason that it
   --  is defined as a sub-package is to allow special access from clients
   --  that need to see the internals of the data structures.

   package Atree_Private_Part is

      -------------------------
      -- Tree Representation --
      -------------------------

      --  The nodes of the tree are stored in a table (i.e. an array). In the
      --  case of extended nodes six consecutive components in the array are
      --  used. There are thus two formats for array components. One is used
      --  for non-extended nodes, and for the first component of extended
      --  nodes. The other is used for the extension parts (second, third,
      --  fourth, fifth, and sixth components) of an extended node. A variant
      --  record structure is used to distinguish the two formats.

      type Node_Record (Is_Extension : Boolean := False) is record

         --  Logically, the only field in the common part is the above
         --  Is_Extension discriminant (a single bit). However, Gigi cannot
         --  yet handle such a structure, so we fill out the common part of
         --  the record with fields that are used in different ways for
         --  normal nodes and node extensions.

         Pflag1, Pflag2 : Boolean;
         --  The Paren_Count field is represented using two boolean flags,
         --  where Pflag1 is worth 1, and Pflag2 is worth 2. This is done
         --  because we need to be easily able to reuse this field for
         --  extra flags in the extended node case.

         In_List : Boolean;
         --  Flag used to indicate if node is a member of a list.
         --  This field is considered private to the Atree package.

         Has_Aspects : Boolean;
         --  Flag used to indicate that a node has aspect specifications that
         --  are associated with the node. See Aspects package for details.

         Rewrite_Ins : Boolean;
         --  Flag set by Mark_Rewrite_Insertion procedure.
         --  This field is considered private to the Atree package.

         Analyzed : Boolean;
         --  Flag to indicate the node has been analyzed (and expanded)

         Comes_From_Source : Boolean;
         --  Flag to indicate that node comes from the source program (i.e.
         --  was built by the parser or scanner, not the analyzer or expander).

         Error_Posted : Boolean;
         --  Flag to indicate that an error message has been posted on the
         --  node (to avoid duplicate flags on the same node)

         Flag4  : Boolean;
         Flag5  : Boolean;
         Flag6  : Boolean;
         Flag7  : Boolean;
         Flag8  : Boolean;
         Flag9  : Boolean;
         Flag10 : Boolean;
         Flag11 : Boolean;
         Flag12 : Boolean;
         Flag13 : Boolean;
         Flag14 : Boolean;
         Flag15 : Boolean;
         Flag16 : Boolean;
         Flag17 : Boolean;
         Flag18 : Boolean;
         --  Flags 4-18 for a normal node. Note that Flags 0-3 are stored
         --  separately in the Flags array.

         --  The above fields are used as follows in components 2-6 of
         --  an extended node entry.

         --    In_List           used as Flag19,Flag40,Flag129,Flag216,Flag287
         --    Has_Aspects       used as Flag20,Flag41,Flag130,Flag217,Flag288
         --    Rewrite_Ins       used as Flag21,Flag42,Flag131,Flag218,Flag289
         --    Analyzed          used as Flag22,Flag43,Flag132,Flag219,Flag290
         --    Comes_From_Source used as Flag23,Flag44,Flag133,Flag220,Flag291
         --    Error_Posted      used as Flag24,Flag45,Flag134,Flag221,Flag292
         --    Flag4             used as Flag25,Flag46,Flag135,Flag222,Flag293
         --    Flag5             used as Flag26,Flag47,Flag136,Flag223,Flag294
         --    Flag6             used as Flag27,Flag48,Flag137,Flag224,Flag295
         --    Flag7             used as Flag28,Flag49,Flag138,Flag225,Flag296
         --    Flag8             used as Flag29,Flag50,Flag139,Flag226,Flag297
         --    Flag9             used as Flag30,Flag51,Flag140,Flag227,Flag298
         --    Flag10            used as Flag31,Flag52,Flag141,Flag228,Flag299
         --    Flag11            used as Flag32,Flag53,Flag142,Flag229,Flag300
         --    Flag12            used as Flag33,Flag54,Flag143,Flag230,Flag301
         --    Flag13            used as Flag34,Flag55,Flag144,Flag231,Flag302
         --    Flag14            used as Flag35,Flag56,Flag145,Flag232,Flag303
         --    Flag15            used as Flag36,Flag57,Flag146,Flag233,Flag304
         --    Flag16            used as Flag37,Flag58,Flag147,Flag234,Flag305
         --    Flag17            used as Flag38,Flag59,Flag148,Flag235,Flag306
         --    Flag18            used as Flag39,Flag60,Flag149,Flag236,Flag307
         --    Pflag1            used as Flag61,Flag62,Flag150,Flag237,Flag308
         --    Pflag2            used as Flag63,Flag64,Flag151,Flag238,Flag309

         Nkind : Node_Kind;
         --  For a non-extended node, or the initial section of an extended
         --  node, this field holds the Node_Kind value. For an extended node,
         --  The Nkind field is used as follows:
         --
         --     Second entry: holds the Ekind field of the entity
         --     Third entry:  holds 8 additional flags (Flag65-Flag72)
         --     Fourth entry: holds 8 additional flags (Flag239-246)
         --     Fifth entry:  holds 8 additional flags (Flag247-254)
         --     Sixth entry:  holds 8 additional flags (Flag310-317)

         --  Now finally (on an 32-bit boundary) comes the variant part

         case Is_Extension is

            --  Non-extended node, or first component of extended node

            when False =>

               Sloc : Source_Ptr;
               --  Source location for this node

               Link : Union_Id;
               --  This field is used either as the Parent pointer (if In_List
               --  is False), or to point to the list header (if In_List is
               --  True). This field is considered private and can be modified
               --  only by Atree or by Nlists.

               Field1 : Union_Id;
               Field2 : Union_Id;
               Field3 : Union_Id;
               Field4 : Union_Id;
               Field5 : Union_Id;
               --  Five general use fields, which can contain Node_Id, List_Id,
               --  Elist_Id, String_Id, or Name_Id values depending on the
               --  values in Nkind and (for extended nodes), in Ekind. See
               --  packages Sinfo and Einfo for details of their use.

            --  Extension (second component) of extended node

            when True =>

               Field6  : Union_Id;
               Field7  : Union_Id;
               Field8  : Union_Id;
               Field9  : Union_Id;
               Field10 : Union_Id;
               Field11 : Union_Id;
               Field12 : Union_Id;
               --  Seven additional general fields available only for entities
               --  See package Einfo for details of their use (which depends
               --  on the value in the Ekind field).

            --  In the third component, the extension format as described
            --  above is used to hold additional general fields and flags
            --  as follows:

            --    Field6-11      Holds Field13-Field18
            --    Field12        Holds Flag73-Flag96 and Convention

            --  In the fourth component, the extension format as described
            --  above is used to hold additional general fields and flags
            --  as follows:

            --    Field6-10      Holds Field19-Field23
            --    Field11        Holds Flag152-Flag183
            --    Field12        Holds Flag97-Flag128

            --  In the fifth component, the extension format as described
            --  above is used to hold additional general fields and flags
            --  as follows:

            --    Field6-11      Holds Field24-Field29
            --    Field12        Holds Flag184-Flag215

            --  In the sixth component, the extension format as described
            --  above is used to hold additional general fields and flags
            --  as follows:

            --    Field6-11      Holds Field30-Field35
            --    Field12        Holds Flag255-Flag286

         end case;
      end record;

      pragma Pack (Node_Record);
      for Node_Record'Size use 8*32;
      for Node_Record'Alignment use 4;

      function E_To_N is new Unchecked_Conversion (Entity_Kind, Node_Kind);
      function N_To_E is new Unchecked_Conversion (Node_Kind, Entity_Kind);

      --  Default value used to initialize default nodes. Note that some of the
      --  fields get overwritten, and in particular, Nkind always gets reset.

      Default_Node : Node_Record := (
         Is_Extension      => False,
         Pflag1            => False,
         Pflag2            => False,
         In_List           => False,
         Has_Aspects       => False,
         Rewrite_Ins       => False,
         Analyzed          => False,
         Comes_From_Source => False,
         --  modified by Set_Comes_From_Source_Default
         Error_Posted      => False,
         Flag4             => False,

         Flag5             => False,
         Flag6             => False,
         Flag7             => False,
         Flag8             => False,
         Flag9             => False,
         Flag10            => False,
         Flag11            => False,
         Flag12            => False,

         Flag13            => False,
         Flag14            => False,
         Flag15            => False,
         Flag16            => False,
         Flag17            => False,
         Flag18            => False,

         Nkind             => N_Unused_At_Start,

         Sloc              => No_Location,
         Link              => Empty_List_Or_Node,
         Field1            => Empty_List_Or_Node,
         Field2            => Empty_List_Or_Node,
         Field3            => Empty_List_Or_Node,
         Field4            => Empty_List_Or_Node,
         Field5            => Empty_List_Or_Node);

      --  Default value used to initialize node extensions (i.e. the second
      --  through sixth components of an extended node). Note we are cheating
      --  a bit here when it comes to Node12, which really holds flags and (for
      --  the third component), the convention. But it works because Empty,
      --  False, Convention_Ada, all happen to be all zero bits.

      Default_Node_Extension : constant Node_Record := (
         Is_Extension      => True,
         Pflag1            => False,
         Pflag2            => False,
         In_List           => False,
         Has_Aspects       => False,
         Rewrite_Ins       => False,
         Analyzed          => False,
         Comes_From_Source => False,
         Error_Posted      => False,
         Flag4             => False,

         Flag5             => False,
         Flag6             => False,
         Flag7             => False,
         Flag8             => False,
         Flag9             => False,
         Flag10            => False,
         Flag11            => False,
         Flag12            => False,

         Flag13            => False,
         Flag14            => False,
         Flag15            => False,
         Flag16            => False,
         Flag17            => False,
         Flag18            => False,

         Nkind             => E_To_N (E_Void),

         Field6            => Empty_List_Or_Node,
         Field7            => Empty_List_Or_Node,
         Field8            => Empty_List_Or_Node,
         Field9            => Empty_List_Or_Node,
         Field10           => Empty_List_Or_Node,
         Field11           => Empty_List_Or_Node,
         Field12           => Empty_List_Or_Node);

      --  The following defines the extendable array used for the nodes table
      --  Nodes with extensions use six consecutive entries in the array

      package Nodes is new Table.Table (
        Table_Component_Type => Node_Record,
        Table_Index_Type     => Node_Id'Base,
        Table_Low_Bound      => First_Node_Id,
        Table_Initial        => Alloc.Nodes_Initial,
        Table_Increment      => Alloc.Nodes_Increment,
        Table_Name           => "Nodes");

      --  The following is a parallel table to Nodes, which provides 8 more
      --  bits of space that logically belong to the corresponding node. This
      --  is currently used to implement Flags 0,1,2,3 for normal nodes, or
      --  the first component of an extended node (four bits unused). Entries
      --  for extending components are completely unused.

      type Flags_Byte is record
         Flag0  : Boolean;
         Flag1  : Boolean;
         Flag2  : Boolean;
         Flag3  : Boolean;
         Spare0 : Boolean;
         Spare1 : Boolean;
         Spare2 : Boolean;
         Spare3 : Boolean;
      end record;

      for Flags_Byte'Size use 8;
      pragma Pack (Flags_Byte);

      Default_Flags : constant Flags_Byte := (others => False);
      --  Default value used to initialize new entries

      package Flags is new Table.Table (
        Table_Component_Type => Flags_Byte,
        Table_Index_Type     => Node_Id'Base,
        Table_Low_Bound      => First_Node_Id,
        Table_Initial        => Alloc.Nodes_Initial,
        Table_Increment      => Alloc.Nodes_Increment,
        Table_Name           => "Flags");

   end Atree_Private_Part;

end Atree;
