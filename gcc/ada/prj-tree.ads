------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . T R E E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
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

--  This package defines the structure of the Project File tree

with GNAT.Dynamic_HTables;
with GNAT.Dynamic_Tables;

with Table;

with Prj.Attr; use Prj.Attr;

package Prj.Tree is

   type Project_Node_Tree_Data;
   type Project_Node_Tree_Ref is access all Project_Node_Tree_Data;
   --  Type to designate a project node tree, so that several project node
   --  trees can coexist in memory.

   Project_Nodes_Initial   : constant := 1_000;
   Project_Nodes_Increment : constant := 100;
   --  Allocation parameters for initializing and extending number
   --  of nodes in table Tree_Private_Part.Project_Nodes

   Project_Node_Low_Bound  : constant := 0;
   Project_Node_High_Bound : constant := 099_999_999;
   --  Range of values for project node id's (in practice infinite)

   type Project_Node_Id is range
     Project_Node_Low_Bound .. Project_Node_High_Bound;
   --  The index of table Tree_Private_Part.Project_Nodes

   Empty_Node : constant Project_Node_Id := Project_Node_Low_Bound;
   --  Designates no node in table Project_Nodes

   First_Node_Id : constant Project_Node_Id := Project_Node_Low_Bound + 1;

   subtype Variable_Node_Id is Project_Node_Id;
   --  Used to designate a node whose expected kind is one of
   --  N_Typed_Variable_Declaration, N_Variable_Declaration or
   --  N_Variable_Reference.

   subtype Package_Declaration_Id is Project_Node_Id;
   --  Used to designate a node whose expected kind is N_Project_Declaration

   type Project_Node_Kind is
     (N_Project,
      N_With_Clause,
      N_Project_Declaration,
      N_Declarative_Item,
      N_Package_Declaration,
      N_String_Type_Declaration,
      N_Literal_String,
      N_Attribute_Declaration,
      N_Typed_Variable_Declaration,
      N_Variable_Declaration,
      N_Expression,
      N_Term,
      N_Literal_String_List,
      N_Variable_Reference,
      N_External_Value,
      N_Attribute_Reference,
      N_Case_Construction,
      N_Case_Item,
      N_Comment_Zones,
      N_Comment);
   --  Each node in the tree is of a Project_Node_Kind. For the signification
   --  of the fields in each node of Project_Node_Kind, look at package
   --  Tree_Private_Part.

   function Present (Node : Project_Node_Id) return Boolean;
   pragma Inline (Present);
   --  Return True if Node /= Empty_Node

   function No (Node : Project_Node_Id) return Boolean;
   pragma Inline (No);
   --  Return True if Node = Empty_Node

   procedure Initialize (Tree : Project_Node_Tree_Ref);
   --  Initialize the Project File tree: empty the Project_Nodes table
   --  and reset the Projects_Htable.

   function Default_Project_Node
     (In_Tree       : Project_Node_Tree_Ref;
      Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined) return Project_Node_Id;
   --  Returns a Project_Node_Record with the specified Kind and Expr_Kind. All
   --  the other components have default nil values.
   --  To create a node for a project itself, see Create_Project below instead

   function Hash (N : Project_Node_Id) return Header_Num;
   --  Used for hash tables where the key is a Project_Node_Id

   function Imported_Or_Extended_Project_Of
     (Project   : Project_Node_Id;
      In_Tree   : Project_Node_Tree_Ref;
      With_Name : Name_Id) return Project_Node_Id;
   --  Return the node of a project imported or extended by project Project and
   --  whose name is With_Name. Return Empty_Node if there is no such project.

   --------------
   -- Comments --
   --------------

   type Comment_State is private;
   --  A type to store the values of several global variables related to
   --  comments.

   procedure Save (S : out Comment_State);
   --  Save in variable S the comment state. Called before scanning a new
   --  project file.

   procedure Restore_And_Free (S : in out Comment_State);
   --  Restore the comment state to a previously saved value. Called after
   --  scanning a project file. Frees the memory occupied by S

   procedure Reset_State;
   --  Set the comment state to its initial value. Called before scanning a
   --  new project file.

   function There_Are_Unkept_Comments return Boolean;
   --  Indicates that some of the comments in a project file could not be
   --  stored in the parse tree.

   procedure Set_Previous_Line_Node (To : Project_Node_Id);
   --  Indicate the node on the previous line. If there are comments
   --  immediately following this line, then they should be associated with
   --  this node.

   procedure Set_Previous_End_Node (To : Project_Node_Id);
   --  Indicate that on the previous line the "end" belongs to node To.
   --  If there are comments immediately following this "end" line, they
   --  should be associated with this node.

   procedure Set_End_Of_Line (To : Project_Node_Id);
   --  Indicate the node on the current line. If there is an end of line
   --  comment, then it should be associated with this node.

   procedure Set_Next_End_Node (To : Project_Node_Id);
   --  Put node To on the top of the end node stack. When an END line is found
   --  with this node on the top of the end node stack, the comments, if any,
   --  immediately preceding this "end" line will be associated with this node.

   procedure Remove_Next_End_Node;
   --  Remove the top of the end node stack

   ------------------------
   -- Comment Processing --
   ------------------------

   type Comment_Data is record
      Value                     : Name_Id := No_Name;
      Follows_Empty_Line        : Boolean := False;
      Is_Followed_By_Empty_Line : Boolean := False;
   end record;
   --  Component type for Comments Table below

   package Comments is new Table.Table
     (Table_Component_Type => Comment_Data,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Prj.Tree.Comments");
   --  A table to store the comments that may be stored is the tree

   procedure Scan (In_Tree : Project_Node_Tree_Ref);
   --  Scan the tokens and accumulate comments

   type Comment_Location is
     (Before, After, Before_End, After_End, End_Of_Line);
   --  Used in call to Add_Comments below

   procedure Add_Comments
     (To      : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      Where   : Comment_Location);
   --  Add comments to this node

   ----------------------
   -- Access Functions --
   ----------------------

   --  The following query functions are part of the abstract interface
   --  of the Project File tree. They provide access to fields of a project.

   --  The access functions should be called only with valid arguments.
   --  For each function the condition of validity is specified. If an access
   --  function is called with invalid arguments, then exception
   --  Assertion_Error is raised if assertions are enabled, otherwise the
   --  behaviour is not defined and may result in a crash.

   function Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (Name_Of);
   --  Valid for all non empty nodes. May return No_Name for nodes that have
   --  no names.

   function Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Kind;
   pragma Inline (Kind_Of);
   --  Valid for all non empty nodes

   function Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Source_Ptr;
   pragma Inline (Location_Of);
   --  Valid for all non empty nodes

   function First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment nodes

   function End_Of_Line_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   --  Valid only for non empty nodes

   function Follows_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Comment nodes

   function Is_Followed_By_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Comment nodes

   function Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Parent_Project_Of);
   --  Valid only for N_Project nodes

   function Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Project nodes

   function Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Directory_Of);
   --  Returns the directory that contains the project file. This always ends
   --  with a directory separator. Only valid for N_Project nodes.

   function Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Kind;
   pragma Inline (Expression_Kind_Of);
   --  Only valid for N_Literal_String, N_Attribute_Declaration,
   --  N_Variable_Declaration, N_Typed_Variable_Declaration, N_Expression,
   --  N_Term, N_Variable_Reference or N_Attribute_Reference nodes.

   function Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   pragma Inline (Is_Extending_All);
   --  Only valid for N_Project and N_With_Clause

   function Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   pragma Inline (Is_Not_Last_In_List);
   --  Only valid for N_With_Clause

   function First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Node_Id;
   pragma Inline (First_Variable_Of);
   --  Only valid for N_Project or N_Package_Declaration nodes

   function First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Declaration_Id;
   pragma Inline (First_Package_Of);
   --  Only valid for N_Project nodes

   function Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Node_Id;
   pragma Inline (Package_Id_Of);
   --  Only valid for N_Package_Declaration nodes

   function Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Path_Name_Of);
   --  Only valid for N_Project and N_With_Clause nodes

   function String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (String_Value_Of);
   --  Only valid for N_With_Clause, N_Literal_String nodes or N_Comment.
   --  For a N_With_Clause created automatically for a virtual extending
   --  project, No_Name is returned.

   function Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Int;
   pragma Inline (Source_Index_Of);
   --  Only valid for N_Literal_String and N_Attribute_Declaration nodes

   function First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_With_Clause_Of);
   --  Only valid for N_Project nodes

   function Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Declaration_Of);
   --  Only valid for N_Project nodes

   function Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Qualifier;
   pragma Inline (Project_Qualifier_Of);
   --  Only valid for N_Project nodes

   function Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Extending_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   function First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_String_Type_Of);
   --  Only valid for N_Project nodes

   function Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Extended_Project_Path_Of);
   --  Only valid for N_With_Clause nodes

   function Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Node_Of);
   --  Only valid for N_With_Clause, N_Variable_Reference and
   --  N_Attribute_Reference nodes.

   function Non_Limited_Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Non_Limited_Project_Node_Of);
   --  Only valid for N_With_Clause nodes. Returns Empty_Node for limited
   --  imported project files, otherwise returns the same result as
   --  Project_Node_Of.

   function Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_With_Clause_Of);
   --  Only valid for N_With_Clause nodes

   function First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Declarative_Item_Of);
   --  Only valid for N_Project_Declaration, N_Case_Item and
   --  N_Package_Declaration.

   function Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Extended_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   function Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Current_Item_Node);
   --  Only valid for N_Declarative_Item nodes

   function Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Declarative_Item);
   --  Only valid for N_Declarative_Item node

   function Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Of_Renamed_Package_Of);
   --  Only valid for N_Package_Declaration nodes. May return Empty_Node.

   function Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Package_In_Project);
   --  Only valid for N_Package_Declaration nodes

   function First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Literal_String);
   --  Only valid for N_String_Type_Declaration nodes

   function Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_String_Type);
   --  Only valid for N_String_Type_Declaration nodes

   function Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Literal_String);
   --  Only valid for N_Literal_String nodes

   function Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Expression_Of);
   --  Only valid for N_Attribute_Declaration, N_Typed_Variable_Declaration
   --  or N_Variable_Declaration nodes

   function Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return  Project_Node_Id;
   pragma Inline (Associative_Project_Of);
   --  Only valid for N_Attribute_Declaration nodes

   function Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return  Project_Node_Id;
   pragma Inline (Associative_Package_Of);
   --  Only valid for N_Attribute_Declaration nodes

   function Value_Is_Valid
     (For_Typed_Variable : Project_Node_Id;
      In_Tree            : Project_Node_Tree_Ref;
      Value              : Name_Id) return Boolean;
   pragma Inline (Value_Is_Valid);
   --  Only valid for N_Typed_Variable_Declaration. Returns True if Value is
   --  in the list of allowed strings for For_Typed_Variable. False otherwise.

   function Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (Associative_Array_Index_Of);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference.
   --  Returns No_Name for non associative array attributes.

   function Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Variable);
   --  Only valid for N_Typed_Variable_Declaration or N_Variable_Declaration
   --  nodes.

   function First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Term);
   --  Only valid for N_Expression nodes

   function Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Expression_In_List);
   --  Only valid for N_Expression nodes

   function Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Current_Term);
   --  Only valid for N_Term nodes

   function Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Term);
   --  Only valid for N_Term nodes

   function First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Expression_In_List);
   --  Only valid for N_Literal_String_List nodes

   function Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Package_Node_Of);
   --  Only valid for N_Variable_Reference or N_Attribute_Reference nodes.
   --  May return Empty_Node.

   function String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (String_Type_Of);
   --  Only valid for N_Variable_Reference or N_Typed_Variable_Declaration
   --  nodes.

   function External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (External_Reference_Of);
   --  Only valid for N_External_Value nodes

   function External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (External_Default_Of);
   --  Only valid for N_External_Value nodes

   function Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Case_Variable_Reference_Of);
   --  Only valid for N_Case_Construction nodes

   function First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Case_Item_Of);
   --  Only valid for N_Case_Construction nodes

   function First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Choice_Of);
   --  Only valid for N_Case_Item nodes. Return the first choice in a
   --  N_Case_Item, or Empty_Node if this is when others.

   function Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Case_Item);
   --  Only valid for N_Case_Item nodes

   function Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference nodes

   -----------------------
   -- Create procedures --
   -----------------------
   --  The following procedures are used to edit a project file tree. They are
   --  slightly higher-level than the Set_* procedures below

   function Create_Project
     (In_Tree        : Project_Node_Tree_Ref;
      Name           : Name_Id;
      Full_Path      : Path_Name_Type;
      Is_Config_File : Boolean := False) return Project_Node_Id;
   --  Create a new node for a project and register it in the tree so that it
   --  can be retrieved later on.

   function Create_Package
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Pkg     : String) return Project_Node_Id;
   --  Create a new package in Project. If the package already exists, it is
   --  returned. The name of the package *must* be lower-cases, or none of its
   --  attributes will be recognized.

   function Create_Attribute
     (Tree       : Project_Node_Tree_Ref;
      Prj_Or_Pkg : Project_Node_Id;
      Name       : Name_Id;
      Index_Name : Name_Id       := No_Name;
      Kind       : Variable_Kind := List;
      At_Index   : Integer       := 0;
      Value      : Project_Node_Id := Empty_Node) return Project_Node_Id;
   --  Create a new attribute. The new declaration is added at the end of the
   --  declarative item list for Prj_Or_Pkg (a project or a package), but
   --  before any package declaration). No addition is done if Prj_Or_Pkg is
   --  Empty_Node. If Index_Name is not "", then if creates an attribute value
   --  for a specific index. At_Index is used for the " at <idx>" in the naming
   --  exceptions.
   --  To set the value of the attribute, either provide a value for
   --  Value, or use Set_Expression_Of to set the value of the attribute
   --  (in which case Enclose_In_Expression might be useful). The former is
   --  recommended since it will more correctly handle cases where the index
   --  needs to be set on the expression rather than on the index of the
   --  attribute ('for Specification ("unit") use "file" at 3', versus
   --  'for Executable ("file" at 3) use "name"'). Value must be a
   --  N_String_Literal if an index will be added to it

   function Create_Literal_String
     (Str  : Namet.Name_Id;
      Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Create a literal string whose value is Str

   procedure Add_At_End
     (Tree                  : Project_Node_Tree_Ref;
      Parent                : Project_Node_Id;
      Expr                  : Project_Node_Id;
      Add_Before_First_Pkg  : Boolean := False;
      Add_Before_First_Case : Boolean := False);
   --  Add a new declarative item in the list in Parent. This new declarative
   --  item will contain Expr (unless Expr is already a declarative item, in
   --  which case it is added directly to the list). The new item is inserted
   --  at the end of the list, unless Add_Before_First_Pkg is True. In the
   --  latter case, it is added just before the first case construction is
   --  seen, or before the first package (this assumes that all packages are
   --  found at the end of the project, which isn't true in the general case
   --  unless you have normalized the project to match this description).

   function Enclose_In_Expression
     (Node : Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Enclose the Node inside a N_Expression node, and return this expression.
   --  This does nothing if Node is already a N_Expression

   --------------------
   -- Set Procedures --
   --------------------

   --  The following procedures are part of the abstract interface of the
   --  Project File tree.

   --  Foe each Set_* procedure the condition of validity is specified. If an
   --  access function is called with invalid arguments, then exception
   --  Assertion_Error is raised if assertions are enabled, otherwise the
   --  behaviour is not defined and may result in a crash.

   --  These are very low-level, and manipulate the tree itself directly. You
   --  should look at the Create_* procedure instead if you want to use higher
   --  level constructs

   procedure Set_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_Name_Of);
   --  Valid for all non empty nodes.

   procedure Set_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Kind);
   pragma Inline (Set_Kind_Of);
   --  Valid for all non empty nodes

   procedure Set_Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Source_Ptr);
   pragma Inline (Set_Location_Of);
   --  Valid for all non empty nodes

   procedure Set_First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_After);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_After_End);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_Before);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_Before_End);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Comment);
   --  Valid only for N_Comment nodes

   procedure Set_Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   --  Valid only for N_Project nodes

   procedure Set_Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean);
   --  Valid only for N_Project nodes

   procedure Set_Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Directory_Of);
   --  Valid only for N_Project nodes

   procedure Set_Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Kind);
   pragma Inline (Set_Expression_Kind_Of);
   --  Only valid for N_Literal_String, N_Attribute_Declaration,
   --  N_Variable_Declaration, N_Typed_Variable_Declaration, N_Expression,
   --  N_Term, N_Variable_Reference or N_Attribute_Reference nodes.

   procedure Set_Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref);
   pragma Inline (Set_Is_Extending_All);
   --  Only valid for N_Project and N_With_Clause

   procedure Set_Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref);
   pragma Inline (Set_Is_Not_Last_In_List);
   --  Only valid for N_With_Clause

   procedure Set_First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Node_Id);
   pragma Inline (Set_First_Variable_Of);
   --  Only valid for N_Project or N_Package_Declaration nodes

   procedure Set_First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Declaration_Id);
   pragma Inline (Set_First_Package_Of);
   --  Only valid for N_Project nodes

   procedure Set_Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Node_Id);
   pragma Inline (Set_Package_Id_Of);
   --  Only valid for N_Package_Declaration nodes

   procedure Set_Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Path_Name_Of);
   --  Only valid for N_Project and N_With_Clause nodes

   procedure Set_String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_String_Value_Of);
   --  Only valid for N_With_Clause, N_Literal_String nodes or N_Comment.

   procedure Set_Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Int);
   pragma Inline (Set_Source_Index_Of);
   --  Only valid for N_Literal_String and N_Attribute_Declaration nodes. For
   --  N_Literal_String, set the source index of the litteral string. For
   --  N_Attribute_Declaration, set the source index of the index of the
   --  associative array element.

   procedure Set_First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_With_Clause_Of);
   --  Only valid for N_Project nodes

   procedure Set_Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Project_Declaration_Of);
   --  Only valid for N_Project nodes

   procedure Set_Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Qualifier);
   pragma Inline (Set_Project_Qualifier_Of);
   --  Only valid for N_Project nodes

   procedure Set_Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Extending_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   procedure Set_First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_String_Type_Of);
   --  Only valid for N_Project nodes

   procedure Set_Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Extended_Project_Path_Of);
   --  Only valid for N_With_Clause nodes

   procedure Set_Project_Node_Of
     (Node         : Project_Node_Id;
      In_Tree      : Project_Node_Tree_Ref;
      To           : Project_Node_Id;
      Limited_With : Boolean := False);
   pragma Inline (Set_Project_Node_Of);
   --  Only valid for N_With_Clause, N_Variable_Reference and
   --  N_Attribute_Reference nodes.

   procedure Set_Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_With_Clause_Of);
   --  Only valid for N_With_Clause nodes

   procedure Set_First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Declarative_Item_Of);
   --  Only valid for N_Project_Declaration, N_Case_Item and
   --  N_Package_Declaration.

   procedure Set_Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Extended_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   procedure Set_Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Current_Item_Node);
   --  Only valid for N_Declarative_Item nodes

   procedure Set_Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Declarative_Item);
   --  Only valid for N_Declarative_Item node

   procedure Set_Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Project_Of_Renamed_Package_Of);
   --  Only valid for N_Package_Declaration nodes.

   procedure Set_Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Package_In_Project);
   --  Only valid for N_Package_Declaration nodes

   procedure Set_First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Literal_String);
   --  Only valid for N_String_Type_Declaration nodes

   procedure Set_Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_String_Type);
   --  Only valid for N_String_Type_Declaration nodes

   procedure Set_Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Literal_String);
   --  Only valid for N_Literal_String nodes

   procedure Set_Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Expression_Of);
   --  Only valid for N_Attribute_Declaration, N_Typed_Variable_Declaration
   --  or N_Variable_Declaration nodes

   procedure Set_Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Associative_Project_Of);
   --  Only valid for N_Attribute_Declaration nodes

   procedure Set_Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Associative_Package_Of);
   --  Only valid for N_Attribute_Declaration nodes

   procedure Set_Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_Associative_Array_Index_Of);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference.

   procedure Set_Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Variable);
   --  Only valid for N_Typed_Variable_Declaration or N_Variable_Declaration
   --  nodes.

   procedure Set_First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Term);
   --  Only valid for N_Expression nodes

   procedure Set_Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Expression_In_List);
   --  Only valid for N_Expression nodes

   procedure Set_Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Current_Term);
   --  Only valid for N_Term nodes

   procedure Set_Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Term);
   --  Only valid for N_Term nodes

   procedure Set_First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Expression_In_List);
   --  Only valid for N_Literal_String_List nodes

   procedure Set_Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Package_Node_Of);
   --  Only valid for N_Variable_Reference or N_Attribute_Reference nodes.

   procedure Set_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_String_Type_Of);
   --  Only valid for N_Variable_Reference or N_Typed_Variable_Declaration
   --  nodes.

   procedure Set_External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_External_Reference_Of);
   --  Only valid for N_External_Value nodes

   procedure Set_External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_External_Default_Of);
   --  Only valid for N_External_Value nodes

   procedure Set_Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Case_Variable_Reference_Of);
   --  Only valid for N_Case_Construction nodes

   procedure Set_First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Case_Item_Of);
   --  Only valid for N_Case_Construction nodes

   procedure Set_First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Choice_Of);
   --  Only valid for N_Case_Item nodes.

   procedure Set_Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Case_Item);
   --  Only valid for N_Case_Item nodes.

   procedure Set_Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference nodes

   -------------------------------
   -- Restricted Access Section --
   -------------------------------

   package Tree_Private_Part is

      --  This is conceptually in the private part. However, for efficiency,
      --  some packages are accessing it directly.

      type Project_Node_Record is record

         Kind : Project_Node_Kind;

         Qualifier : Project_Qualifier := Unspecified;

         Location : Source_Ptr := No_Location;

         Directory : Path_Name_Type := No_Path;
         --  Only for N_Project

         Expr_Kind : Variable_Kind := Undefined;
         --  See below for what Project_Node_Kind it is used

         Variables : Variable_Node_Id := Empty_Node;
         --  First variable in a project or a package

         Packages : Package_Declaration_Id := Empty_Node;
         --  First package declaration in a project

         Pkg_Id : Package_Node_Id := Empty_Package;
         --  Only used for N_Package_Declaration
         --
         --  The component Pkg_Id is an entry into the table Package_Attributes
         --  (in Prj.Attr). It is used to indicate all the attributes of the
         --  package with their characteristics.
         --
         --  The tables Prj.Attr.Attributes and Prj.Attr.Package_Attributes
         --  are built once and for all through a call (from Prj.Initialize)
         --  to procedure Prj.Attr.Initialize. It is never modified after that.

         Name : Name_Id := No_Name;
         --  See below for what Project_Node_Kind it is used

         Src_Index : Int := 0;
         --  Index of a unit in a multi-unit source.
         --  Only for some N_Attribute_Declaration and N_Literal_String.

         Path_Name : Path_Name_Type := No_Path;
         --  See below for what Project_Node_Kind it is used

         Value : Name_Id := No_Name;
         --  See below for what Project_Node_Kind it is used

         Field1 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Field2 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Field3 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Field4 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Flag1 : Boolean := False;
         --  This flag is significant only for:
         --
         --    N_Attribute_Declaration and N_Attribute_Reference
         --      Indicates for an associative array attribute, that the
         --      index is case insensitive.
         --
         --    N_Comment
         --      Indicates that the comment is preceded by an empty line.
         --
         --    N_Project
         --      Indicates that there are comments in the project source that
         --      cannot be kept in the tree.
         --
         --    N_Project_Declaration
         --      Indicates that there are unkept comments in the project.
         --
         --    N_With_Clause
         --      Indicates that this is not the last with in a with clause.
         --      Set for "A", but not for "B" in with "B"; and with "A", "B";

         Flag2 : Boolean := False;
         --  This flag is significant only for:
         --
         --    N_Project
         --      Indicates that the project "extends all" another project.
         --
         --    N_Comment
         --      Indicates that the comment is followed by an empty line.
         --
         --    N_With_Clause
         --      Indicates that the originally imported project is an extending
         --      all project.

         Comments : Project_Node_Id := Empty_Node;
         --  For nodes other that N_Comment_Zones or N_Comment, designates the
         --  comment zones associated with the node.
         --
         --  For N_Comment_Zones, designates the comment after the "end" of
         --  the construct.
         --
         --  For N_Comment, designates the next comment, if any.

      end record;

      --  type Project_Node_Kind is

      --   (N_Project,
      --    --  Name:      project name
      --    --  Path_Name: project path name
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first with clause
      --    --  Field2:    project declaration
      --    --  Field3:    first string type
      --    --  Field4:    parent project, if any
      --    --  Value:     extended project path name (if any)

      --    N_With_Clause,
      --    --  Name:      imported project name
      --    --  Path_Name: imported project path name
      --    --  Expr_Kind: Undefined
      --    --  Field1:    project node
      --    --  Field2:    next with clause
      --    --  Field3:    project node or empty if "limited with"
      --    --  Field4:    not used
      --    --  Value:     literal string withed

      --    N_Project_Declaration,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first declarative item
      --    --  Field2:    extended project
      --    --  Field3:    extending project
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Declarative_Item,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    current item node
      --    --  Field2:    next declarative item
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Package_Declaration,
      --    --  Name:      package name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    project of renamed package (if any)
      --    --  Field2:    first declarative item
      --    --  Field3:    next package in project
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_String_Type_Declaration,
      --    --  Name:      type name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first literal string
      --    --  Field2:    next string type
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Literal_String,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Single
      --    --  Field1:    next literal string
      --    --  Field2:    not used
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     string value

      --    N_Attribute_Declaration,
      --    --  Name:      attribute name
      --    --  Path_Name: not used
      --    --  Expr_Kind: attribute kind
      --    --  Field1:    expression
      --    --  Field2:    project of full associative array
      --    --  Field3:    package of full associative array
      --    --  Field4:    not used
      --    --  Value:     associative array index
      --    --             (if an associative array element)

      --    N_Typed_Variable_Declaration,
      --    --  Name:      variable name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Single
      --    --  Field1:    expression
      --    --  Field2:    type of variable (N_String_Type_Declaration)
      --    --  Field3:    next variable
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Variable_Declaration,
      --    --  Name:      variable name
      --    --  Path_Name: not used
      --    --  Expr_Kind: variable kind
      --    --  Field1:    expression
      --    --  Field2:    not used
      --    --             Field3 is used for next variable, instead of Field2,
      --    --             so that it is the same field for
      --    --             N_Variable_Declaration and
      --    --             N_Typed_Variable_Declaration
      --    --  Field3:    next variable
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Expression,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: expression kind
      --    --  Field1:    first term
      --    --  Field2:    next expression in list
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Term,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: term kind
      --    --  Field1:    current term
      --    --  Field2:    next term in the expression
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Literal_String_List,
      --    --  Designates a list of string expressions between brackets
      --    --  separated by commas. The string expressions are not necessarily
      --    --  literal strings.
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: List
      --    --  Field1:    first expression
      --    --  Field2:    not used
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Variable_Reference,
      --    --  Name:      variable name
      --    --  Path_Name: not used
      --    --  Expr_Kind: variable kind
      --    --  Field1:    project (if specified)
      --    --  Field2:    package (if specified)
      --    --  Field3:    type of variable (N_String_Type_Declaration), if any
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_External_Value,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Single
      --    --  Field1:    Name of the external reference (literal string)
      --    --  Field2:    Default (literal string)
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Attribute_Reference,
      --    --  Name:      attribute name
      --    --  Path_Name: not used
      --    --  Expr_Kind: attribute kind
      --    --  Field1:    project
      --    --  Field2:    package (if attribute of a package)
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     associative array index
      --    --             (if an associative array element)

      --    N_Case_Construction,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    case variable reference
      --    --  Field2:    first case item
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Case_Item
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: not used
      --    --  Field1:    first choice (literal string), or Empty_Node
      --    --             for when others
      --    --  Field2:    first declarative item
      --    --  Field3:    next case item
      --    --  Field4:    not used
      --    --  Value:     not used

      --    N_Comment_zones
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: not used
      --    --  Field1:    comment before the construct
      --    --  Field2:    comment after the construct
      --    --  Field3:    comment before the "end" of the construct
      --    --  Value:     end of line comment
      --    --  Field4:    not used
      --    --  Comments:  comment after the "end" of the construct

      --    N_Comment
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: not used
      --    --  Field1:    not used
      --    --  Field2:    not used
      --    --  Field3:    not used
      --    --  Field4:    not used
      --    --  Value:     comment
      --    --  Flag1:     comment is preceded by an empty line
      --    --  Flag2:     comment is followed by an empty line
      --    --  Comments:  next comment

      package Project_Node_Table is new
        GNAT.Dynamic_Tables
          (Table_Component_Type => Project_Node_Record,
           Table_Index_Type     => Project_Node_Id,
           Table_Low_Bound      => First_Node_Id,
           Table_Initial        => Project_Nodes_Initial,
           Table_Increment      => Project_Nodes_Increment);
      --  Table contains the syntactic tree of project data from project files

      type Project_Name_And_Node is record
         Name : Name_Id;
         --  Name of the project

         Display_Name : Name_Id;
         --  The name of the project as it appears in the .gpr file

         Node : Project_Node_Id;
         --  Node of the project in table Project_Nodes

         Canonical_Path : Path_Name_Type;
         --  Resolved and canonical path of a real project file.
         --  No_Name in case of virtual projects.

         Extended : Boolean;
         --  True when the project is being extended by another project

         Proj_Qualifier : Project_Qualifier;
         --  The project qualifier of the project, if any
      end record;

      No_Project_Name_And_Node : constant Project_Name_And_Node :=
        (Name           => No_Name,
         Display_Name   => No_Name,
         Node           => Empty_Node,
         Canonical_Path => No_Path,
         Extended       => True,
         Proj_Qualifier => Unspecified);

      package Projects_Htable is new GNAT.Dynamic_HTables.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Project_Name_And_Node,
         No_Element => No_Project_Name_And_Node,
         Key        => Name_Id,
         Hash       => Hash,
         Equal      => "=");
      --  This hash table contains a mapping of project names to project nodes.
      --  Note that this hash table contains only the nodes whose Kind is
      --  N_Project. It is used to find the node of a project from its name,
      --  and to verify if a project has already been parsed, knowing its name.

   end Tree_Private_Part;

   package Name_To_Name_HTable is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  General type for htables associating name_id to name_id. This is in
   --  particular used to store the values of external references.

   type Project_Node_Tree_Data is record
      Project_Nodes : Tree_Private_Part.Project_Node_Table.Instance;
      Projects_HT   : Tree_Private_Part.Projects_Htable.Instance;

      External_References : Name_To_Name_HTable.Instance;
      --  External references are stored in this hash table (and manipulated
      --  through subprogrames in prj-ext.ads). External references are
      --  project-tree specific so that one can load the same tree twice but
      --  have two views of it, for instance.

      Project_Path : String_Access;
      --  The project path, manipulated through subprograms in prj-ext.ads.
      --  As a special case, if the first character is '#:" or this variable is
      --  unset, this means that the PATH has not been fully initialized yet
      --  (although subprograms prj-ext.ads will properly take care of that).
      --
      --  The project path is tree specific, since we might want to load
      --  simultaneously multiple projects, each with its own search path, in
      --  particular when using different compilers with different default
      --  search directories.
   end record;

   procedure Free (Proj : in out Project_Node_Tree_Ref);
   --  Free memory used by Prj

private
   type Comment_Array is array (Positive range <>) of Comment_Data;
   type Comments_Ptr is access Comment_Array;

   type Comment_State is record
      End_Of_Line_Node   : Project_Node_Id := Empty_Node;
      Previous_Line_Node : Project_Node_Id := Empty_Node;
      Previous_End_Node  : Project_Node_Id := Empty_Node;
      Unkept_Comments    : Boolean := False;
      Comments           : Comments_Ptr := null;
   end record;

end Prj.Tree;
