------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . T R E E                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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

--  This package defines the structure of the Project File tree.

with GNAT.HTable;

with Prj.Attr; use Prj.Attr;
with Prj.Com;  use Prj.Com;
with Types;    use Types;
with Table;

package Prj.Tree is

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
   --  Used to designate a node whose expected kind is N_Proect_Declaration

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
      N_Case_Item);
   --  Each node in the tree is of a Project_Node_Kind
   --  For the signification of the fields in each node of a
   --  Project_Node_Kind, look at package Tree_Private_Part.

   procedure Initialize;
   --  Initialize the Project File tree: empty the Project_Nodes table
   --  and reset the Projects_Htable.

   function Default_Project_Node
     (Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined)
      return          Project_Node_Id;
   --  Returns a Project_Node_Record with the specified Kind and
   --  Expr_Kind; all the other components have default nil values.

   ----------------------
   -- Access Functions --
   ----------------------

   --  The following query functions are part of the abstract interface
   --  of the Project File tree

   function Name_Of (Node : Project_Node_Id) return Name_Id;
   --  Valid for all non empty nodes. May return No_Name for nodes that have
   --  no names.

   function Kind_Of (Node : Project_Node_Id) return Project_Node_Kind;
   --  Valid for all non empty nodes

   function Location_Of (Node : Project_Node_Id) return Source_Ptr;
   --  Valid for all non empty nodes

   function Directory_Of (Node : Project_Node_Id) return Name_Id;
   --  Only valid for N_Project nodes.

   function Expression_Kind_Of (Node : Project_Node_Id) return Variable_Kind;
   --  Only valid for N_Literal_String, N_Attribute_Declaration,
   --  N_Variable_Declaration, N_Typed_Variable_Declaration, N_Expression,
   --  N_Term, N_Variable_Reference or N_Attribute_Reference nodes.

   function First_Variable_Of
     (Node  : Project_Node_Id)
      return  Variable_Node_Id;
   --  Only valid for N_Project or N_Package_Declaration nodes

   function First_Package_Of
     (Node  : Project_Node_Id)
      return  Package_Declaration_Id;
   --  Only valid for N_Project nodes

   function Package_Id_Of (Node  : Project_Node_Id) return Package_Node_Id;
   --  Only valid for N_Package_Declaration nodes

   function Path_Name_Of (Node  : Project_Node_Id) return Name_Id;
   --  Only valid for N_Project and N_With_Clause nodes.

   function String_Value_Of (Node  : Project_Node_Id) return String_Id;
   --  Only valid for N_With_Clause or N_Literal_String nodes.

   function First_With_Clause_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Project nodes

   function Project_Declaration_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Project nodes

   function First_String_Type_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Project nodes

   function Modified_Project_Path_Of
     (Node  : Project_Node_Id)
      return  String_Id;
   --  Only valid for N_With_Clause nodes

   function Project_Node_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Project nodes

   function Next_With_Clause_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_With_Clause nodes

   function First_Declarative_Item_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_With_Clause nodes

   function Modified_Project_Of
     (Node  : Project_Node_Id)
      return Project_Node_Id;
   --  Only valid for N_With_Clause nodes

   function Current_Item_Node
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Declarative_Item nodes

   function Next_Declarative_Item
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Declarative_Item node

   function Project_Of_Renamed_Package_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Package_Declaration nodes.
   --  May return Empty_Node.

   function Next_Package_In_Project
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Package_Declaration nodes

   function First_Literal_String
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_String_Type_Declaration nodes

   function Next_String_Type
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_String_Type_Declaration nodes

   function Next_Literal_String
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Literal_String nodes

   function Expression_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Attribute_Declaration, N_Typed_Variable_Declaration
   --  or N_Variable_Declaration nodes

   function Value_Is_Valid
     (For_Typed_Variable : Project_Node_Id;
      Value              : String_Id)
      return               Boolean;
   --  Only valid for N_Typed_Variable_Declaration. Returns True if Value is
   --  in the list of allowed strings for For_Typed_Variable. False otherwise.

   function Associative_Array_Index_Of
     (Node  : Project_Node_Id)
      return  String_Id;
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference.
   --  Returns No_String for non associative array attributes.

   function Next_Variable
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Typed_Variable_Declaration or N_Variable_Declaration
   --  nodes.

   function First_Term
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Expression nodes

   function Next_Expression_In_List
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Expression nodes

   function Current_Term
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Term nodes

   function Next_Term
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Term nodes

   function First_Expression_In_List
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Literal_String_List nodes

   function Package_Node_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Variable_Reference or N_Attribute_Reference nodes.
   --  May return Empty_Node.

   function String_Type_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Variable_Reference or N_Typed_Variable_Declaration
   --  nodes.

   function External_Reference_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_External_Value nodes

   function External_Default_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_External_Value nodes

   function Case_Variable_Reference_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Case_Construction nodes

   function First_Case_Item_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Case_Construction nodes

   function First_Choice_Of
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Return the first choice in a N_Case_Item, or Empty_Node if
   --  this is when others.

   function Next_Case_Item
     (Node  : Project_Node_Id)
      return  Project_Node_Id;
   --  Only valid for N_Case_Item nodes

   function Case_Insensitive (Node : Project_Node_Id) return Boolean;
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference nodes

   --------------------
   -- Set Procedures --
   --------------------

   --  The following procedures are part of the abstract interface of
   --  the Project File tree.

   --  Each Set_* procedure is valid only for the same Project_Node_Kind
   --  nodes as the corresponding query function above.

   procedure Set_Name_Of
     (Node : Project_Node_Id;
      To   : Name_Id);

   procedure Set_Kind_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Kind);

   procedure Set_Location_Of
     (Node : Project_Node_Id;
      To   : Source_Ptr);

   procedure Set_Directory_Of
     (Node : Project_Node_Id;
      To   : Name_Id);

   procedure Set_Expression_Kind_Of
     (Node : Project_Node_Id;
      To   : Variable_Kind);

   procedure Set_First_Variable_Of
     (Node : Project_Node_Id;
      To   : Variable_Node_Id);

   procedure Set_First_Package_Of
     (Node : Project_Node_Id;
      To   : Package_Declaration_Id);

   procedure Set_Package_Id_Of
     (Node : Project_Node_Id;
      To   : Package_Node_Id);

   procedure Set_Path_Name_Of
     (Node : Project_Node_Id;
      To   : Name_Id);

   procedure Set_String_Value_Of
     (Node : Project_Node_Id;
      To   : String_Id);

   procedure Set_First_With_Clause_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Project_Declaration_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_String_Type_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Modified_Project_Path_Of
     (Node : Project_Node_Id;
      To   : String_Id);

   procedure Set_Project_Node_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_With_Clause_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Declarative_Item_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Modified_Project_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Current_Item_Node
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Declarative_Item
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Project_Of_Renamed_Package_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Package_In_Project
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Literal_String
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_String_Type
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Literal_String
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Expression_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Associative_Array_Index_Of
     (Node : Project_Node_Id;
      To   : String_Id);

   procedure Set_Next_Variable
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Expression_In_List
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Current_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Expression_In_List
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Package_Node_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_String_Type_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_External_Reference_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_External_Default_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Case_Variable_Reference_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Case_Item_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_First_Choice_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Next_Case_Item
     (Node : Project_Node_Id;
      To   : Project_Node_Id);

   procedure Set_Case_Insensitive
     (Node : Project_Node_Id;
      To   : Boolean);

   -------------------------------
   -- Restricted Access Section --
   -------------------------------

   package Tree_Private_Part is

      --  This is conceptually in the private part.
      --  However, for efficiency, some packages are accessing it directly.

      type Project_Node_Record is record

         Kind : Project_Node_Kind;

         Location : Source_Ptr := No_Location;

         Directory : Name_Id       := No_Name;
         --  Only for N_Project

         Expr_Kind : Variable_Kind := Undefined;
         --  See below for what Project_Node_Kind it is used

         Variables : Variable_Node_Id := Empty_Node;
         --  First variable in a project or a package

         Packages : Package_Declaration_Id := Empty_Node;
         --  First package declaration in a project

         Pkg_Id : Package_Node_Id := Empty_Package;
         --  Only used for N_Package_Declaration
         --  The component Pkg_Id is an entry into the table Package_Attributes
         --  (in Prj.Attr). It is used to indicate all the attributes of the
         --  package with their characteristics.
         --
         --  The tables Prj.Attr.Attributes and Prj.Attr.Package_Attributes
         --  are built once and for all through a call (from Prj.Initialize)
         --  to procedure Prj.Attr.Initialize. It is never modified after that.

         Name : Name_Id := No_Name;
         --  See below for what Project_Node_Kind it is used

         Path_Name : Name_Id := No_Name;
         --  See below for what Project_Node_Kind it is used

         Value : String_Id := No_String;
         --  See below for what Project_Node_Kind it is used

         Field1 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Field2 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Field3 : Project_Node_Id := Empty_Node;
         --  See below the meaning for each Project_Node_Kind

         Case_Insensitive : Boolean := False;
         --  This flag is significant only for N_Attribute_Declaration and
         --  N_Atribute_Reference. It indicates for an associative array
         --  attribute, that the index is case insensitive.

      end record;

      --  type Project_Node_Kind is

      --   (N_Project,
      --    --  Name:      project name
      --    --  Path_Name: project path name
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first with clause
      --    --  Field2:    project declaration
      --    --  Field3:    first string type
      --    --  Value:     modified project path name (if any)

      --    N_With_Clause,
      --    --  Name:      imported project name
      --    --  Path_Name: imported project path name
      --    --  Expr_Kind: Undefined
      --    --  Field1:    project node
      --    --  Field2:    next with clause
      --    --  Field3:    not used
      --    --  Value:     literal string withed

      --    N_Project_Declaration,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first declarative item
      --    --  Field2:    modified project
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Declarative_Item,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    current item node
      --    --  Field2:    next declarative item
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Package_Declaration,
      --    --  Name:      package name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    project of renamed package (if any)
      --    --  Field2:    first declarative item
      --    --  Field3:    next package in project
      --    --  Value:     not used

      --    N_String_Type_Declaration,
      --    --  Name:      type name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    first literal string
      --    --  Field2:    next string type
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Literal_String,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Single
      --    --  Field1:    next literal string
      --    --  Field2:    not used
      --    --  Field3:    not used
      --    --  Value:     string value

      --    N_Attribute_Declaration,
      --    --  Name:      attribute name
      --    --  Path_Name: not used
      --    --  Expr_Kind: attribute kind
      --    --  Field1:    expression
      --    --  Field2:    not used
      --    --  Field3:    not used
      --    --  Value:     associative array index
      --    --             (if an associative array element)

      --    N_Typed_Variable_Declaration,
      --    --  Name:      variable name
      --    --  Path_Name: not used
      --    --  Expr_Kind: Single
      --    --  Field1:    expression
      --    --  Field2:    type of variable (N_String_Type_Declaration)
      --    --  Field3:    next variable
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
      --    --  Value:     not used

      --    N_Variable_Reference,
      --    --  Name:      variable name
      --    --  Path_Name: not used
      --    --  Expr_Kind: variable kind
      --    --  Field1:    project (if specified)
      --    --  Field2:    package (if specified)
      --    --  Field3:    type of variable (N_String_Type_Declaration), if any
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
      --    --  Value:     associative array index
      --    --             (if an associative array element)

      --    N_Case_Construction,
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: Undefined
      --    --  Field1:    case variable reference
      --    --  Field2:    first case item
      --    --  Field3:    not used
      --    --  Value:     not used

      --    N_Case_Item);
      --    --  Name:      not used
      --    --  Path_Name: not used
      --    --  Expr_Kind: not used
      --    --  Field1:    first choice (literal string), or Empty_Node
      --    --             for when others
      --    --  Field2:    first declarative item
      --    --  Field3:    next case item
      --    --  Value:     not used

      package Project_Nodes is
         new Table.Table (Table_Component_Type => Project_Node_Record,
                          Table_Index_Type     => Project_Node_Id,
                          Table_Low_Bound      => First_Node_Id,
                          Table_Initial        => Project_Nodes_Initial,
                          Table_Increment      => Project_Nodes_Increment,
                          Table_Name           => "Project_Nodes");
      --  This table contains the syntactic tree of project data
      --  from project files.

      type Project_Name_And_Node is record
         Name : Name_Id;
         --  Name of the project

         Node : Project_Node_Id;
         --  Node of the project in table Project_Nodes

         Modified : Boolean;
         --  True when the project is being modified by another project
      end record;

      No_Project_Name_And_Node : constant Project_Name_And_Node :=
        (Name => No_Name, Node => Empty_Node, Modified => True);

      package Projects_Htable is new GNAT.HTable.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Project_Name_And_Node,
         No_Element => No_Project_Name_And_Node,
         Key        => Name_Id,
         Hash       => Hash,
         Equal      => "=");
      --  This hash table contains a mapping of project names to project nodes.
      --  Note that this hash table contains only the nodes whose Kind is
      --  N_Project. It is used to find the node of a project from its
      --  name, and to verify if a project has already been parsed, knowing
      --  its name.

   end Tree_Private_Part;

end Prj.Tree;
