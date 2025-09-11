------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains various utility procedures to assist in building
--  specific types of tree nodes.

with Namet;          use Namet;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Types;          use Types;
with Uintp;          use Uintp;

package Tbuild is

   function Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id;
   --  Returns an expression that is a type conversion of expression Expr to
   --  type Typ. If the type of Expr is Typ, then no conversion is required.
   --  Otherwise an N_Type_Conversion node is constructed to convert the
   --  expression. Relocate_Node is applied to Expr, so that it is safe to
   --  replace a node by a Convert_To of itself to some other type.

   procedure Convert_To_And_Rewrite (Typ : Entity_Id; Expr : Node_Id);
   pragma Inline (Convert_To_And_Rewrite);
   --  Like the function, except that there is an extra step of calling
   --  Rewrite on the Expr node and replacing it with the converted result.

   procedure Discard_Node (N : Node_Or_Entity_Id);
   pragma Inline (Discard_Node);
   --  This is a dummy procedure that simply returns and does nothing. It is
   --  used when a function returning a Node_Id value is called for its side
   --  effect (e.g. a call to Make to construct a node) but the Node_Id value
   --  is not required.

   procedure Discard_List (L : List_Id);
   pragma Inline (Discard_List);
   --  This is a dummy procedure that simply returns and does nothing. It is
   --  used when a function returning a Node_Id value is called for its side
   --  effect (e.g. a call to the parser to parse a list of compilation
   --  units), but the List_Id value is not required.

   function Make_Assertion_Level
     (Loc : Source_Ptr; Nam : Name_Id) return Entity_Id;
   --  Create a new Defining_Identifier node for an Assertion_Level

   function Make_Byte_Aligned_Attribute_Reference
     (Sloc           : Source_Ptr;
      Prefix         : Node_Id;
      Attribute_Name : Name_Id) return Node_Id;
   pragma Inline (Make_Byte_Aligned_Attribute_Reference);
   --  Like the standard Make_Attribute_Reference but the special flag
   --  Must_Be_Byte_Aligned is set in the attribute reference node. The
   --  Attribute_Name must be Name_Address or Name_Unrestricted_Access.

   function Make_Float_Literal
     (Loc         : Source_Ptr;
      Radix       : Uint;
      Significand : Uint;
      Exponent    : Uint) return Node_Id;
   --  Create a real literal for the floating point expression value
   --  Significand * Radix ** Exponent. Radix must be greater than 1.

   function Make_Implicit_Exception_Handler
     (Sloc              : Source_Ptr;
      Choice_Parameter  : Node_Id := Empty;
      Exception_Choices : List_Id;
      Statements        : List_Id) return Node_Id;
   pragma Inline (Make_Implicit_Exception_Handler);
   --  This is just like Make_Exception_Handler, except that it also sets the
   --  Local_Raise_Statements field to No_Elist, ensuring that it is properly
   --  initialized. This should always be used when creating implicit exception
   --  handlers during expansion (i.e. handlers that do not correspond to user
   --  source program exception handlers).

   function Make_Implicit_If_Statement
     (Node            : Node_Id;
      Condition       : Node_Id;
      Then_Statements : List_Id;
      Elsif_Parts     : List_Id := No_List;
      Else_Statements : List_Id := No_List) return Node_Id;
   pragma Inline (Make_Implicit_If_Statement);
   --  This function makes an N_If_Statement node whose fields are filled
   --  in with the indicated values (see Sinfo), and whose Sloc field is
   --  is set to Sloc (Node). The effect is identical to calling function
   --  Nmake.Make_If_Statement except that there is a check for restriction
   --  No_Implicit_Conditionals, and if this restriction is being violated,
   --  an error message is posted on Node.

   function Make_Implicit_Label_Declaration
     (Loc                 : Source_Ptr;
      Defining_Identifier : Node_Id;
      Label_Construct     : Node_Id) return Node_Id;
   --  Used to construct an implicit label declaration node, including setting
   --  the proper Label_Construct field (since Label_Construct is a semantic
   --  field, the normal call to Make_Implicit_Label_Declaration does not
   --  set this field).

   function Make_Implicit_Loop_Statement
     (Node                   : Node_Id;
      Statements             : List_Id;
      Identifier             : Node_Id := Empty;
      Iteration_Scheme       : Node_Id := Empty;
      Has_Created_Identifier : Boolean := False;
      End_Label              : Node_Id := Empty) return Node_Id;
   --  This function makes an N_Loop_Statement node whose fields are filled
   --  in with the indicated values (see Sinfo), and whose Sloc field is
   --  is set to Sloc (Node). The effect is identical to calling function
   --  Nmake.Make_Loop_Statement except that there is a check for restrictions
   --  No_Implicit_Loops and No_Implicit_Conditionals (the first applying in
   --  all cases, and the second only for while loops), and if one of these
   --  restrictions is being violated, an error message is posted on Node.

   function Make_Increment
     (Loc : Source_Ptr; Index : Entity_Id; Typ : Entity_Id) return Node_Id;
   --  Return an assignment statement of the form "Index := Typ'Succ (Index);"

   function Make_Integer_Literal
     (Loc    : Source_Ptr;
      Intval : Int) return Node_Id;
   pragma Inline (Make_Integer_Literal);
   --  A convenient form of Make_Integer_Literal taking Int instead of Uint

   function Make_Linker_Section_Pragma
     (Ent : Entity_Id;
      Loc : Source_Ptr;
      Sec : String) return Node_Id;
   --  Construct a Linker_Section pragma for entity Ent, using string Sec as
   --  the section name. Loc is the Sloc value to use in building the pragma.

   function Make_Pragma
     (Sloc                         : Source_Ptr;
      Chars                        : Name_Id;
      Pragma_Argument_Associations : List_Id := No_List) return Node_Id;
   --  A convenient form of Make_Pragma not requiring a Pragma_Identifier
   --  argument (this argument is built from the value given for Chars).

   function Make_Raise_Constraint_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id;
   pragma Inline (Make_Raise_Constraint_Error);
   --  A convenient form of Make_Raise_Constraint_Error where the Reason
   --  is given simply as an enumeration value, rather than a Uint code.

   function Make_Raise_Program_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id;
   pragma Inline (Make_Raise_Program_Error);
   --  A convenient form of Make_Raise_Program_Error where the Reason
   --  is given simply as an enumeration value, rather than a Uint code.

   function Make_Raise_Storage_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code) return Node_Id;
   pragma Inline (Make_Raise_Storage_Error);
   --  A convenient form of Make_Raise_Storage_Error where the Reason is given
   --  simply as an enumeration value, rather than a Uint code.

   function Make_String_Literal
     (Sloc   : Source_Ptr;
      Strval : String) return Node_Id;
   --  A convenient form of Make_String_Literal, where the string value is
   --  given as a normal string instead of a String_Id value.

   function Make_Suppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id) return Node_Id;
   --  Build a block with a pragma Suppress on Check. Stmts is the statements
   --  list that needs protection against the check activation.

   function Make_Temporary
     (Loc          : Source_Ptr;
      Id           : Character;
      Related_Node : Node_Id := Empty) return Entity_Id;
   --  This function should be used for all cases where a defining identifier
   --  is to be built with a name to be obtained by New_Internal_Name (here Id
   --  is the character passed as the argument to New_Internal_Name). Loc is
   --  the location for the Sloc value of the resulting Entity. Note that this
   --  can be used for all kinds of temporary defining identifiers used in
   --  expansion (objects, subtypes, functions etc).
   --
   --  Related_Node is used when the defining identifier is for an object that
   --  captures the value of an expression (e.g. an aggregate). It should be
   --  set whenever possible to point to the expression that is being captured.
   --  This is provided to get better error messages, e.g. from CodePeer.

   function Make_Unsuppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id) return Node_Id;
   --  Build a block with a pragma Unsuppress on Check. Stmts is the statements
   --  list that needs protection against the check suppression.

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id;
   --  This function builds a tree corresponding to the Ada statement
   --  "raise Constraint_Error" and returns the root of this tree,
   --  the N_Raise_Statement node.

   function New_Op_Node
     (New_Node_Kind : Node_Kind;
      New_Sloc      : Source_Ptr) return Node_Id;
   --  Create node using New_Node and, if its kind is in N_Op, set its Chars
   --  field accordingly.

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : Character := ' ';
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ') return Name_Id;
   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : String;
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ') return Name_Id;
   --  Builds a new entry in the names table of the form:
   --
   --    [Prefix  &] Related_Id [& Suffix] [& Suffix_Index]
   --
   --  Prefix is prepended only if Prefix is non-blank (in which case it
   --  must be an upper case letter other than O,Q,U,W (which are used for
   --  identifier encoding, see Namet), or an underscore, and T is reserved for
   --  use by implicit types, and X is reserved for use by debug type encoding
   --  (see package Exp_Dbug). Note: the reason that Prefix is last is that it
   --  is almost always omitted. The notable case of Prefix being non-null is
   --  when it is 'T' for an implicit type.

   --  Suffix_Index'Image is appended only if the value of Suffix_Index is
   --  positive, or if Suffix_Index is negative 1, then a unique serialized
   --  suffix is added. If Suffix_Index is zero, then no index is appended.

   --  Suffix is also a single upper case letter other than O,Q,U,W,X (T is
   --  allowed in this context), or a string of such upper case letters. In
   --  the case of a string, an initial underscore may be given.
   --
   --  The constructed name is stored using Name_Find so that it can be located
   --  using a subsequent Name_Find operation (i.e. it is properly hashed into
   --  the names table). The upper case letter given as the Suffix argument
   --  ensures that the name does not clash with any Ada identifier name. These
   --  generated names are permitted, but not required, to be made public by
   --  setting the flag Is_Public in the associated entity.
   --
   --  Note: it is dubious to make them public if they have serial numbers,
   --  since we are counting on the serial numbers being the same for the
   --  clients with'ing a package and the actual compilation of the package
   --  with full expansion. This is a dubious assumption ???

   function New_External_Name
     (Suffix       : Character;
      Suffix_Index : Nat) return Name_Id;
   --  Builds a new entry in the names table of the form
   --    Suffix & Suffix_Index'Image
   --  where Suffix is a single upper case letter other than O,Q,U,W,X and is
   --  a required parameter (T is permitted). The constructed name is stored
   --  using Name_Find so that it can be located using a subsequent Name_Find
   --  operation (i.e. it is properly hashed into the names table). The upper
   --  case letter given as the Suffix argument ensures that the name does
   --  not clash with any Ada identifier name. These generated names are
   --  permitted, but not required, to be made public by setting the flag
   --  Is_Public in the associated entity.
   --
   --  Note: it is dubious to make these public since they have serial numbers,
   --  which means we are counting on the serial numbers being the same for the
   --  clients with'ing a package and the actual compilation of the package
   --  with full expansion. This is a dubious assumption ???

   function New_Internal_Name (Id_Char : Character) return Name_Id;
   --  Id_Char is an upper case letter other than O,Q,U,W (which are reserved
   --  for identifier encoding (see Namet package for details) and X which is
   --  used for debug encoding (see Exp_Dbug). The letter T is permitted, but
   --  is reserved by convention for the case of internally generated types.
   --  The result of the call is a new generated unique name of the form XyyyU
   --  where X is Id_Char, yyy is a unique serial number, and U is either a
   --  lower case s or b indicating if the current unit is a spec or a body.
   --
   --  The name is entered into the names table using Name_Enter rather than
   --  Name_Find, because there can never be a need to locate the entry using
   --  the Name_Find procedure later on. Names created by New_Internal_Name
   --  are guaranteed to be consistent from one compilation to another (i.e.
   --  if the identical unit is compiled with a semantically consistent set
   --  of sources, the numbers will be consistent). This means that it is fine
   --  to use these as public symbols.
   --
   --  Note: Nearly all uses of this function are via calls to Make_Temporary,
   --  but there are just a few cases where it is called directly.
   --
   --  Note: despite the guarantee of consistency stated above, it is dubious
   --  to make these public since they have serial numbers, which means we are
   --  counting on the serial numbers being the same for the clients with'ing
   --  a package and the actual compilation of the package with full expansion.
   --  This is a dubious assumption ???

   function New_Occurrence_Of
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr) return Node_Id;
   --  New_Occurrence_Of creates an N_Identifier node that is an occurrence of
   --  the defining identifier Def_Id. The Entity of the result is Def_Id. The
   --  Etype of the result is Def_Id for types, and Etype (Def_Id) otherwise.
   --  Is_Static_Expression is set if this call creates an occurrence of an
   --  enumeration literal.

   function New_Suffixed_Name
     (Related_Id : Name_Id;
      Suffix     : String) return Name_Id;
   --  This function is used to create special suffixed names used by the
   --  debugger. Suffix is a string of upper case letters, used to construct
   --  the required name. For instance, the special type used to record the
   --  fixed-point small is called typ_SMALL where typ is the name of the
   --  fixed-point type (as passed in Related_Id), and Suffix is "SMALL".

   function Sel_Comp (Pre, Sel : String; Loc : Source_Ptr) return Node_Id;
   function Sel_Comp (Pre : Node_Id; Sel : String) return Node_Id;
   --  Create a selected component of the form Pre.Sel; that is, Pre is the
   --  prefix, and Sel is the selector name.

   function OK_Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id;
   --  Like Convert_To, except that a conversion node is always generated, and
   --  the Conversion_OK flag is set on this conversion node.

   function Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id) return Node_Id;
   --  Like Convert_To, but if a conversion is actually needed, constructs an
   --  N_Unchecked_Type_Conversion node to do the required conversion. Unlike
   --  Convert_To, a new node is not required if Expr is already of the correct
   --  BASE type, and if a new node is created, the Parent of Expr is copied to
   --  it.

   -------------------------------------
   -- Subprograms for Use by Gnat1drv --
   -------------------------------------

   function  Make_Id (Str : Text_Buffer) return Node_Id;
   function  Make_SC (Pre, Sel : Node_Id) return Node_Id;
   procedure Set_NOD (Unit : Node_Id);
   procedure Set_NSA (Asp  : Name_Id; OK : out Boolean);
   procedure Set_NUA (Attr : Name_Id; OK : out Boolean);
   procedure Set_NUP (Prag : Name_Id; OK : out Boolean);
   --  Subprograms for call to Get_Target_Parameters in Gnat1drv, see spec
   --  of package Targparm for full description of these four subprograms.
   --  These have to be declared at the top level of a package (accessibility
   --  issues), and Gnat1drv is a procedure, so they can't go there.

end Tbuild;
