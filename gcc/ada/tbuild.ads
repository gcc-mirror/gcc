------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T B U I L D                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002, Free Software Foundation, Inc.         --
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

--  This package contains various utility procedures to assist in
--  building specific types of tree nodes.

with Types; use Types;

package Tbuild is

   function Checks_Off (N : Node_Id) return Node_Id;
   pragma Inline (Checks_Off);
   --  Returns an N_Unchecked_Expression node whose expression is the given
   --  argument. The results is a subexpression identical to the argument,
   --  except that it will be analyzed and resolved with checks off.

   function Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id;
   --  Returns an expression that represents the result of a checked convert
   --  of expression Exp to type T. If the base type of Exp is T, then no
   --  conversion is required, and Exp is returned unchanged. Otherwise an
   --  N_Type_Conversion node is constructed to convert the expression.
   --  If an N_Type_Conversion node is required, Relocate_Node is used on
   --  Exp. This means that it is safe to replace a node by a Convert_To
   --  of itself to some other type.

   function Make_Byte_Aligned_Attribute_Reference
     (Sloc           : Source_Ptr;
      Prefix         : Node_Id;
      Attribute_Name : Name_Id)
      return           Node_Id;
   pragma Inline (Make_Byte_Aligned_Attribute_Reference);
   --  Like the standard Make_Attribute_Reference but the special flag
   --  Must_Be_Byte_Aligned is set in the attribute reference node. The
   --  Attribute_Name must be Name_Address or Name_Unrestricted_Access.

   function Make_DT_Component
     (Loc  : Source_Ptr;
      Typ  : Entity_Id;
      I    : Positive)
      return Node_Id;
   --  Gives a reference to the Ith component of the Dispatch Table of
   --  a given Tagged Type.
   --
   --  I = 1    --> Inheritance_Depth
   --  I = 2    --> Tags (array of ancestors)
   --  I = 3, 4 --> predefined primitive
   --            function _Size (X : Typ) return Long_Long_Integer;
   --            function _Equality (X : Typ; Y : Typ'Class) return Boolean;
   --  I >= 5   --> User-Defined Primitive Operations

   function Make_DT_Access
     (Loc : Source_Ptr; Rec : Node_Id; Typ : Entity_Id) return Node_Id;
   --  Create an access to the Dispatch Table by using the Tag field
   --  of a tagged record : Acc_Dt (Rec.tag).all

   function Make_Implicit_If_Statement
     (Node            : Node_Id;
      Condition       : Node_Id;
      Then_Statements : List_Id;
      Elsif_Parts     : List_Id := No_List;
      Else_Statements : List_Id := No_List)
      return            Node_Id;
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
      Label_Construct     : Node_Id)
      return                Node_Id;
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
      End_Label              : Node_Id := Empty)
      return                   Node_Id;
   --  This function makes an N_Loop_Statement node whose fields are filled
   --  in with the indicated values (see Sinfo), and whose Sloc field is
   --  is set to Sloc (Node). The effect is identical to calling function
   --  Nmake.Make_Loop_Statement except that there is a check for restrictions
   --  No_Implicit_Loops and No_Implicit_Conditionals (the first applying in
   --  all cases, and the second only for while loops), and if one of these
   --  restrictions is being violated, an error message is posted on Node.

   function Make_Integer_Literal
     (Loc    : Source_Ptr;
      Intval : Int)
      return   Node_Id;
   pragma Inline (Make_Integer_Literal);
   --  A convenient form of Make_Integer_Literal taking Int instead of Uint

   function Make_Raise_Constraint_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code)
      return      Node_Id;
   pragma Inline (Make_Raise_Constraint_Error);
   --  A convenient form of Make_Raise_Constraint_Error where the Reason
   --  is given simply as an enumeration value, rather than a Uint code.

   function Make_Raise_Program_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code)
      return      Node_Id;
   pragma Inline (Make_Raise_Program_Error);
   --  A convenient form of Make_Raise_Program_Error where the Reason
   --  is given simply as an enumeration value, rather than a Uint code.

   function Make_Raise_Storage_Error
     (Sloc      : Source_Ptr;
      Condition : Node_Id := Empty;
      Reason    : RT_Exception_Code)
      return      Node_Id;
   pragma Inline (Make_Raise_Storage_Error);
   --  A convenient form of Make_Raise_Storage_Error where the Reason
   --  is given simply as an enumeration value, rather than a Uint code.

   function Make_Unsuppress_Block
     (Loc   : Source_Ptr;
      Check : Name_Id;
      Stmts : List_Id)
      return  Node_Id;
   --  Build a block with a pragma Suppress on 'Check'. Stmts is the
   --  statements list that needs protection against the check

   function New_Constraint_Error (Loc : Source_Ptr) return Node_Id;
   --  This function builds a tree corresponding to the Ada statement
   --  "raise Constraint_Error" and returns the root of this tree,
   --  the N_Raise_Statement node.

   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : Character := ' ';
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ')
      return         Name_Id;
   function New_External_Name
     (Related_Id   : Name_Id;
      Suffix       : String;
      Suffix_Index : Int       := 0;
      Prefix       : Character := ' ')
      return         Name_Id;
   --  Builds a new entry in the names table of the form:
   --
   --    [Prefix  &] Related_Id [& Suffix] [& Suffix_Index]
   --
   --  Prefix is prepended only if Prefix is non-blank (in which case it
   --  must be an upper case letter other than O,Q,U,W (which are used for
   --  identifier encoding, see Namet), and T is reserved for use by implicit
   --  types. and X is reserved for use by debug type encoding (see package
   --  Exp_Dbug). Note: the reason that Prefix is last is that it is almost
   --  always omitted. The notable case of Prefix being non-null is when
   --  it is 'T' for an implicit type.

   --  Suffix_Index'Image is appended only if the value of Suffix_Index is
   --  positive, or if Suffix_Index is negative 1, then a unique serialized
   --  suffix is added. If Suffix_Index is zero, then no index is appended.

   --  Suffix is also a single upper case letter other than O,Q,U,W,X and is a
   --  required parameter (T is permitted). The constructed name is stored
   --  using Find_Name so that it can be located using a subsequent Find_Name
   --  operation (i.e. it is properly hashed into the names table). The upper
   --  case letter given as the Suffix argument ensures that the name does
   --  not clash with any Ada identifier name. These generated names are
   --  permitted, but not required, to be made public by setting the flag
   --  Is_Public in the associated entity.

   function New_External_Name
     (Suffix       : Character;
      Suffix_Index : Nat)
      return         Name_Id;
   --  Builds a new entry in the names table of the form
   --    Suffix & Suffix_Index'Image
   --  where Suffix is a single upper case letter other than O,Q,U,W,X and is
   --  a required parameter (T is permitted). The constructed name is stored
   --  using Find_Name so that it can be located using a subsequent Find_Name
   --  operation (i.e. it is properly hashed into the names table). The upper
   --  case letter given as the Suffix argument ensures that the name does
   --  not clash with any Ada identifier name. These generated names are
   --  permitted, but not required, to be made public by setting the flag
   --  Is_Public in the associated entity.

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
   --  of sources, the numbers will be consistent. This means that it is fine
   --  to use these as public symbols.

   function New_Occurrence_Of
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr)
      return   Node_Id;
   --  New_Occurrence_Of creates an N_Identifier node which is an
   --  occurrence of the defining identifier which is passed as its
   --  argument. The Entity and Etype of the result are set from
   --  the given defining identifier as follows: Entity is simply
   --  a copy of Def_Id. Etype is a copy of Def_Id for types, and
   --  a copy of the Etype of Def_Id for other entities.

   function New_Reference_To
     (Def_Id : Entity_Id;
      Loc    : Source_Ptr)
      return   Node_Id;
   --  This is like New_Occurrence_Of, but it does not set the Etype field.
   --  It is used from the expander, where Etype fields are generally not set,
   --  since they are set when the expanded tree is reanalyzed.

   function New_Suffixed_Name
     (Related_Id : Name_Id;
      Suffix     : String)
      return       Name_Id;
   --  This function is used to create special suffixed names used by the
   --  debugger. Suffix is a string of upper case letters, used to construct
   --  the required name. For instance, the special type used to record the
   --  fixed-point small is called typ_SMALL where typ is the name of the
   --  fixed-point type (as passed in Related_Id), and Suffix is "SMALL".

   function OK_Convert_To (Typ : Entity_Id; Expr : Node_Id) return Node_Id;
   --  Like Convert_To, except that a conversion node is always generated,
   --  and the Conversion_OK flag is set on this conversion node.

   function Unchecked_Convert_To
     (Typ  : Entity_Id;
      Expr : Node_Id)
      return Node_Id;
   --  Like Convert_To, but if a conversion is actually needed, constructs
   --  an N_Unchecked_Type_Conversion node to do the required conversion.

end Tbuild;
