------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G E N _ I L . G E N                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2020-2025, Free Software Foundation, Inc.         --
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

--  "Language design is library design and library design is language design".
--    -- Bjarne Stroustrup

--  This package provides a "little language" for defining type hierarchies,
--  which we call "Gen_IL.Gen". In particular, it is used to describe the type
--  hierarchies rooted at Node_Id and Entity_Id in the intermediate language
--  used by GNAT.

--  The type hierarchy is a strict hierarchy (treeish, no multiple
--  inheritance). We have "abstract" and "concrete" types. Each type has a
--  "parent", except for the root type (Node_Id or Entity_Id). All leaf types
--  in the hierarchy are concrete; all nonleaf types (including the two root
--  types) are abstract. One can create instances of concrete, but not
--  abstract, types.
--
--  Descendants of Node_Id/Node_Kind are node types, and descendants of
--  Entity_Id/Entity_Kind are entity types.
--
--  Types have "fields". Each type inherits all the fields from its parent, and
--  may add new ones. A node field can be marked "syntactic"; entity fields are
--  never syntactic. A nonsyntactic field is "semantic".
--
--  If a field is syntactic, then the constructors in Nmake take a parameter to
--  initialize that field. In addition, the tree-traversal routines in Atree
--  (Traverse_Func and Traverse_Proc) traverse syntactic fields that are of
--  type Node_Id (or subtypes of Node_Id) or List_Id. Finally, (with some
--  exceptions documented in the body) the setter for a syntactic node or list
--  field "Set_F (N, Val)" will set the Parent of Val to N, unless Val is Empty
--  or Error[_List].
--
--  Note that the same field can be syntactic in some node types but semantic
--  in other node types. This is an added complexity that we might want to
--  eliminate someday. We shouldn't add any new such cases.
--
--  A "program" written in the Gen_IL.Gen language consists of calls to the
--  "Create_..." routines below, followed by a call to Compile, also below. In
--  order to understand what's going on, you need to look not only at the
--  Gen_IL.Gen "code", but at the output of the compiler -- at least, look at
--  the specs of Sinfo.Nodes and Einfo.Entities, because GNAT invokes those
--  directly. It's not like a normal language where you don't usually have to
--  look at the generated machine code.
--
--  Thus, the Gen_IL.Gen code is really Ada code, and when you run it as an Ada
--  program, it generates the above-mentioned files. The program is somewhat
--  unusual in that it has no input. Everything it needs to generate code is
--  embodied in it.

--  Why don't we just use a variant record, instead of inventing a wheel?
--  Or a hierarchy of tagged types?
--
--  The key feature that Ada's variant records and tagged types lack, and that
--  this little language has, is that if two types have a field with the same
--  name, then those are the same field, even though they weren't inherited
--  from a common ancestor. Such fields are required to have the same type, the
--  same default value, and the same extra precondition.

with Gen_IL.Types;  use Gen_IL.Types;
pragma Warnings (Off);
with Gen_IL.Fields; use Gen_IL.Fields; -- for children
pragma Warnings (On);
with Gen_IL.Internals;  use Gen_IL.Internals;
use Gen_IL.Internals.Type_Vectors;
use Gen_IL.Internals.Field_Vectors;

package Gen_IL.Gen is

   procedure Create_Root_Node_Type
     (T : Abstract_Node;
      Fields : Field_Sequence := No_Fields)
      with Pre => T = Node_Kind;
   --  Create the root node type (Node_Kind), which is an abstract type

   procedure Create_Abstract_Node_Type
     (T : Abstract_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields);
   --  Create an abstract node type (other than the root node type)

   procedure Create_Concrete_Node_Type
     (T : Concrete_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields;
      Nmake_Assert : String := "");
   --  Create a concrete node type. Every node is an instance of a concrete
   --  node type. Nmake_Assert is an assertion to put in the Make_... function
   --  in the generated Nmake package. It should be a String that represents a
   --  Boolean expression.

   procedure Create_Root_Entity_Type
     (T : Abstract_Entity;
      Fields : Field_Sequence := No_Fields)
      with Pre => T = Entity_Kind;
   --  Create the root entity type (Entity_Kind), which is an abstract type

   procedure Create_Abstract_Entity_Type
     (T : Abstract_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields);
   --  Create an abstract entity type (other than the root entity type)

   procedure Create_Concrete_Entity_Type
     (T : Concrete_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields);
   --  Create a concrete entity type. Every entity is an instance of a concrete
   --  entity type.

   function Create_Syntactic_Field
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc;
   --  Create a syntactic field of a node type. Entities do not have syntactic
   --  fields.

   function Create_Semantic_Field
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc;
   --  Create a semantic field of a node or entity type

   --  Create_Syntactic_Field is used for syntactic fields of nodes. The order
   --  of calls to Create_Syntactic_Field determines the order of the formal
   --  parameters of the Make_... functions in Nmake.
   --
   --  Create_Semantic_Field is used for semantic fields of nodes, and all
   --  fields of entities are considered semantic. The order of calls doesn't
   --  make any difference.
   --
   --  Field_Type is the type of the field. Default_Value is the default value
   --  for the parameter of the Make_... function in Nmake; this is effective
   --  only for syntactic fields. Flag fields of syntactic nodes always have a
   --  default value, which is False unless specified as Default_True. Pre is
   --  an additional precondition for the field getter and setter, in addition
   --  to the precondition that asserts that the type has that field. It should
   --  be a String that represents a Boolean expression. Pre_Get and Pre_Set
   --  are similar to Pre, but for the getter or setter only, respectively.
   --
   --  If multiple calls to these occur for the same Field but different types,
   --  the Field_Type, Pre, Pre_Get, and Pre_Set must match. Default_Value
   --  should match for syntactic fields. See the declaration of Type_Only_Enum
   --  for Type_Only.
   --
   --  (The matching Default_Value requirement is a simplification from the
   --  earlier hand-written version.)

   --  When adding new node or entity kinds, or adding new fields, all back
   --  ends must be made aware of the changes. In addition, the documentation
   --  in Sinfo or Einfo needs to be updated.

   --  To add a new node or entity type, add it to the enumeration type in
   --  Gen_IL.Types, taking care that it is in the approprate range
   --  (Abstract_Node, Abstract_Entity, Concrete_Node, or Concrete_Entity).
   --  Then add a call to one of the above type-creation procedures to
   --  Gen_IL.Gen.Gen_Nodes or Gen_IL.Gen.Gen_Entities.
   --
   --  To add a new field to a type, add it to the enumeration type in
   --  Gen_IL.Fields in the appropriate range. Then add a call to one of
   --  the above field-creation procedures to Gen_IL.Gen.Gen_Nodes or
   --  Gen_IL.Gen.Gen_Entities.
   --
   --  If a type or field name does not follow the usual Mixed_Case convention,
   --  such as "SPARK_Pragma", then you have to add a special case to one of
   --  the Image functions in Gen_IL.Internals and in Treepr.

   --  Forward references are not allowed. So if you say:
   --
   --     Create..._Type (..., Parent => P);
   --
   --  then Create..._Type must have already been called to create P.
   --
   --  Likewise, if you say:
   --
   --     Create..._Field (T, F, Field_Type, ...);
   --
   --  then Create..._Type must have already been called to create T and
   --  (if it's a node or entity type) to create Field_Type.
   --
   --  To delete a node or entity type, delete it from Gen_IL.Types, update the
   --  subranges in Gen_IL.Internals if necessary, and delete all occurrences
   --  from Gen_IL.Gen.Gen_Entities. To delete a field, delete it from
   --  Gen_IL.Fields, and delete all occurrences from Gen_IL.Gen.Gen_Entities.

   --  If a field is not set, it is initialized by default to whatever value is
   --  represented by all-zero bits, with some exceptions. This means Flags are
   --  initialized to False, Node_Ids and List_Ids are initialized to Empty,
   --  and enumeration fields are initialized to 'First of the type (assuming
   --  there is no representation clause).
   --
   --  Elists default to No_Elist.
   --
   --  Fields of type Uint (but not its subtypes) are initialized to No_Uint.
   --  Fields of subtypes Valid_Uint, Unat, Upos, Nonzero_Uint, and Ureal have
   --  no default; it is an error to call a getter before calling the setter.
   --  Likewise, other types whose range does not include zero have no default
   --  (see package Types for the ranges).
   --
   --  If a node is created by a function in Nmake, then the defaults are
   --  different from what is specified above. The parameters of Make_...
   --  functions can have defaults specified; see Create_Syntactic_Field.

   procedure Create_Node_Union_Type
     (T : Abstract_Node; Children : Type_Array);
   procedure Create_Entity_Union_Type
     (T : Abstract_Entity; Children : Type_Array);
   --  Create a "union" type that is the union of the Children. This is used
   --  for nonhierachical types. This is the opposite of the normal "object
   --  oriented" routines above, which create child types based on existing
   --  parents. Here we are creating parent types based on existing child
   --  types. A union type is considered to be an abstract type because it has
   --  multiple children. We do not allow union types to have their own fields,
   --  because that would introduce the well-known complexity of multiple
   --  inheritance. That restriction could be relaxed, but for now, union types
   --  are mainly for allowing things like "Pre => X in Some_Union_Type".

   Illegal : exception;
   --  Exception raised when Gen_IL code (in particular in Gen_Nodes and
   --  Gen_Entities) is illegal. We don't try elaborate error recovery, but
   --  hopefully the exception message will indicate what's wrong. You might
   --  have to go in the debugger to see which line it's complaining about.

   procedure Compile;

private

   function Sy
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Sequence;
   function Sm
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Sequence;
   --  The above functions return Field_Sequence. This is a trick to get around
   --  the fact that Ada doesn't allow singleton positional aggregates. It
   --  allows us to write things like:
   --
   --     Cc (N_Empty, Node_Kind,
   --         (Sy (Chars, Name_Id, Default_No_Name)));
   --
   --  where that thing pretending to be an aggregate is really a parenthesized
   --  expression. See Gen_Nodes for documentation of the functions these are
   --  standing in for.

end Gen_IL.Gen;
