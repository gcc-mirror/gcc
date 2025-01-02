------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R E P I N F O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

--  This package contains the routines to handle back annotation of the
--  tree to fill in representation information, and also the routines used
--  by -gnatR to output this information.

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file repinfo.h

with Types; use Types;
with Uintp; use Uintp;

package Repinfo is

   --------------------------------
   -- Representation Information --
   --------------------------------

   --  The representation information of interest here is size and
   --  component information for arrays and records. For primitive
   --  types, the front end computes the Esize and RM_Size fields of
   --  the corresponding entities as constant non-negative integers,
   --  and the Uint values are stored directly in these fields.

   --  For composite types, there are two cases:

   --    1. In some cases the front end knows the values statically,
   --       for example in the case where representation clauses or
   --       pragmas specify the values.

   --    2. The backend is responsible for layout of all types and objects
   --       not laid out by the front end. This includes all dynamic values,
   --       and also static values (e.g. record sizes) when not set by the
   --       front end.

   -----------------------------
   -- Back Annotation by Gigi --
   -----------------------------

   --  The following interface is used by gigi

   --  As part of the processing in gigi, the types are laid out and
   --  appropriate values computed for the sizes and component positions
   --  and sizes of records and arrays.

   --  The back-annotation circuit in gigi is responsible for updating the
   --  relevant fields in the tree to reflect these computations, as follows:

   --    For E_Array_Type entities, the Component_Size field

   --    For all record and array types and subtypes, the Esize and RM_Size
   --    fields, which respectively contain the Object_Size and Value_Size
   --    values for the type or subtype.

   --    For E_Component and E_Discriminant entities, the Esize (size
   --    of component) and Component_Bit_Offset fields. Note that gigi
   --    does not generally back annotate Normalized_Position/First_Bit.

   --  There are three cases to consider:

   --    1. The value is constant. In this case, the back annotation works
   --       by simply storing the non-negative universal integer value in
   --       the appropriate field corresponding to this constant size.

   --    2. The value depends on the discriminant values for the current
   --       record. In this case, gigi back annotates the field with a
   --       representation of the expression for computing the value in
   --       terms of the discriminants. A negative Uint value is used to
   --       represent the value of such an expression, as explained in
   --       the following section.

   --    3. The value depends on variables other than discriminants of the
   --       current record. In this case, gigi also back annotates the field
   --       with a representation of the expression for computing the value
   --       in terms of the variables represented symbolically.

   --  Note: the extended back annotation for the dynamic case is needed only
   --  for -gnatR3 output. Since it can be expensive to do this back annotation
   --  (for discriminated records with many variable-length arrays), we only do
   --  the full back annotation in -gnatR3 mode. In any other mode, the
   --  back-end just sets the value to Uint_Minus_1, indicating that the value
   --  of the attribute depends on discriminant information, but not giving
   --  further details.

   --  GCC expressions are represented with a Uint value that is negative.
   --  See the body of this package for details on the representation used.

   --  One other case in which gigi back annotates GCC expressions is in
   --  the Present_Expr field of an N_Variant node. This expression which
   --  will always depend on discriminants, and hence always be represented
   --  as a negative Uint value, provides an expression which, when evaluated
   --  with a given set of discriminant values, indicates whether the variant
   --  is present for that set of values (result is True, i.e. non-zero) or
   --  not present (result is False, i.e. zero). Again, the full annotation of
   --  this field is done only in -gnatR3 mode, and in other modes, the value
   --  is set to Uint_Minus_1.

   subtype Node_Ref is Unegative;
   --  Subtype used for negative Uint values used to represent nodes

   subtype Node_Ref_Or_Val is Uint;
   --  Subtype used for values that can be a Node_Ref (negative) or a value
   --  (non-negative) or No_Uint.

   type TCode is range 0 .. 27;
   --  Type used on Ada side to represent DEFTREECODE values defined in
   --  tree.def. Only a subset of these tree codes can actually appear.
   --  The names are the names from tree.def in Ada casing.

   --  name                             code   description     operands  symbol

   Cond_Expr        : constant TCode :=  1; -- conditional          3      ?<>
   Plus_Expr        : constant TCode :=  2; -- addition             2        +
   Minus_Expr       : constant TCode :=  3; -- subtraction          2        -
   Mult_Expr        : constant TCode :=  4; -- multiplication       2        *
   Trunc_Div_Expr   : constant TCode :=  5; -- truncating div       2       /t
   Ceil_Div_Expr    : constant TCode :=  6; -- div rounding up      2       /c
   Floor_Div_Expr   : constant TCode :=  7; -- div rounding down    2       /f
   Trunc_Mod_Expr   : constant TCode :=  8; -- mod for trunc_div    2     modt
   Ceil_Mod_Expr    : constant TCode :=  9; -- mod for ceil_div     2     modc
   Floor_Mod_Expr   : constant TCode := 10; -- mod for floor_div    2     modf
   Exact_Div_Expr   : constant TCode := 11; -- exact div            2       /e
   Negate_Expr      : constant TCode := 12; -- negation             1        -
   Min_Expr         : constant TCode := 13; -- minimum              2      min
   Max_Expr         : constant TCode := 14; -- maximum              2      max
   Abs_Expr         : constant TCode := 15; -- absolute value       1      abs
   Truth_And_Expr   : constant TCode := 16; -- boolean and          2      and
   Truth_Or_Expr    : constant TCode := 17; -- boolean or           2       or
   Truth_Xor_Expr   : constant TCode := 18; -- boolean xor          2      xor
   Truth_Not_Expr   : constant TCode := 19; -- boolean not          1      not
   Lt_Expr          : constant TCode := 20; -- comparison <         2        <
   Le_Expr          : constant TCode := 21; -- comparison <=        2       <=
   Gt_Expr          : constant TCode := 22; -- comparison >         2        >
   Ge_Expr          : constant TCode := 23; -- comparison >=        2       >=
   Eq_Expr          : constant TCode := 24; -- comparison =         2       ==
   Ne_Expr          : constant TCode := 25; -- comparison /=        2       !=
   Bit_And_Expr     : constant TCode := 26; -- bitwise and          2        &

   --  The following entry is used to represent a discriminant value in
   --  the tree. It has a special tree code that does not correspond
   --  directly to a GCC node. The single operand is the index number
   --  of the discriminant in the record (1 = first discriminant).

   Discrim_Val      : constant TCode :=  0;  -- discriminant value  1        #

   --  The following entry is used to represent a value not known at
   --  compile time in the tree, other than a discriminant value. It
   --  has a special tree code that does not correspond directly to
   --  a GCC node. The single operand is an arbitrary index number.

   Dynamic_Val      : constant TCode := 27;  -- dynamic value       1      var

   ----------------------------
   -- The JSON output format --
   ----------------------------

   --  The representation information can be output to a file in the JSON
   --  data interchange format specified by the ECMA-404 standard. In the
   --  following description, the terminology is that of the JSON syntax
   --  from the ECMA document and of the JSON grammar from www.json.org.

   --  The output is an array of entities

   --  An entity is an object whose members are pairs taken from:

   --    "name"                 :  string
   --    "location"             :  string
   --    "record"               :  array of components
   --    "[parent_]*variant"    :  array of variants
   --    "formal"               :  array of formal parameters
   --    "mechanism"            :  string
   --    "Size"                 :  numerical expression
   --    "Object_Size"          :  numerical expression
   --    "Value_Size"           :  numerical expression
   --    "Component_Size"       :  numerical expression
   --    "Range"                :  array of numerical expressions
   --    "Small"                :  numerical expression
   --    "Alignment"            :  number
   --    "Convention"           :  string
   --    "Linker_Section"       :  string
   --    "Bit_Order"            :  string
   --    "Scalar_Storage_Order" :  string

   --    "name" and "location" are present for every entity and come from the
   --    declaration of the associated Ada entity. The value of "name" is the
   --    fully qualified Ada name. The value of "location" is the expanded
   --    chain of instantiation locations that contains the entity.
   --    "record" is present for every record type and its value is the list of
   --    components. "[parent_]*variant" is present only if the record type, or
   --    one of its ancestors (parent, grand-parent, etc) if it's an extension,
   --    has a variant part and its value is the list of variants.
   --    "formal" is present for every subprogram and entry, and its value is
   --    the list of formal parameters. "mechanism" is present for functions
   --    only and its value is the return mechanim.
   --    The other pairs may be present when the eponymous aspect/attribute is
   --    defined for the Ada entity, and their value is set by the language.

   --  A component is an object whose members are pairs taken from:

   --    "name"                 :  string
   --    "discriminant"         :  number
   --    "Position"             :  numerical expression
   --    "First_Bit"            :  number
   --    "Size"                 :  numerical expression

   --    "name" is present for every component and comes from the declaration
   --    of the type; its value is the unqualified Ada name. "discriminant" is
   --    present only if the component is a discriminant, and its value is the
   --    ranking of the discriminant in the list of discriminants of the type,
   --    i.e. an integer index ranging from 1 to the number of discriminants.
   --    The other three pairs are present for every component and come from
   --    the layout of the type; their value is the value of the eponymous
   --    attribute set by the language.

   --  A variant is an object whose members are pairs taken from:

   --    "present"              :  numerical expression
   --    "record"               :  array of components
   --    "variant"              :  array of variants

   --    "present" and "record" are present for every variant. The value of
   --    "present" is a boolean expression that evaluates to true when the
   --    components of the variant are contained in the record type and to
   --    false when they are not, with the exception that a value of 1 means
   --    that the components of the variant are contained in the record type
   --    only when the "present" member of all the preceding variants in the
   --    variant list evaluates to false. The value of "record" is the list of
   --    components in the variant. "variant" is present only if the variant
   --    itself has a variant part and its value is the list of (sub)variants.

   --  A formal parameter is an object whose members are pairs taken from:

   --    "name"                 :  string
   --    "mechanism"            :  string

   --    The two pairs are present for every formal parameter. "name" comes
   --    from the declaration of the parameter in the subprogram or entry
   --    and its value is the unqualified Ada name. The value of "mechanism"
   --    is the passing mechanism for the parameter set by the language.

   --  A numerical expression is either a number or an object whose members
   --  are pairs taken from:

   --    "code"                 :  string
   --    "operands"             :  array of numerical expressions

   --    The two pairs are present for every such object. The value of "code"
   --    is a symbol taken from the table defining the TCode type above. The
   --    number of elements of the value of "operands" is specified by the
   --    operands column in the line associated with the symbol in the table.

   --    As documented above, the full back annotation is only done in -gnatR3.
   --    In the other cases, if the numerical expression is not a number, then
   --    it is replaced with the "??" string.

   ------------------------
   -- The gigi Interface --
   ------------------------

   --  The following declarations are for use by gigi for back annotation

   function Create_Node
     (Expr : TCode;
      Op1  : Node_Ref_Or_Val;
      Op2  : Node_Ref_Or_Val := No_Uint;
      Op3  : Node_Ref_Or_Val := No_Uint) return Node_Ref;
   --  Creates a node using the tree code defined by Expr and from one to three
   --  operands as required (unused operands set as shown to No_Uint) Note that
   --  this call can be used to create a discriminant reference by using (Expr
   --  => Discrim_Val, Op1 => discriminant_number).

   function Create_Discrim_Ref (Discr : Entity_Id) return Node_Ref;
   --  Creates a reference to the discriminant whose entity is Discr

   --------------------------------------------------------
   -- Front-End Interface for Dynamic Size/Offset Values --
   --------------------------------------------------------

   --  This interface is used by GNAT LLVM to deal with all dynamic size and
   --  offset fields.

   --  The interface here allows these created entities to be referenced
   --  using negative Unit values, so that they can be stored in the
   --  appropriate size and offset fields in the tree.

   --  In the case of components, if the location of the component is static,
   --  then all four fields (Component_Bit_Offset, Normalized_Position, Esize,
   --  and Normalized_First_Bit) are set to appropriate values. In the case of
   --  a nonstatic component location, Component_Bit_Offset is not used and
   --  is left set to Unknown. Normalized_Position and Normalized_First_Bit
   --  are set appropriately.

   subtype SO_Ref is Uint;
   --  Type used to represent a Uint value that represents a static or
   --  dynamic size/offset value (non-negative if static, negative if
   --  the size value is dynamic).

   subtype Dynamic_SO_Ref is Uint;
   --  Type used to represent a negative Uint value used to store
   --  a dynamic size/offset value.

   function Is_Dynamic_SO_Ref (U : SO_Ref) return Boolean;
   pragma Inline (Is_Dynamic_SO_Ref);
   --  Given a SO_Ref (Uint) value, returns True iff the SO_Ref value
   --  represents a dynamic Size/Offset value (i.e. it is negative).

   function Is_Static_SO_Ref (U : SO_Ref) return Boolean;
   pragma Inline (Is_Static_SO_Ref);
   --  Given a SO_Ref (Uint) value, returns True iff the SO_Ref value
   --  represents a static Size/Offset value (i.e. it is non-negative).

   function Create_Dynamic_SO_Ref (E : Entity_Id) return Dynamic_SO_Ref;
   --  Given the Entity_Id for a constant (case 1), the Node_Id for an
   --  expression (case 2), or the Entity_Id for a function (case 3),
   --  this function returns a (negative) Uint value that can be used
   --  to retrieve the entity or expression for later use.

   function Get_Dynamic_SO_Entity (U : Dynamic_SO_Ref) return Entity_Id;
   --  Retrieve the Node_Id or Entity_Id stored by a previous call to
   --  Create_Dynamic_SO_Ref. The approach is that the front end makes
   --  the necessary Create_Dynamic_SO_Ref calls to associate the node
   --  and entity id values and the back end makes Get_Dynamic_SO_Ref
   --  calls to retrieve them.

   ------------------------------
   -- External tools Interface --
   ------------------------------

   type Discrim_List is array (Pos range <>) of Uint;
   --  Type used to represent list of discriminant values

   function Rep_Value (Val : Node_Ref_Or_Val; D : Discrim_List) return Uint;
   --  Given the contents of a First_Bit_Position or Esize field containing
   --  a node reference (i.e. a negative Uint value) and D, the list of
   --  discriminant values, returns the interpreted value of this field.
   --  For convenience, Rep_Value will take a non-negative Uint value
   --  as an argument value, and return it unmodified. A No_Uint value is
   --  also returned unmodified.

   ------------------------
   -- Compiler Interface --
   ------------------------

   procedure List_Rep_Info (Bytes_Big_Endian : Boolean);
   --  Procedure to list representation information. Bytes_Big_Endian is the
   --  value from Ttypes (Repinfo cannot have a dependency on Ttypes).

   --------------------------
   -- Debugging Procedures --
   --------------------------

   procedure List_GCC_Expression (U : Node_Ref_Or_Val);
   --  Prints out given expression in symbolic form. Constants are listed
   --  in decimal numeric form, Discriminants are listed with a # followed
   --  by the discriminant number, and operators are output in appropriate
   --  symbolic form No_Uint displays as two question marks. The output is
   --  on a single line but has no line return after it. This procedure is
   --  useful only if operating in backend layout mode.

   procedure lgx (U : Node_Ref_Or_Val);
   --  In backend layout mode, this is like List_GCC_Expression, but
   --  includes a line return at the end. If operating in front end
   --  layout mode, then the name of the entity for the size (either
   --  a function of a variable) is listed followed by a line return.

end Repinfo;
