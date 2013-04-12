------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              R E P I N F O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2013, Free Software Foundation, Inc.         --
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

--  This package contains the routines to handle back annotation of the
--  tree to fill in representation information, and also the routine used
--  by -gnatR to print this information. This unit is used both in the
--  compiler and in ASIS (it is used in ASIS as part of the implementation
--  of the data decomposition annex).

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

   --  For composite types, there are three cases:

   --    1. In some cases the front end knows the values statically,
   --       for example in the case where representation clauses or
   --       pragmas specify the values.

   --    2. If Backend_Layout is True, then the backend is responsible
   --       for layout of all types and objects not laid out by the
   --       front end. This includes all dynamic values, and also
   --       static values (e.g. record sizes) when not set by the
   --       front end.

   --    3. If Backend_Layout is False, then the front end lays out
   --       all data, according to target dependent size and alignment
   --       information, creating dynamic inlinable functions where
   --       needed in the case of sizes not known till runtime.

   -----------------------------
   -- Back-Annotation by Gigi --
   -----------------------------

   --  The following interface is used by gigi if Backend_Layout is True

   --  As part of the processing in gigi, the types are laid out and
   --  appropriate values computed for the sizes and component positions
   --  and sizes of records and arrays.

   --  The back-annotation circuit in gigi is responsible for updating the
   --  relevant fields in the tree to reflect these computations, as follows:

   --    For E_Array_Type entities, the Component_Size field

   --    For all record and array types and subtypes, the Esize field,
   --    which contains the Size (more accurately the Object_SIze) value
   --    for the type or subtype.

   --    For E_Component and E_Discriminant entities, the Esize (size
   --    of component) and Component_Bit_Offset fields. Note that gigi
   --    does not (yet ???) back annotate Normalized_Position/First_Bit.

   --  There are three cases to consider:

   --    1. The value is constant. In this case, the back annotation works
   --       by simply storing the non-negative universal integer value in
   --       the appropriate field corresponding to this constant size.

   --    2. The value depends on variables other than discriminants of the
   --       current record. In this case, the value is not known, even if
   --       the complete data of the record is available, and gigi marks
   --       this situation by storing the special value No_Uint.

   --    3. The value depends on the discriminant values for the current
   --       record. In this case, gigi back annotates the field with a
   --       representation of the expression for computing the value in
   --       terms of the discriminants. A negative Uint value is used to
   --       represent the value of such an expression, as explained in
   --       the following section.

   --  GCC expressions are represented with a Uint value that is negative.
   --  See the body of this package for details on the representation used.

   --  One other case in which gigi back annotates GCC expressions is in
   --  the Present_Expr field of an N_Variant node. This expression which
   --  will always depend on discriminants, and hence always be represented
   --  as a negative Uint value, provides an expression which, when evaluated
   --  with a given set of discriminant values, indicates whether the variant
   --  is present for that set of values (result is True, i.e. non-zero) or
   --  not present (result is False, i.e. zero).

   subtype Node_Ref is Uint;
   --  Subtype used for negative Uint values used to represent nodes

   subtype Node_Ref_Or_Val is Uint;
   --  Subtype used for values that can either be a Node_Ref (negative)
   --  or a value (non-negative)

   type TCode is range 0 .. 28;
   --  Type used on Ada side to represent DEFTREECODE values defined in
   --  tree.def. Only a subset of these tree codes can actually appear.
   --  The names are the names from tree.def in Ada casing.

   --  name                             code   description           operands

   Cond_Expr        : constant TCode :=  1; -- conditional              3
   Plus_Expr        : constant TCode :=  2; -- addition                 2
   Minus_Expr       : constant TCode :=  3; -- subtraction              2
   Mult_Expr        : constant TCode :=  4; -- multiplication           2
   Trunc_Div_Expr   : constant TCode :=  5; -- truncating division      2
   Ceil_Div_Expr    : constant TCode :=  6; -- division rounding up     2
   Floor_Div_Expr   : constant TCode :=  7; -- division rounding down   2
   Trunc_Mod_Expr   : constant TCode :=  8; -- mod for trunc_div        2
   Ceil_Mod_Expr    : constant TCode :=  9; -- mod for ceil_div         2
   Floor_Mod_Expr   : constant TCode := 10; -- mod for floor_div        2
   Exact_Div_Expr   : constant TCode := 11; -- exact div                2
   Negate_Expr      : constant TCode := 12; -- negation                 1
   Min_Expr         : constant TCode := 13; -- minimum                  2
   Max_Expr         : constant TCode := 14; -- maximum                  2
   Abs_Expr         : constant TCode := 15; -- absolute value           1
   Truth_Andif_Expr : constant TCode := 16; -- Boolean and then         2
   Truth_Orif_Expr  : constant TCode := 17; -- Boolean or else          2
   Truth_And_Expr   : constant TCode := 18; -- Boolean and              2
   Truth_Or_Expr    : constant TCode := 19; -- Boolean or               2
   Truth_Xor_Expr   : constant TCode := 20; -- Boolean xor              2
   Truth_Not_Expr   : constant TCode := 21; -- Boolean not              1
   Lt_Expr          : constant TCode := 22; -- comparison <             2
   Le_Expr          : constant TCode := 23; -- comparison <=            2
   Gt_Expr          : constant TCode := 24; -- comparison >             2
   Ge_Expr          : constant TCode := 25; -- comparison >=            2
   Eq_Expr          : constant TCode := 26; -- comparison =             2
   Ne_Expr          : constant TCode := 27; -- comparison /=            2
   Bit_And_Expr     : constant TCode := 28; -- Binary and               2

   --  The following entry is used to represent a discriminant value in
   --  the tree. It has a special tree code that does not correspond
   --  directly to a gcc node. The single operand is the number of the
   --  discriminant in the record (1 = first discriminant).

   Discrim_Val : constant TCode := 0;  -- discriminant value       1

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

   --  If Backend_Layout is False, then the front-end deals with all
   --  dynamic size and offset fields. There are two cases:

   --    1. The value can be computed at the time of type freezing, and
   --       is stored in a run-time constant. In this case, the field
   --       contains a reference to this entity. In the case of sizes
   --       the value stored is the size in storage units, since dynamic
   --       sizes are always a multiple of storage units.

   --    2. The size/offset depends on the value of discriminants at
   --       run-time. In this case, the front end builds a function to
   --       compute the value. This function has a single parameter
   --       which is the discriminated record object in question. Any
   --       references to discriminant values are simply references to
   --       the appropriate discriminant in this single argument, and
   --       to compute the required size/offset value at run time, the
   --       code generator simply constructs a call to the function
   --       with the appropriate argument. The size/offset field in
   --       this case contains a reference to the function entity.
   --       Note that as for case 1, if such a function is used to
   --       return a size, then the size in storage units is returned,
   --       not the size in bits.

   --  The interface here allows these created entities to be referenced
   --  using negative Unit values, so that they can be stored in the
   --  appropriate size and offset fields in the tree.

   --  In the case of components, if the location of the component is static,
   --  then all four fields (Component_Bit_Offset, Normalized_Position, Esize,
   --  and Normalized_First_Bit) are set to appropriate values. In the case of
   --  a non-static component location, Component_Bit_Offset is not used and
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

   --------------------
   -- ASIS_Interface --
   --------------------

   type Discrim_List is array (Pos range <>) of Uint;
   --  Type used to represent list of discriminant values

   function Rep_Value
     (Val : Node_Ref_Or_Val;
      D   : Discrim_List) return Uint;
   --  Given the contents of a First_Bit_Position or Esize field containing
   --  a node reference (i.e. a negative Uint value) and D, the list of
   --  discriminant values, returns the interpreted value of this field.
   --  For convenience, Rep_Value will take a non-negative Uint value
   --  as an argument value, and return it unmodified. A No_Uint value is
   --  also returned unmodified.

   procedure Tree_Read;
   --  Initializes internal tables from current tree file using the relevant
   --  Table.Tree_Read routines.

   ------------------------
   -- Compiler Interface --
   ------------------------

   procedure List_Rep_Info (Bytes_Big_Endian : Boolean);
   --  Procedure to list representation information. Bytes_Big_Endian is the
   --  value from Ttypes (Repinfo cannot have a dependency on Ttypes).

   procedure Tree_Write;
   --  Writes out internal tables to current tree file using the relevant
   --  Table.Tree_Write routines.

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
