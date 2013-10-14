------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T A N D                                 --
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

--  This package contains the declarations of entities in package Standard,
--  These values are initialized either by calling CStand.Create_Standard,
--  or by calling Stand.Tree_Read.

with Types; use Types;

package Stand is

   type Standard_Entity_Type is (
   --  This enumeration type contains an entry for each name in Standard

      --  Package names

      S_Standard,
      S_ASCII,

      --  Types and subtypes defined in package Standard (in the order in which
      --  they appear in the RM, so that the declarations are in the right
      --  order for the purposes of ASIS traversals

      S_Boolean,

      S_Short_Short_Integer,
      S_Short_Integer,
      S_Integer,
      S_Long_Integer,
      S_Long_Long_Integer,

      S_Natural,
      S_Positive,

      S_Short_Float,
      S_Float,
      S_Long_Float,
      S_Long_Long_Float,

      S_Character,
      S_Wide_Character,
      S_Wide_Wide_Character,

      S_String,
      S_Wide_String,
      S_Wide_Wide_String,

      S_Duration,

      --  Enumeration literals for type Boolean

      S_False,
      S_True,

      --  Exceptions declared in package Standard

      S_Constraint_Error,
      S_Numeric_Error,
      S_Program_Error,
      S_Storage_Error,
      S_Tasking_Error,

      --  Binary Operators declared in package Standard

      S_Op_Add,
      S_Op_And,
      S_Op_Concat,
      S_Op_Concatw,
      S_Op_Concatww,
      S_Op_Divide,
      S_Op_Eq,
      S_Op_Expon,
      S_Op_Ge,
      S_Op_Gt,
      S_Op_Le,
      S_Op_Lt,
      S_Op_Mod,
      S_Op_Multiply,
      S_Op_Ne,
      S_Op_Or,
      S_Op_Rem,
      S_Op_Subtract,
      S_Op_Xor,

      --  Unary operators declared in package Standard

      S_Op_Abs,
      S_Op_Minus,
      S_Op_Not,
      S_Op_Plus,

      --  Constants defined in package ASCII (with value in hex).
      --  First the thirty-two C0 control characters)

      S_NUL,            -- 16#00#
      S_SOH,            -- 16#01#
      S_STX,            -- 16#02#
      S_ETX,            -- 16#03#
      S_EOT,            -- 16#04#
      S_ENQ,            -- 16#05#
      S_ACK,            -- 16#06#
      S_BEL,            -- 16#07#
      S_BS,             -- 16#08#
      S_HT,             -- 16#09#
      S_LF,             -- 16#0A#
      S_VT,             -- 16#0B#
      S_FF,             -- 16#0C#
      S_CR,             -- 16#0D#
      S_SO,             -- 16#0E#
      S_SI,             -- 16#0F#
      S_DLE,            -- 16#10#
      S_DC1,            -- 16#11#
      S_DC2,            -- 16#12#
      S_DC3,            -- 16#13#
      S_DC4,            -- 16#14#
      S_NAK,            -- 16#15#
      S_SYN,            -- 16#16#
      S_ETB,            -- 16#17#
      S_CAN,            -- 16#18#
      S_EM,             -- 16#19#
      S_SUB,            -- 16#1A#
      S_ESC,            -- 16#1B#
      S_FS,             -- 16#1C#
      S_GS,             -- 16#1D#
      S_RS,             -- 16#1E#
      S_US,             -- 16#1F#

      --  Here are the ones for Colonel Whitaker's O26 keypunch!

      S_Exclam,         -- 16#21#
      S_Quotation,      -- 16#22#
      S_Sharp,          -- 16#23#
      S_Dollar,         -- 16#24#
      S_Percent,        -- 16#25#
      S_Ampersand,      -- 16#26#

      S_Colon,          -- 16#3A#
      S_Semicolon,      -- 16#3B#

      S_Query,          -- 16#3F#
      S_At_Sign,        -- 16#40#

      S_L_Bracket,      -- 16#5B#
      S_Back_Slash,     -- 16#5C#
      S_R_Bracket,      -- 16#5D#
      S_Circumflex,     -- 16#5E#
      S_Underline,      -- 16#5F#
      S_Grave,          -- 16#60#

      S_LC_A,           -- 16#61#
      S_LC_B,           -- 16#62#
      S_LC_C,           -- 16#63#
      S_LC_D,           -- 16#64#
      S_LC_E,           -- 16#65#
      S_LC_F,           -- 16#66#
      S_LC_G,           -- 16#67#
      S_LC_H,           -- 16#68#
      S_LC_I,           -- 16#69#
      S_LC_J,           -- 16#6A#
      S_LC_K,           -- 16#6B#
      S_LC_L,           -- 16#6C#
      S_LC_M,           -- 16#6D#
      S_LC_N,           -- 16#6E#
      S_LC_O,           -- 16#6F#
      S_LC_P,           -- 16#70#
      S_LC_Q,           -- 16#71#
      S_LC_R,           -- 16#72#
      S_LC_S,           -- 16#73#
      S_LC_T,           -- 16#74#
      S_LC_U,           -- 16#75#
      S_LC_V,           -- 16#76#
      S_LC_W,           -- 16#77#
      S_LC_X,           -- 16#78#
      S_LC_Y,           -- 16#79#
      S_LC_Z,           -- 16#7A#

      S_L_BRACE,        -- 16#7B#
      S_BAR,            -- 16#7C#
      S_R_BRACE,        -- 16#7D#
      S_TILDE,          -- 16#7E#

      --  And one more control character, all on its own

      S_DEL);           -- 16#7F#

   subtype S_Types is
     Standard_Entity_Type range S_Boolean .. S_Duration;

   subtype S_Exceptions is
     Standard_Entity_Type range S_Constraint_Error .. S_Tasking_Error;

   subtype S_ASCII_Names is
     Standard_Entity_Type range S_NUL .. S_DEL;

   subtype S_Binary_Ops is
      Standard_Entity_Type range S_Op_Add .. S_Op_Xor;

   subtype S_Unary_Ops is
      Standard_Entity_Type range S_Op_Abs .. S_Op_Plus;

   type Standard_Entity_Array_Type is array (Standard_Entity_Type) of Node_Id;

   Standard_Entity : Standard_Entity_Array_Type;
   --  This array contains pointers to the Defining Identifier nodes for each
   --  of the visible entities defined in Standard_Entities_Type. The array is
   --  initialized by the Create_Standard procedure.

   Standard_Package_Node : Node_Id;
   --  Points to the N_Package_Declaration node for standard. Also
   --  initialized by the Create_Standard procedure.

   --  The following Entities are the pointers to the Defining Identifier
   --  nodes for some visible entities defined in Standard_Entities_Type.

   SE : Standard_Entity_Array_Type renames Standard_Entity;

   Standard_Standard            : Entity_Id renames SE (S_Standard);

   Standard_ASCII               : Entity_Id renames SE (S_ASCII);
   Standard_Character           : Entity_Id renames SE (S_Character);
   Standard_Wide_Character      : Entity_Id renames SE (S_Wide_Character);
   Standard_Wide_Wide_Character : Entity_Id renames SE (S_Wide_Wide_Character);
   Standard_String              : Entity_Id renames SE (S_String);
   Standard_Wide_String         : Entity_Id renames SE (S_Wide_String);
   Standard_Wide_Wide_String    : Entity_Id renames SE (S_Wide_Wide_String);

   Standard_Boolean             : Entity_Id renames SE (S_Boolean);
   Standard_False               : Entity_Id renames SE (S_False);
   Standard_True                : Entity_Id renames SE (S_True);

   Standard_Duration            : Entity_Id renames SE (S_Duration);

   Standard_Natural             : Entity_Id renames SE (S_Natural);
   Standard_Positive            : Entity_Id renames SE (S_Positive);

   Standard_Constraint_Error    : Entity_Id renames SE (S_Constraint_Error);
   Standard_Numeric_Error       : Entity_Id renames SE (S_Numeric_Error);
   Standard_Program_Error       : Entity_Id renames SE (S_Program_Error);
   Standard_Storage_Error       : Entity_Id renames SE (S_Storage_Error);
   Standard_Tasking_Error       : Entity_Id renames SE (S_Tasking_Error);

   Standard_Short_Float         : Entity_Id renames SE (S_Short_Float);
   Standard_Float               : Entity_Id renames SE (S_Float);
   Standard_Long_Float          : Entity_Id renames SE (S_Long_Float);
   Standard_Long_Long_Float     : Entity_Id renames SE (S_Long_Long_Float);

   Standard_Short_Short_Integer : Entity_Id renames SE (S_Short_Short_Integer);
   Standard_Short_Integer       : Entity_Id renames SE (S_Short_Integer);
   Standard_Integer             : Entity_Id renames SE (S_Integer);
   Standard_Long_Integer        : Entity_Id renames SE (S_Long_Integer);
   Standard_Long_Long_Integer   : Entity_Id renames SE (S_Long_Long_Integer);

   Standard_Op_Add              : Entity_Id renames SE (S_Op_Add);
   Standard_Op_And              : Entity_Id renames SE (S_Op_And);
   Standard_Op_Concat           : Entity_Id renames SE (S_Op_Concat);
   Standard_Op_Concatw          : Entity_Id renames SE (S_Op_Concatw);
   Standard_Op_Concatww         : Entity_Id renames SE (S_Op_Concatww);
   Standard_Op_Divide           : Entity_Id renames SE (S_Op_Divide);
   Standard_Op_Eq               : Entity_Id renames SE (S_Op_Eq);
   Standard_Op_Expon            : Entity_Id renames SE (S_Op_Expon);
   Standard_Op_Ge               : Entity_Id renames SE (S_Op_Ge);
   Standard_Op_Gt               : Entity_Id renames SE (S_Op_Gt);
   Standard_Op_Le               : Entity_Id renames SE (S_Op_Le);
   Standard_Op_Lt               : Entity_Id renames SE (S_Op_Lt);
   Standard_Op_Mod              : Entity_Id renames SE (S_Op_Mod);
   Standard_Op_Multiply         : Entity_Id renames SE (S_Op_Multiply);
   Standard_Op_Ne               : Entity_Id renames SE (S_Op_Ne);
   Standard_Op_Or               : Entity_Id renames SE (S_Op_Or);
   Standard_Op_Rem              : Entity_Id renames SE (S_Op_Rem);
   Standard_Op_Subtract         : Entity_Id renames SE (S_Op_Subtract);
   Standard_Op_Xor              : Entity_Id renames SE (S_Op_Xor);

   Standard_Op_Abs              : Entity_Id renames SE (S_Op_Abs);
   Standard_Op_Minus            : Entity_Id renames SE (S_Op_Minus);
   Standard_Op_Not              : Entity_Id renames SE (S_Op_Not);
   Standard_Op_Plus             : Entity_Id renames SE (S_Op_Plus);

   Last_Standard_Node_Id : Node_Id;
   --  Highest Node_Id value used by Standard

   Last_Standard_List_Id : List_Id;
   --  Highest List_Id value used by Standard (including those used by
   --  normal list headers, element list headers, and list elements)

   Boolean_Literals : array (Boolean) of Entity_Id;
   --  Entities for the two boolean literals, used by the expander

   -------------------------------------
   -- Semantic Phase Special Entities --
   -------------------------------------

   --  The semantic phase needs a number of entities for internal processing
   --  that are logically at the level of Standard, and hence defined in this
   --  package. However, they are never visible to a program, and are not
   --  chained on to the Decls list of Standard. The names of all these
   --  types are relevant only in certain debugging and error message
   --  situations. They have names that are suitable for use in such
   --  error messages (see body for actual names used).

   Standard_Void_Type  : Entity_Id;
   --  This is a type used to represent the return type of procedures

   Standard_Exception_Type  : Entity_Id;
   --  This is a type used to represent the Etype of exceptions

   Standard_A_String   : Entity_Id;
   --  An access to String type used for building elements of tables
   --  carrying the enumeration literal names.

   Standard_A_Char : Entity_Id;
   --  Access to character, used as a component of the exception type to denote
   --  a thin pointer component.

   Standard_Debug_Renaming_Type : Entity_Id;
   --  A zero-size subtype of Integer, used as the type of variables used to
   --  provide the debugger with name encodings for renaming declarations.

   Predefined_Float_Types : Elist_Id;
   --  Entities for predefined floating point types. These are used by
   --  the semantic phase to select appropriate types for floating point
   --  declarations. This list is ordered by preference. All types up to
   --  Long_Long_Float_Type are considered for plain "digits N" declarations,
   --  while selection of later types requires a range specification and
   --  possibly other attributes or pragmas.

   --  The entities labeled Any_xxx are used in situations where the full
   --  characteristics of an entity are not yet known, e.g. Any_Character
   --  is used to label a character literal before resolution is complete.
   --  These entities are also used to construct appropriate references in
   --  error messages ("expecting an integer type").

   Any_Id : Entity_Id;
   --  Used to represent some unknown identifier. Used to label undefined
   --  identifier references to prevent cascaded errors.

   Any_Type : Entity_Id;
   --  Used to represent some unknown type. Any_Type is the type of an
   --  unresolved operator, and it is the type of a node where a type error
   --  has been detected. Any_Type plays an important role in avoiding cascaded
   --  errors, because it is compatible with all other types, and is propagated
   --  to any expression that has a subexpression of Any_Type. When resolving
   --  operators, Any_Type is the initial type of the node before any of its
   --  candidate interpretations has been examined. If after examining all of
   --  them the type is still Any_Type, the node has no possible interpretation
   --  and an error can be emitted (and Any_Type will be propagated upwards).
   --
   --  There is one situation in which Any_Type is used to legitimately
   --  represent a case where the type is not known pre-resolution, and that
   --  is for the N_Raise_Expression node. In this case, the Etype being set to
   --  Any_Type is normal and does not represent an error. In particular, it is
   --  compatible with the type of any constituent of the enclosing expression,
   --  if any. The type is eventually replaced with the type of the context,
   --  which plays no role in the resolution of the Raise_Expression.

   Any_Access : Entity_Id;
   --  Used to resolve the overloaded literal NULL

   Any_Array : Entity_Id;
   --  Used to represent some unknown array type

   Any_Boolean : Entity_Id;
   --  The context type of conditions in IF and WHILE statements

   Any_Character : Entity_Id;
   --  Any_Character is used to label character literals, which in general
   --  will not have an explicit declaration (this is true of the predefined
   --  character types).

   Any_Composite : Entity_Id;
   --  The type Any_Composite is used for aggregates before type resolution.
   --  It is compatible with any array or non-limited record type.

   Any_Discrete : Entity_Id;
   --  Used to represent some unknown discrete type

   Any_Fixed : Entity_Id;
   --  Used to represent some unknown fixed-point type

   Any_Integer : Entity_Id;
   --  Used to represent some unknown integer type

   Any_Modular : Entity_Id;
   --  Used to represent the result type of a boolean operation on an integer
   --  literal. The result is not Universal_Integer, because it is only legal
   --  in a modular context.

   Any_Numeric : Entity_Id;
   --  Used to represent some unknown numeric type

   Any_Real : Entity_Id;
   --  Used to represent some unknown real type

   Any_Scalar : Entity_Id;
   --  Used to represent some unknown scalar type

   Any_String : Entity_Id;
   --  The type Any_String is used for string literals before type resolution.
   --  It corresponds to array (Positive range <>) of character where the
   --  component type is compatible with any character type, not just
   --  Standard_Character.

   Universal_Integer : Entity_Id;
   --  Entity for universal integer type. The bounds of this type correspond
   --  to the largest supported integer type (i.e. Long_Long_Integer). It is
   --  the type used for runtime calculations in type universal integer.

   Universal_Real : Entity_Id;
   --  Entity for universal real type. The bounds of this type correspond to
   --  to the largest supported real type (i.e. Long_Long_Float). It is the
   --  type used for runtime calculations in type universal real. Note that
   --  this type is always IEEE format, even if Long_Long_Float is Vax_Float
   --  (and in that case the bounds don't correspond exactly).

   Universal_Fixed : Entity_Id;
   --  Entity for universal fixed type. This is a type with  arbitrary
   --  precision that can only appear in  a context with a specific type.
   --  Universal_Fixed labels the result of multiplication or division of
   --  two fixed point numbers, and has no specified bounds (since, unlike
   --  universal integer and universal real, it is never used for runtime
   --  calculations).

   Standard_Integer_8  : Entity_Id;
   Standard_Integer_16 : Entity_Id;
   Standard_Integer_32 : Entity_Id;
   Standard_Integer_64 : Entity_Id;
   --  These are signed integer types with the indicated sizes. Used for the
   --  underlying implementation types for fixed-point and enumeration types.

   Standard_Unsigned : Entity_Id;
   --  An unsigned type of the same size as Standard_Integer

   Standard_Unsigned_64 : Entity_Id;
   --  An unsigned type, mod 2 ** 64, size of 64 bits.

   Abort_Signal : Entity_Id;
   --  Entity for abort signal exception

   Standard_Op_Rotate_Left            : Entity_Id;
   Standard_Op_Rotate_Right           : Entity_Id;
   Standard_Op_Shift_Left             : Entity_Id;
   Standard_Op_Shift_Right            : Entity_Id;
   Standard_Op_Shift_Right_Arithmetic : Entity_Id;
   --  These entities are used for shift operators generated by the expander

   -----------------
   -- Subprograms --
   -----------------

   procedure Tree_Read;
   --  Initializes entity values in this package from the current tree file
   --  using Tree_IO. Note that Tree_Read includes all the initialization that
   --  is carried out by Create_Standard.

   procedure Tree_Write;
   --  Writes out the entity values in this package to the current tree file
   --  using Tree_IO.

end Stand;
