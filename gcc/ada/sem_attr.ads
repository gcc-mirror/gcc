------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ A T T R                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; see file COPYING3.  If not see                        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Attribute handling is isolated in a separate package to ease the addition
--  of implementation defined attributes. Logically this processing belongs
--  in chapter 4. See Sem_Ch4 for a description of the relation of the
--  Analyze and Resolve routines for expression components.

--  This spec also documents all GNAT implementation defined pragmas

with Exp_Tss; use Exp_Tss;
with Namet;   use Namet;
with Snames;  use Snames;
with Types;   use Types;

package Sem_Attr is

   -----------------------------------------
   -- Implementation Dependent Attributes --
   -----------------------------------------

   --  This section describes the implementation dependent attributes
   --  provided in GNAT, as well as constructing an array of flags
   --  indicating which attributes these are.

   Attribute_Impl_Def : Attribute_Class_Array := Attribute_Class_Array'(

      ------------------
      -- Abort_Signal --
      ------------------

      Attribute_Abort_Signal => True,
      --  Standard'Abort_Signal (Standard is the only allowed prefix) provides
      --  the entity for the special exception used to signal task abort or
      --  asynchronous transfer of control. Normally this attribute should only
      --  be used in the tasking runtime (it is highly peculiar, and completely
      --  outside the normal semantics of Ada, for a user program to intercept
      --  the abort exception).

      ------------------
      -- Address_Size --
      ------------------

      Attribute_Address_Size => True,
      --  Standard'Address_Size (Standard is the only allowed prefix) is
      --  a static constant giving the number of bits in an Address. It
      --  is used primarily for constructing the definition of Memory_Size
      --  in package Standard, but may be freely used in user programs.
      --  This is a static attribute.

      ---------------
      -- Asm_Input --
      ---------------

      Attribute_Asm_Input => True,
      --  Used only in conjunction with the Asm subprograms in package
      --  Machine_Code to construct machine instructions. See documentation
      --  in package Machine_Code in file s-maccod.ads.

      ----------------
      -- Asm_Output --
      ----------------

      Attribute_Asm_Output => True,
      --  Used only in conjunction with the Asm subprograms in package
      --  Machine_Code to construct machine instructions. See documentation
      --  in package Machine_Code in file s-maccod.ads.

      ---------------
      -- AST_Entry --
      ---------------

      Attribute_AST_Entry => True,
      --  E'Ast_Entry, where E is a task entry, yields a value of the
      --  predefined type System.DEC.AST_Handler, that enables the given
      --  entry to be called when an AST occurs. If the name to which the
      --  attribute applies has not been specified with the pragma AST_Entry,
      --  the attribute returns the value No_Ast_Handler, and no AST occurs.
      --  If the entry is for a task that is not callable (T'Callable False),
      --  the exception program error is raised. If an AST occurs for an
      --  entry of a task that is terminated, the program is erroneous.
      --
      --  The attribute AST_Entry is supported only in OpenVMS versions
      --  of GNAT. It will be rejected as illegal in other GNAT versions.

      ---------
      -- Bit --
      ---------

      Attribute_Bit => True,
      --  Obj'Bit, where Obj is any object, yields the bit offset within the
      --  storage unit (byte) that contains the first bit of storage allocated
      --  for the object. The attribute value is of type Universal_Integer,
      --  and is always a non-negative number not exceeding the value of
      --  System.Storage_Unit.
      --
      --  For an object that is a variable or a constant allocated in a
      --  register, the value is zero. (The use of this attribute does not
      --  force the allocation of a variable to memory).
      --
      --  For an object that is a formal parameter, this attribute applies to
      --  either the matching actual parameter or to a copy of the matching
      --  actual parameter.
      --
      --  For an access object the value is zero. Note that Obj.all'Bit is
      --  subject to an Access_Check for the designated object. Similarly
      --  for a record component X.C'Bit is subject to a discriminant check
      --  and X(I).Bit and X(I1..I2)'Bit are subject to index checks.
      --
      --  This attribute is designed to be compatible with the DEC Ada
      --  definition and implementation of the Bit attribute.

      ------------------
      -- Code_Address --
      ------------------

      Attribute_Code_Address => True,
      --  The reference subp'Code_Address, where subp is a subprogram entity,
      --  gives the address of the first generated instruction for the sub-
      --  program. This is often, but not always the same as the 'Address
      --  value, which is the address to be used in a call. The differences
      --  occur in the case of a nested procedure (where Address yields the
      --  address of the trampoline code used to load the static link), and on
      --  some systems which use procedure descriptors (in which case Address
      --  yields the address of the descriptor).

      -----------------------
      -- Default_Bit_Order --
      -----------------------

      Attribute_Default_Bit_Order => True,
      --  Standard'Default_Bit_Order (Standard is the only permissible prefix),
      --  provides the value System.Default_Bit_Order as a Pos value (0 for
      --  High_Order_First, 1 for Low_Order_First). This is used to construct
      --  the definition of Default_Bit_Order in package System. This is a
      --  static attribute.

      ---------------
      -- Elab_Body --
      ---------------

      Attribute_Elab_Body => True,
      --  This attribute can only be applied to a program unit name. It returns
      --  the entity for the corresponding elaboration procedure for elabor-
      --  ating the body of the referenced unit. This is used in the main
      --  generated elaboration procedure by the binder, and is not normally
      --  used in any other context, but there may be specialized situations in
      --  which it is useful to be able to call this elaboration procedure from
      --  Ada code, e.g. if it is necessary to do selective reelaboration to
      --  fix some error.

      ---------------
      -- Elab_Spec --
      ---------------

      Attribute_Elab_Spec => True,
      --  This attribute can only be applied to a program unit name. It
      --  returns the entity for the corresponding elaboration procedure
      --  for elaborating the spec of the referenced unit. This is used
      --  in the main generated elaboration procedure by the binder, and
      --  is not normally used in any other context, but there may be
      --  specialized situations in which it is useful to be able to
      --  call this elaboration procedure from Ada code, e.g. if it
      --  is necessary to do selective reelaboration to fix some error.

      ----------------
      -- Elaborated --
      ----------------

      Attribute_Elaborated => True,
      --  Lunit'Elaborated, where Lunit is a library unit, yields a boolean
      --  value indicating whether or not the body of the designated library
      --  unit has been elaborated yet.

      --------------
      -- Enum_Rep --
      --------------

      Attribute_Enum_Rep => True,
      --  For every enumeration subtype S, S'Enum_Rep denotes a function
      --  with the following specification:
      --
      --    function S'Enum_Rep (Arg : S'Base) return universal_integer;
      --
      --  The function returns the representation value for the given
      --  enumeration value. This will be equal to the 'Pos value in the
      --  absence of an enumeration representation clause. This is a static
      --  attribute (i.e. the result is static if the argument is static).

      --------------
      -- Enum_Val --
      --------------

      Attribute_Enum_Val => True,
      --  For every enumeration subtype S, S'Enum_Val denotes a function
      --  with the following specification:
      --
      --    function S'Enum_Val (Arg : universal_integer) return S'Base;
      --
      --  This function performs the inverse transformation to Enum_Rep. Given
      --  a representation value for the type, it returns the corresponding
      --  enumeration value. Constraint_Error is raised if no value of the
      --  enumeration type corresponds to the given integer value.

      -----------------
      -- Fixed_Value --
      -----------------

      Attribute_Fixed_Value => True,
      --  For every fixed-point type S, S'Fixed_Value denotes a function
      --  with the following specification:
      --
      --    function S'Fixed_Value (Arg : universal_integer) return S;
      --
      --  The value returned is the fixed-point value V such that
      --
      --    V = Arg * S'Small
      --
      --  The effect is thus equivalent to first converting the argument to
      --  the integer type used to represent S, and then doing an unchecked
      --  conversion to the fixed-point type. This attribute is primarily
      --  intended for use in implementation of the input-output functions for
      --  fixed-point values.

      -----------------------
      -- Has_Discriminants --
      -----------------------

      Attribute_Has_Discriminants => True,
      --  Gtyp'Has_Discriminants, where Gtyp is a generic formal type, yields
      --  a Boolean value indicating whether or not the actual instantiation
      --  type has discriminants.

      ---------
      -- Img --
      ---------

      Attribute_Img => True,
      --  The 'Img function is defined for any prefix, P, that denotes an
      --  object of scalar type T. P'Img is equivalent to T'Image (P). This
      --  is convenient for debugging. For example:
      --
      --     Put_Line ("X = " & X'Img);
      --
      --  has the same meaning as the more verbose:
      --
      --     Put_Line ("X = " & Temperature_Type'Image (X));
      --
      --  where Temperature_Type is the subtype of the object X.

      -------------------
      -- Integer_Value --
      -------------------

      Attribute_Integer_Value => True,
      --  For every integer type S, S'Integer_Value denotes a function
      --  with the following specification:
      --
      --    function S'Integer_Value (Arg : universal_fixed) return S;
      --
      --  The value returned is the integer value V, such that
      --
      --    Arg = V * fixed-type'Small
      --
      --  The effect is thus equivalent to first doing an unchecked convert
      --  from the fixed-point type to its corresponding implementation type,
      --  and then converting the result to the target integer type. This
      --  attribute is primarily intended for use in implementation of the
      --  standard input-output functions for fixed-point values.

      Attribute_Invalid_Value => True,
      --  For every scalar type, S'Invalid_Value designates an undefined value
      --  of the type. If possible this value is an invalid value, and in fact
      --  is identical to the value that would be set if Initialize_Scalars
      --  mode were in effect (including the behavior of its value on
      --  environment variables or binder switches). The intended use is
      --  to set a value where initialization is required (e.g. as a result of
      --  the coding standards in use), but logically no initialization is
      --  needed, and the value should never be accessed.

      ------------------
      -- Machine_Size --
      ------------------

      Attribute_Machine_Size => True,
      --  This attribute is identical to the Object_Size attribute. It is
      --  provided for compatibility with the DEC attribute of this name.

      -----------------------
      -- Maximum_Alignment --
      -----------------------

      Attribute_Maximum_Alignment => True,
      --  Standard'Maximum_Alignment (Standard is the only permissible prefix)
      --  provides the maximum useful alignment value for the target. This
      --  is a static value that can be used to specify the alignment for an
      --  object, guaranteeing that it is properly aligned in all cases. The
      --  time this is useful is when an external object is imported and its
      --  alignment requirements are unknown. This is a static attribute.

      --------------------
      -- Mechanism_Code --
      --------------------

      Attribute_Mechanism_Code => True,
      --  function'Mechanism_Code yields an integer code for the mechanism
      --  used for the result of function, and subprogram'Mechanism_Code (n)
      --  yields the mechanism used for formal parameter number n (a static
      --  integer value, 1 = first parameter). The code returned is:
      --
      --     1 = by copy (value)
      --     2 = by reference
      --     3 = by descriptor (default descriptor type)
      --     4 = by descriptor (UBS  unaligned bit string)
      --     5 = by descriptor (UBSB aligned bit string with arbitrary bounds)
      --     6 = by descriptor (UBA  unaligned bit array)
      --     7 = by descriptor (S    string, also scalar access type parameter)
      --     8 = by descriptor (SB   string with arbitrary bounds)
      --     9 = by descriptor (A    contiguous array)
      --    10 = by descriptor (NCA  non-contiguous array)

      --------------------
      -- Null_Parameter --
      --------------------

      Attribute_Null_Parameter => True,
      --  A reference T'Null_Parameter denotes an (imaginary) object of type or
      --  subtype T allocated at (machine) address zero. The attribute is
      --  allowed only as the default expression of a formal parameter, or as
      --  an actual expression of a subprogram call. In either case, the
      --  subprogram must be imported.
      --
      --  The identity of the object is represented by the address zero in the
      --  argument list, independent of the passing mechanism (explicit or
      --  default).
      --
      --  The reason that this capability is needed is that for a record or
      --  other composite object passed by reference, there is no other way of
      --  specifying that a zero address should be passed.

      -----------------
      -- Object_Size --
      -----------------

      Attribute_Object_Size => True,
      --  Type'Object_Size is the same as Type'Size for all types except
      --  fixed-point types and discrete types. For fixed-point types and
      --  discrete types, this attribute gives the size used for default
      --  allocation of objects and components of the size. See section in
      --  Einfo ("Handling of type'Size values") for further details.

      -------------------------
      -- Passed_By_Reference --
      -------------------------

      Attribute_Passed_By_Reference => True,
      --  T'Passed_By_Reference for any subtype T returns a boolean value that
      --  is true if the type is normally passed by reference and false if the
      --  type is normally passed by copy in calls. For scalar types, the
      --  result is always False and is static. For non-scalar types, the
      --  result is non-static (since it is computed by Gigi).

      ------------------
      -- Range_Length --
      ------------------

      Attribute_Range_Length => True,
      --  T'Range_Length for any discrete type T yields the number of values
      --  represented by the subtype (zero for a null range). The result is
      --  static for static subtypes. Note that Range_Length applied to the
      --  index subtype of a one dimensional array always gives the same result
      --  as Range applied to the array itself. The result is of type universal
      --  integer.

      ---------
      -- Ref --
      ---------

      Attribute_Ref => True,
      --  System.Address'Ref (Address is the only permissible prefix) is
      --  equivalent to System'To_Address, provided for compatibility with
      --  other compilers.

      ------------------
      -- Storage_Unit --
      ------------------

      Attribute_Storage_Unit => True,
      --  Standard'Storage_Unit (Standard is the only permissible prefix)
      --  provides the value System.Storage_Unit, and is intended primarily
      --  for constructing this definition in package System (see note above
      --  in Default_Bit_Order description). The is a static attribute.

      ---------------
      -- Stub_Type --
      ---------------

      Attribute_Stub_Type => True,
      --  The GNAT implementation of remote access-to-classwide types is
      --  organised as described in AARM E.4(20.t): a value of an RACW type
      --  (designating a remote object) is represented as a normal access
      --  value, pointing to a "stub" object which in turn contains the
      --  necessary information to contact the designated remote object. A
      --  call on any dispatching operation of such a stub object does the
      --  remote call, if necessary, using the information in the stub object
      --  to locate the target partition, etc.
      --
      --  For a prefix T that denotes a remote access-to-classwide type,
      --  T'Stub_Type denotes the type of the corresponding stub objects.
      --
      --  By construction, the layout of T'Stub_Type is identical to that of
      --  System.Partition_Interface.RACW_Stub_Type (see implementation notes
      --  in body of Exp_Dist).

      -----------------
      -- Target_Name --
      -----------------

      Attribute_Target_Name => True,
      --  Standard'Target_Name yields the string identifying the target for the
      --  compilation, taken from Sdefault.Target_Name.

      ----------------
      -- To_Address --
      ----------------

      Attribute_To_Address => True,
      --  System'To_Address (System is the only permissible prefix) is a
      --  function that takes any integer value, and converts it into an
      --  address value. The semantics is to first convert the integer value to
      --  type Integer_Address according to normal conversion rules, and then
      --  to convert this to an address using the same semantics as the
      --  System.Storage_Elements.To_Address function. The important difference
      --  is that this is a static attribute so it can be used in
      --  initializations in preelaborate packages.

      ----------------
      -- Type_Class --
      ----------------

      Attribute_Type_Class => True,
      --  T'Type_Class for any type or subtype T yields the value of the type
      --  class for the full type of T. If T is a generic formal type, then the
      --  value is the value for the corresponding actual subtype. The value of
      --  this attribute is of type System.Aux_DEC.Type_Class, which has the
      --  following definition:
      --
      --    type Type_Class is
      --      (Type_Class_Enumeration,
      --       Type_Class_Integer,
      --       Type_Class_Fixed_Point,
      --       Type_Class_Floating_Point,
      --       Type_Class_Array,
      --       Type_Class_Record,
      --       Type_Class_Access,
      --       Type_Class_Task,
      --       Type_Class_Address);
      --
      --  Protected types yield the value Type_Class_Task, which thus applies
      --  to all concurrent types. This attribute is designed to be compatible
      --  with the DEC Ada attribute of the same name.
      --
      --  Note: if pragma Extend_System is used to merge the definitions of
      --  Aux_DEC into System, then the type Type_Class can be referenced
      --  as an entity within System, as can its enumeration literals.

      -----------------
      -- UET_Address --
      -----------------

      Attribute_UET_Address => True,
      --  Unit'UET_Address, where Unit is a program unit, yields the address
      --  of the unit exception table for the specified unit. This is only
      --  used in the internal implementation of exception handling. See the
      --  implementation of unit Ada.Exceptions for details on its use.

      ------------------------------
      -- Universal_Literal_String --
      ------------------------------

      Attribute_Universal_Literal_String => True,
      --  The prefix of 'Universal_Literal_String must be a named number. The
      --  static result is the string consisting of the characters of the
      --  number as defined in the original source. This allows the user
      --  program to access the actual text of named numbers without
      --  intermediate conversions and without the need to enclose the strings
      --  in quotes (which would preclude their use as numbers). This is used
      --  internally for the construction of values of the floating-point
      --  attributes from the file ttypef.ads, but may also be used by user
      --  programs.

      -------------------------
      -- Unrestricted_Access --
      -------------------------

      Attribute_Unrestricted_Access => True,
      --  The Unrestricted_Access attribute is similar to Access except that
      --  all accessibility and aliased view checks are omitted. This is very
      --  much a user-beware attribute. Basically its status is very similar
      --  to Address, for which it is a desirable replacement where the value
      --  desired is an access type. In other words, its effect is identical
      --  to first taking 'Address and then doing an unchecked conversion to
      --  a desired access type. Note that in GNAT, but not necessarily in
      --  other implementations, the use of static chains for inner level
      --  subprograms means that Unrestricted_Access applied to a subprogram
      --  yields a value that can be called as long as the subprogram is in
      --  scope (normal Ada 95 accessibility rules restrict this usage).

      ---------------
      -- VADS_Size --
      ---------------

      Attribute_VADS_Size => True,
      --  Typ'VADS_Size yields the Size value typically yielded by some Ada 83
      --  compilers. The differences between VADS_Size and Size is that for
      --  scalar types for which no Size has been specified, VADS_Size yields
      --  the Object_Size rather than the Value_Size. For example, while
      --  Natural'Size is typically 31, the value of Natural'VADS_Size is 32.
      --  For all other types, Size and VADS_Size yield the same value.

      ----------------
      -- Value_Size --
      ----------------

      Attribute_Value_Size => True,
      --  Type'Value_Size is the number of bits required to represent value of
      --  the given subtype. It is the same as Type'Size, but, unlike Size, may
      --  be set for non-first subtypes. See section in Einfo ("Handling of
      --  type'Size values") for further details.

      ---------------
      -- Word_Size --
      ---------------

      Attribute_Word_Size => True,
      --  Standard'Word_Size (Standard is the only permissible prefix)
      --  provides the value System.Word_Size, and is intended primarily
      --  for constructing this definition in package System (see note above
      --  in Default_Bit_Order description). This is a static attribute.

      others => False);

   -----------------
   -- Subprograms --
   -----------------

   procedure Analyze_Attribute (N : Node_Id);
   --  Performs bottom up semantic analysis of an attribute. Note that the
   --  parser has already checked that type returning attributes appear only
   --  in appropriate contexts (i.e. in subtype marks, or as prefixes for
   --  other attributes).

   function Name_Implies_Lvalue_Prefix (Nam : Name_Id) return Boolean;
   --  Determine whether the name of an attribute reference categorizes its
   --  prefix as an lvalue. The following attributes fall under this bracket
   --  by directly or indirectly modifying their prefixes.
   --     Access
   --     Address
   --     Input
   --     Read
   --     Unchecked_Access
   --     Unrestricted_Access

   procedure Resolve_Attribute (N : Node_Id; Typ : Entity_Id);
   --  Performs type resolution of attribute. If the attribute yields a
   --  universal value, mark its type as that of the context. On the other
   --  hand, if the context itself is universal (as in T'Val (T'Pos (X)), mark
   --  the type as being the largest type of that class that can be used at
   --  run-time. This is correct since either the value gets folded (in which
   --  case it doesn't matter what type of the class we give if, since the
   --  folding uses universal arithmetic anyway) or it doesn't get folded (in
   --  which case it is going to be dealt with at runtime, and the largest type
   --  is right).

   function Stream_Attribute_Available
     (Typ          : Entity_Id;
      Nam          : TSS_Name_Type;
      Partial_View : Entity_Id := Empty) return Boolean;
   --  For a limited type Typ, return True iff the given attribute is
   --  available. For Ada 05, availability is defined by 13.13.2(36/1). For Ada
   --  95, an attribute is considered to be available if it has been specified
   --  using an attribute definition clause for the type, or for its full view,
   --  or for an ancestor of either. Parameter Partial_View is used only
   --  internally, when checking for an attribute definition clause that is not
   --  visible (Ada 95 only).

end Sem_Attr;
