------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         G N A T . A L T I V E C                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2004-2019, Free Software Foundation, Inc.         --
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

-------------------------
-- General description --
-------------------------

--  This is the root of a package hierarchy offering an Ada binding to the
--  PowerPC AltiVec extensions, a set of 128bit vector types together with a
--  set of subprograms operating on them. Relevant documents are:

--  o AltiVec Technology, Programming Interface Manual (1999-06)
--    to which we will refer as [PIM], describes the data types, the
--    functional interface and the ABI conventions.

--  o AltiVec Technology, Programming Environments Manual (2002-02)
--    to which we will refer as [PEM], describes the hardware architecture
--    and instruction set.

--  These documents, as well as a number of others of general interest on the
--  AltiVec technology, are available from the Motorola/AltiVec Web site at:

--  http://www.freescale.com/altivec

--  The binding interface is structured to allow alternate implementations:
--  for real AltiVec capable targets, and for other targets. In the latter
--  case, everything is emulated in software. The two versions are referred
--  to as:

--  o The Hard binding for AltiVec capable targets (with the appropriate
--    hardware support and corresponding instruction set)

--  o The Soft binding for other targets (with the low level primitives
--    emulated in software).

--  In addition, interfaces that are not strictly part of the base AltiVec API
--  are provided, such as vector conversions to and from array representations,
--  which are of interest for client applications (e.g. for vector
--  initialization purposes).

--  Only the soft binding is available today

-----------------------------------------
-- General package architecture survey --
-----------------------------------------

--  The various vector representations are all "containers" of elementary
--  values, the possible types of which are declared in this root package to
--  be generally accessible.

--  From the user standpoint, the binding materializes as a consistent
--  hierarchy of units:

--                             GNAT.Altivec
--                           (component types)
--                                   |
--          o----------------o----------------o-------------o
--          |                |                |             |
--    Vector_Types   Vector_Operations   Vector_Views   Conversions

--  Users can manipulate vectors through two families of types: Vector
--  types and View types.

--  Vector types are available through the Vector_Types and Vector_Operations
--  packages, which implement the core binding to the AltiVec API, as
--  described in [PIM-2.1 data types] and [PIM-4 AltiVec operations and
--  predicates].

--  The layout of Vector objects is dependant on the target machine
--  endianness, and View types were devised to offer a higher level user
--  interface. With Views, a vector of 4 uints (1, 2, 3, 4) is always declared
--  with a VUI_View := (Values => (1, 2, 3, 4)), element 1 first, natural
--  notation to denote the element values, and indexed notation is available
--  to access individual elements.

--  View types do not represent Altivec vectors per se, in the sense that the
--  Altivec_Operations are not available for them. They are intended to allow
--  Vector initializations as well as access to the Vector component values.

--  The GNAT.Altivec.Conversions package is provided to convert a View to the
--  corresponding Vector and vice-versa.

---------------------------
-- Underlying principles --
---------------------------

--  Internally, the binding relies on an abstraction of the Altivec API, a
--  rich set of functions around a core of low level primitives mapping to
--  AltiVec instructions. See for instance "vec_add" in [PIM-4.4 Generic and
--  Specific AltiVec operations], with no less than six result/arguments
--  combinations of byte vector types that map to "vaddubm".

--  The "soft" version is a software emulation of the low level primitives.

--  The "hard" version would map to real AltiVec instructions via GCC builtins
--  and inlining.

--  See the "Design Notes" section below for additional details on the
--  internals.

-------------------
-- Example usage --
-------------------

--  Here is a sample program declaring and initializing two vectors, 'add'ing
--  them and displaying the result components:

--  with GNAT.Altivec.Vector_Types;      use GNAT.Altivec.Vector_Types;
--  with GNAT.Altivec.Vector_Operations; use GNAT.Altivec.Vector_Operations;
--  with GNAT.Altivec.Vector_Views;      use GNAT.Altivec.Vector_Views;
--  with GNAT.Altivec.Conversions;       use GNAT.Altivec.Conversions;

--  use GNAT.Altivec;

--  with Ada.Text_IO; use Ada.Text_IO;

--  procedure Sample is
--     Va : Vector_Unsigned_Int := To_Vector ((Values => (1, 2, 3, 4)));
--     Vb : Vector_Unsigned_Int := To_Vector ((Values => (1, 2, 3, 4)));

--     Vs : Vector_Unsigned_Int;
--     Vs_View : VUI_View;
--  begin
--     Vs := Vec_Add (Va, Vb);
--     Vs_View := To_View (Vs);

--     for I in Vs_View.Values'Range loop
--        Put_Line (Unsigned_Int'Image (Vs_View.Values (I)));
--     end loop;
--  end;

--  $ gnatmake sample.adb
--  [...]
--  $ ./sample
--  2
--  4
--  6
--  8

------------------------------------------------------------------------------

with System;

package GNAT.Altivec is

   --  Definitions of constants and vector/array component types common to all
   --  the versions of the binding.

   --  All the vector types are 128bits

   VECTOR_BIT : constant := 128;

   -------------------------------------------
   -- [PIM-2.3.1 Alignment of vector types] --
   -------------------------------------------

   --  "A defined data item of any vector data type in memory is always
   --  aligned on a 16-byte boundary. A pointer to any vector data type always
   --  points to a 16-byte boundary. The compiler is responsible for aligning
   --  vector data types on 16-byte boundaries."

   VECTOR_ALIGNMENT : constant := Natural'Min (16, Standard'Maximum_Alignment);
   --  This value is used to set the alignment of vector datatypes in both the
   --  hard and the soft binding implementations.
   --
   --  We want this value to never be greater than 16, because none of the
   --  binding implementations requires larger alignments and such a value
   --  would cause useless space to be allocated/wasted for vector objects.
   --  Furthermore, the alignment of 16 matches the hard binding leading to
   --  a more faithful emulation.
   --
   --  It needs to be exactly 16 for the hard binding, and the initializing
   --  expression is just right for this purpose since Maximum_Alignment is
   --  expected to be 16 for the real Altivec ABI.
   --
   --  The soft binding doesn't rely on strict 16byte alignment, and we want
   --  the value to be no greater than Standard'Maximum_Alignment in this case
   --  to ensure it is supported on every possible target.

   -------------------------------------------------------
   -- [PIM-2.1] Data Types - Interpretation of contents --
   -------------------------------------------------------

   ---------------------
   -- char components --
   ---------------------

   CHAR_BIT    : constant := 8;
   SCHAR_MIN   : constant := -2 ** (CHAR_BIT - 1);
   SCHAR_MAX   : constant := 2 ** (CHAR_BIT - 1) - 1;
   UCHAR_MAX   : constant := 2 ** CHAR_BIT - 1;

   type unsigned_char is mod UCHAR_MAX + 1;
   for unsigned_char'Size use CHAR_BIT;

   type signed_char is range SCHAR_MIN .. SCHAR_MAX;
   for signed_char'Size use CHAR_BIT;

   subtype bool_char is unsigned_char;
   --  ??? There is a difference here between what the Altivec Technology
   --  Programming Interface Manual says and what GCC says. In the manual,
   --  vector_bool_char is a vector_unsigned_char, while in altivec.h it
   --  is a vector_signed_char.

   bool_char_True  : constant bool_char := bool_char'Last;
   bool_char_False : constant bool_char := 0;

   ----------------------
   -- short components --
   ----------------------

   SHORT_BIT   : constant := 16;
   SSHORT_MIN  : constant := -2 ** (SHORT_BIT - 1);
   SSHORT_MAX  : constant := 2 ** (SHORT_BIT - 1) - 1;
   USHORT_MAX  : constant := 2 ** SHORT_BIT - 1;

   type unsigned_short is mod USHORT_MAX + 1;
   for unsigned_short'Size use SHORT_BIT;

   subtype unsigned_short_int is unsigned_short;

   type signed_short is range SSHORT_MIN .. SSHORT_MAX;
   for signed_short'Size use SHORT_BIT;

   subtype signed_short_int is signed_short;

   subtype bool_short is unsigned_short;
   --  ??? See bool_char

   bool_short_True  : constant bool_short := bool_short'Last;
   bool_short_False : constant bool_short := 0;

   subtype bool_short_int is bool_short;

   --------------------
   -- int components --
   --------------------

   INT_BIT     : constant := 32;
   SINT_MIN    : constant := -2 ** (INT_BIT - 1);
   SINT_MAX    : constant := 2 ** (INT_BIT - 1) - 1;
   UINT_MAX    : constant := 2 ** INT_BIT - 1;

   type unsigned_int is mod UINT_MAX + 1;
   for unsigned_int'Size use INT_BIT;

   type signed_int is range SINT_MIN .. SINT_MAX;
   for signed_int'Size use INT_BIT;

   subtype bool_int is unsigned_int;
   --  ??? See bool_char

   bool_int_True  : constant bool_int := bool_int'Last;
   bool_int_False : constant bool_int := 0;

   ----------------------
   -- float components --
   ----------------------

   FLOAT_BIT   : constant := 32;
   FLOAT_DIGIT : constant := 6;
   FLOAT_MIN   : constant := -16#0.FFFF_FF#E+32;
   FLOAT_MAX   : constant := 16#0.FFFF_FF#E+32;

   type C_float is digits FLOAT_DIGIT range FLOAT_MIN .. FLOAT_MAX;
   for C_float'Size use FLOAT_BIT;
   --  Altivec operations always use the standard native floating-point
   --  support of the target. Note that this means that there may be
   --  minor differences in results between targets when the floating-
   --  point implementations are slightly different, as would happen
   --  with normal non-Altivec floating-point operations. In particular
   --  the Altivec simulations may yield slightly different results
   --  from those obtained on a true hardware Altivec target if the
   --  floating-point implementation is not 100% compatible.

   ----------------------
   -- pixel components --
   ----------------------

   subtype pixel is unsigned_short;

   -----------------------------------------------------------
   -- Subtypes for variants found in the GCC implementation --
   -----------------------------------------------------------

   subtype c_int is signed_int;
   subtype c_short is c_int;

   LONG_BIT  : constant := 32;
   --  Some of the GCC builtins are built with "long" arguments and
   --  expect SImode to come in.

   SLONG_MIN : constant := -2 ** (LONG_BIT - 1);
   SLONG_MAX : constant :=  2 ** (LONG_BIT - 1) - 1;
   ULONG_MAX : constant :=  2 ** LONG_BIT - 1;

   type signed_long   is range SLONG_MIN .. SLONG_MAX;
   type unsigned_long is mod ULONG_MAX + 1;

   subtype c_long is signed_long;

   subtype c_ptr is System.Address;

   ---------------------------------------------------------
   -- Access types, for the sake of some argument passing --
   ---------------------------------------------------------

   type signed_char_ptr    is access all signed_char;
   type unsigned_char_ptr  is access all unsigned_char;

   type short_ptr          is access all c_short;
   type signed_short_ptr   is access all signed_short;
   type unsigned_short_ptr is access all unsigned_short;

   type int_ptr            is access all c_int;
   type signed_int_ptr     is access all signed_int;
   type unsigned_int_ptr   is access all unsigned_int;

   type long_ptr           is access all c_long;
   type signed_long_ptr    is access all signed_long;
   type unsigned_long_ptr  is access all unsigned_long;

   type float_ptr          is access all Float;

   --

   type const_signed_char_ptr    is access constant signed_char;
   type const_unsigned_char_ptr  is access constant unsigned_char;

   type const_short_ptr          is access constant c_short;
   type const_signed_short_ptr   is access constant signed_short;
   type const_unsigned_short_ptr is access constant unsigned_short;

   type const_int_ptr            is access constant c_int;
   type const_signed_int_ptr     is access constant signed_int;
   type const_unsigned_int_ptr   is access constant unsigned_int;

   type const_long_ptr           is access constant c_long;
   type const_signed_long_ptr    is access constant signed_long;
   type const_unsigned_long_ptr  is access constant unsigned_long;

   type const_float_ptr          is access constant Float;

   --  Access to const volatile arguments need specialized types

   type volatile_float is new Float;
   pragma Volatile (volatile_float);

   type volatile_signed_char is new signed_char;
   pragma Volatile (volatile_signed_char);

   type volatile_unsigned_char is new unsigned_char;
   pragma Volatile (volatile_unsigned_char);

   type volatile_signed_short is new signed_short;
   pragma Volatile (volatile_signed_short);

   type volatile_unsigned_short is new unsigned_short;
   pragma Volatile (volatile_unsigned_short);

   type volatile_signed_int is new signed_int;
   pragma Volatile (volatile_signed_int);

   type volatile_unsigned_int is new unsigned_int;
   pragma Volatile (volatile_unsigned_int);

   type volatile_signed_long is new signed_long;
   pragma Volatile (volatile_signed_long);

   type volatile_unsigned_long is new unsigned_long;
   pragma Volatile (volatile_unsigned_long);

   type constv_char_ptr           is access constant volatile_signed_char;
   type constv_signed_char_ptr    is access constant volatile_signed_char;
   type constv_unsigned_char_ptr  is access constant volatile_unsigned_char;

   type constv_short_ptr          is access constant volatile_signed_short;
   type constv_signed_short_ptr   is access constant volatile_signed_short;
   type constv_unsigned_short_ptr is access constant volatile_unsigned_short;

   type constv_int_ptr            is access constant volatile_signed_int;
   type constv_signed_int_ptr     is access constant volatile_signed_int;
   type constv_unsigned_int_ptr   is access constant volatile_unsigned_int;

   type constv_long_ptr           is access constant volatile_signed_long;
   type constv_signed_long_ptr    is access constant volatile_signed_long;
   type constv_unsigned_long_ptr  is access constant volatile_unsigned_long;

   type constv_float_ptr  is access constant volatile_float;

private

   -----------------------
   -- Various constants --
   -----------------------

   CR6_EQ     : constant := 0;
   CR6_EQ_REV : constant := 1;
   CR6_LT     : constant := 2;
   CR6_LT_REV : constant := 3;

end GNAT.Altivec;

--------------------
--  Design Notes  --
--------------------

------------------------
-- General principles --
------------------------

--  The internal organization has been devised from a number of driving ideas:

--  o From the clients standpoint, the two versions of the binding should be
--    as easily exchangable as possible,

--  o From the maintenance standpoint, we want to avoid as much code
--    duplication as possible.

--  o From both standpoints above, we want to maintain a clear interface
--    separation between the base bindings to the Motorola API and the
--    additional facilities.

--  The identification of the low level interface is directly inspired by the
--  the base API organization, basically consisting of a rich set of functions
--  around a core of low level primitives mapping to AltiVec instructions.

--  See for instance "vec_add" in [PIM-4.4 Generic and Specific AltiVec
--  operations]: no less than six result/arguments combinations of byte vector
--  types map to "vaddubm".

--  The "hard" version of the low level primitives map to real AltiVec
--  instructions via the corresponding GCC builtins. The "soft" version is
--  a software emulation of those.

---------------------------------------
-- The Low_Level_Vectors abstraction --
---------------------------------------

--  The AltiVec C interface spirit is to map a large set of C functions down
--  to a much smaller set of AltiVec instructions, most of them operating on a
--  set of vector data types in a transparent manner. See for instance the
--  case of vec_add, which maps six combinations of result/argument types to
--  vaddubm for signed/unsigned/bool variants of 'char' components.

--  The GCC implementation of this idiom for C/C++ is to setup builtins
--  corresponding to the instructions and to expose the C user function as
--  wrappers around those builtins with no-op type conversions as required.
--  Typically, for the vec_add case mentioned above, we have (altivec.h):
--
--    inline __vector signed char
--    vec_add (__vector signed char a1, __vector signed char a2)
--    {
--      return (__vector signed char)
--        __builtin_altivec_vaddubm ((__vector signed char) a1,
--                                   (__vector signed char) a2);
--    }

--    inline __vector unsigned char
--    vec_add (__vector __bool char a1, __vector unsigned char a2)
--    {
--      return (__vector unsigned char)
--        __builtin_altivec_vaddubm ((__vector signed char) a1,
--                                   (__vector signed char) a2);
--    }

--  The central idea for the Ada bindings is to leverage on the existing GCC
--  architecture, with the introduction of a Low_Level_Vectors abstraction.
--  This abstaction acts as a representative of the vector-types and builtins
--  compiler interface for either the Hard or the Soft case.

--  For the Hard binding, Low_Level_Vectors exposes data types with a GCC
--  internal translation identical to the "vector ..." C types, and a set of
--  subprograms mapping straight to the internal GCC builtins.

--  For the Soft binding, Low_Level_Vectors exposes the same set of types
--  and subprograms, with bodies simulating the instructions behavior.

--  Vector_Types/Operations "simply" bind the user types and operations to
--  some Low_Level_Vectors implementation, selected in accordance with the
--  target

--  To achieve a complete Hard/Soft independence in the Vector_Types and
--  Vector_Operations implementations, both versions of the low level support
--  are expected to expose a number of facilities:

--  o Private data type declarations for base vector representations embedded
--    in the user visible vector types, that is:

--      LL_VBC, LL_VUC and LL_VSC
--        for vector_bool_char, vector_unsigned_char and vector_signed_char

--      LL_VBS, LL_VUS and LL_VSS
--        for vector_bool_short, vector_unsigned_short and vector_signed_short

--      LL_VBI, LL_VUI and LL_VSI
--        for vector_bool_int, vector_unsigned_int and vector_signed_int

--    as well as:

--      LL_VP for vector_pixel and LL_VF for vector_float

--  o Primitive operations corresponding to the AltiVec hardware instruction
--    names, like "vaddubm". The whole set is not described here. The actual
--    sets are inspired from the GCC builtins which are invoked from GCC's
--    "altivec.h".

--  o An LL_Altivec convention identifier, specifying the calling convention
--    to be used to access the aforementioned primitive operations.

--  Besides:

--  o Unchecked_Conversion are expected to be allowed between any pair of
--    exposed data types, and are expected to have no effect on the value
--    bit patterns.

-------------------------
-- Vector views layout --
-------------------------

--  Vector Views combine intuitive user level ordering for both elements
--  within a vector and bytes within each element. They basically map to an
--  array representation where array(i) always represents element (i), in the
--  natural target representation. This way, a user vector (1, 2, 3, 4) is
--  represented as:

--                                                       Increasing Addresses
--  ------------------------------------------------------------------------->

--  | 0x0 0x0 0x0 0x1 | 0x0 0x0 0x0 0x2 | 0x0 0x0 0x0 0x3 | 0x0 0x0 0x0 0x4 |
--  | V (0), BE       | V (1), BE       | V (2), BE       | V (3), BE       |

--  on a big endian target, and as:

--  | 0x1 0x0 0x0 0x0 | 0x2 0x0 0x0 0x0 | 0x3 0x0 0x0 0x0 | 0x4 0x0 0x0 0x0 |
--  | V (0), LE       | V (1), LE       | V (2), LE       | V (3), LE       |

--  on a little-endian target

-------------------------
-- Vector types layout --
-------------------------

--  In the case of the hard binding, the layout of the vector type in
--  memory is documented by the Altivec documentation. In the case of the
--  soft binding, the simplest solution is to represent a vector as an
--  array of components. This representation can depend on the endianness.
--  We can consider three possibilities:

--  * First component at the lowest address, components in big endian format.
--  It is the natural way to represent an array in big endian, and it would
--  also be the natural way to represent a quad-word integer in big endian.

--  Example:

--  Let V be a vector of unsigned int which value is (1, 2, 3, 4). It is
--  represented as:

--                                                           Addresses growing
--  ------------------------------------------------------------------------->
--  | 0x0 0x0 0x0 0x1 | 0x0 0x0 0x0 0x2 | 0x0 0x0 0x0 0x3 | 0x0 0x0 0x0 0x4 |
--  | V (0), BE       | V (1), BE       | V (2), BE       | V (3), BE       |

--  * First component at the lowest address, components in little endian
--  format. It is the natural way to represent an array in little endian.

--  Example:

--  Let V be a vector of unsigned int which value is (1, 2, 3, 4). It is
--  represented as:

--                                                           Addresses growing
--  ------------------------------------------------------------------------->
--  | 0x1 0x0 0x0 0x0 | 0x2 0x0 0x0 0x0 | 0x3 0x0 0x0 0x0 | 0x4 0x0 0x0 0x0 |
--  | V (0), LE       | V (1), LE       | V (2), LE       | V (3), LE       |

--  * Last component at the lowest address, components in little endian format.
--  It is the natural way to represent a quad-word integer in little endian.

--  Example:

--  Let V be a vector of unsigned int which value is (1, 2, 3, 4). It is
--  represented as:

--                                                           Addresses growing
--  ------------------------------------------------------------------------->
--  | 0x4 0x0 0x0 0x0 | 0x3 0x0 0x0 0x0 | 0x2 0x0 0x0 0x0 | 0x1 0x0 0x0 0x0 |
--  | V (3), LE       | V (2), LE       | V (1), LE       | V (0), LE       |

--  There is actually a fourth case (components in big endian, first
--  component at the lowest address), but it does not have any interesting
--  properties: it is neither the natural way to represent a quad-word on any
--  machine, nor the natural way to represent an array on any machine.

--  Example:

--  Let V be a vector of unsigned int which value is (1, 2, 3, 4). It is
--  represented as:

--                                                           Addresses growing
--  ------------------------------------------------------------------------->
--  | 0x0 0x0 0x0 0x4 | 0x0 0x0 0x0 0x3 | 0x0 0x0 0x0 0x2 | 0x0 0x0 0x0 0x1 |
--  | V (3), BE       | V (2), BE       | V (1), BE       | V (0), BE       |

--  Most of the Altivec operations are specific to a component size, and
--  can be implemented with any of these three formats. But some operations
--  are defined by the same Altivec primitive operation for different type
--  sizes:

--  * operations doing arithmetics on a complete vector, seen as a quad-word;
--  * operations dealing with memory.

--  Operations on a complete vector:
--  --------------------------------

--  Examples:

--  vec_sll/vsl : shift left on the entire vector.
--  vec_slo/vslo: shift left on the entire vector, by octet.

--  Those operations works on vectors seens as a quad-word.
--  Let us suppose that we have a conversion operation named To_Quad_Word
--  for converting vector types to a quad-word.

--  Let A be a Altivec vector of 16 components:
--  A = (A(0), A(1), A(2), A(3), ... , A(14), A(15))
--  Let B be a Altivec vector of 8 components verifying:
--  B = (A(0) |8| A(1), A(2) |8| A(3), ... , A(14) |8| A(15))
--  Let C be a Altivec vector of 4 components verifying:
--  C = (A(0)  |8| A(1)  |8| A(2)  |8| A(3), ... ,
--       A(12) |8| A(13) |8| A(14) |8| A(15))

--  (definition: |8| is the concatenation operation between two bytes;
--  i.e. 0x1 |8| 0x2 = 0x0102)

--  According to [PIM - 4.2 byte ordering], we have the following property:
--  To_Quad_Word (A) = To_Quad_Word (B) = To_Quad_Word (C)

--  Let To_Type_Of_A be a conversion operation from the type of B to the
--  type of A.  The quad-word operations are only implemented by one
--  Altivec primitive operation.  That means that, if QW_Operation is a
--  quad-word operation, we should have:
--  QW_Operation (To_Type_Of_A (B)) = QW_Operation (A)

--  That is true iff:
--  To_Quad_Word (To_Type_Of_A (B)) = To_Quad_Word (A)

--  As To_Quad_Word is a bijection. we have:
--  To_Type_Of_A (B) = A

--  resp. any combination of A, B, C:
--  To_Type_Of_A (C) = A
--  To_Type_Of_B (A) = B
--  To_Type_Of_C (B) = C
--  ...

--  Making sure that the properties described above are verified by the
--  conversion operations between vector types has different implications
--  depending on the layout of the vector types:
--  * with format 1 and 3: only a unchecked conversion is needed;
--  * with format 2 and 4: some reorganisation is needed for conversions
--  between vector types with different component sizes; that has a cost on the
--  efficiency, plus the complexity of having different memory pattern for
--  the same quad-word value, depending on the type.

--  Operation dealing with memory:
--  ------------------------------

--  These operations are either load operation (vec_ld and the
--  corresponding primitive operation: vlx) or store operation (vec_st
--  and the corresponding primitive operation: vstx).

--  According to [PIM 4.4 - vec_ld], those operations take in input
--  either an access to a vector (e.g. a const_vector_unsigned_int_ptr)
--  or an access to a flow of components (e.g. a const_unsigned_int_ptr),
--  relying on the same Altivec primitive operations. That means that both
--  should have the same representation in memory.

--  For the stream, it is easier to adopt the format of the target. That
--  means that, in memory, the components of the vector should also have the
--  format of the target. meaning that we will prefer:
--  * On a big endian target: format 1 or 4
--  * On a little endian target: format 2 or 3

--  Conclusion:
--  -----------

--  To take into consideration the constraint brought about by the routines
--  operating on quad-words and the routines operating on memory, the best
--  choice seems to be:

--  * On a big endian target: format 1;
--  * On a little endian target: format 3.

--  Those layout choices are enforced by GNAT.Altivec.Low_Level_Conversions,
--  which is the endianness-dependant unit providing conversions between
--  vector views and vector types.

----------------------
--  Layouts summary --
----------------------

--  For a user abstract vector of 4 uints (1, 2, 3, 4), increasing
--  addresses from left to right:

--  =========================================================================
--                 BIG ENDIAN TARGET MEMORY LAYOUT for (1, 2, 3, 4)
--  =========================================================================

--                                    View
--  -------------------------------------------------------------------------
--  | 0x0 0x0 0x0 0x1 | 0x0 0x0 0x0 0x2 | 0x0 0x0 0x0 0x3 | 0x0 0x0 0x0 0x4 |
--  | V (0), BE       | V (1), BE       | V (2), BE       | V (3), BE       |
--  -------------------------------------------------------------------------

--                                   Vector
--  -------------------------------------------------------------------------
--  | 0x0 0x0 0x0 0x1 | 0x0 0x0 0x0 0x2 | 0x0 0x0 0x0 0x3 | 0x0 0x0 0x0 0x4 |
--  | V (0), BE       | V (1), BE       | V (2), BE       | V (3), BE       |
--  -------------------------------------------------------------------------

--  =========================================================================
--              LITTLE ENDIAN TARGET MEMORY LAYOUT for (1, 2, 3, 4)
--  =========================================================================

--                                    View
--  -------------------------------------------------------------------------
--  | 0x1 0x0 0x0 0x0 | 0x2 0x0 0x0 0x0 | 0x3 0x0 0x0 0x0 | 0x4 0x0 0x0 0x0 |
--  | V (0), LE       | V (1), LE       | V (2), LE       | V (3), LE       |

--                                    Vector
--  -------------------------------------------------------------------------
--  | 0x4 0x0 0x0 0x0 | 0x3 0x0 0x0 0x0 | 0x2 0x0 0x0 0x0 | 0x1 0x0 0x0 0x0 |
--  | V (3), LE       | V (2), LE       | V (1), LE       | V (0), LE       |
--  -------------------------------------------------------------------------

--  These layouts are common to both the soft and hard implementations on
--  Altivec capable targets.
