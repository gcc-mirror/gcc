------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P A K D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  Expand routines for manipulation of packed arrays

with Types; use Types;

package Exp_Pakd is

   -------------------------------------
   -- Implementation of Packed Arrays --
   -------------------------------------

   --  When a packed array (sub)type is frozen, we create a corresponding
   --  type that will be used to hold the bits of the packed value, and
   --  store the entity for this type in the Packed_Array_Type field of the
   --  E_Array_Type or E_Array_Subtype entity for the packed array.

   --  This packed array type has the name xxxPn, where xxx is the name
   --  of the packed type, and n is the component size. The expanded
   --  declaration declares a type that is one of the following:

   --    For an unconstrained array with component size 1,2,4 or any other
   --    odd component size. These are the cases in which we do not need
   --    to align the underlying array.

   --      type xxxPn is new Packed_Bytes1;

   --    For an unconstrained array with component size that is divisible
   --    by 2, but not divisible by 4 (other than 2 itself). These are the
   --    cases in which we can generate better code if the underlying array
   --    is 2-byte aligned (see System.Pack_14 in file s-pack14 for example).

   --      type xxxPn is new Packed_Bytes2;

   --    For an unconstrained array with component size that is divisible
   --    by 4, other than powers of 2 (which either come under the 1,2,4
   --    exception above, or are not packed at all). These are cases where
   --    we can generate better code if the underlying array is 4-byte
   --    aligned (see System.Pack_20 in file s-pack20 for example).

   --      type xxxPn is new Packed_Bytes4;

   --    For a constrained array with a static index type where the number
   --    of bits does not exceed the size of Unsigned:

   --      type xxxPn is new Unsigned range 0 .. 2 ** nbits - 1;

   --    For a constrained array with a static index type where the number
   --    of bits is greater than the size of Unsigned, but does not exceed
   --    the size of Long_Long_Unsigned:

   --       type xxxPn is new Long_Long_Unsigned range 0 .. 2 ** nbits - 1;

   --    For all other constrained arrays, we use one of

   --       type xxxPn is new Packed_Bytes1 (0 .. m);
   --       type xxxPn is new Packed_Bytes2 (0 .. m);
   --       type xxxPn is new Packed_Bytes4 (0 .. m);

   --    where m is calculated (from the length of the original packed array)
   --    to hold the required number of bits, and the choice of the particular
   --    Packed_Bytes{1,2,4} type is made on the basis of alignment needs as
   --    described above for the unconstrained case.

   --  When a variable of packed array type is allocated, gigi will allocate
   --  the amount of space indicated by the corresponding packed array type.
   --  However, we do NOT attempt to rewrite the types of any references or
   --  to retype the variable itself, since this would cause all kinds of
   --  semantic problems in the front end (remember that expansion proceeds
   --  at the same time as analysis).

   --  For an indexed reference to a packed array, we simply convert the
   --  reference to the appropriate equivalent reference to the object
   --  of the packed array type (using unchecked conversion).

   --  In some cases (for internally generated types, and for the subtypes
   --  for record fields that depend on a discriminant), the corresponding
   --  packed type cannot be easily generated in advance. In these cases,
   --  we generate the required subtype on the fly at the reference point.

   --  For the modular case, any unused bits are initialized to zero, and
   --  all operations maintain these bits as zero (where necessary all
   --  unchecked conversions from corresponding array values require
   --  these bits to be clear, which is done automatically by gigi).

   --  For the array cases, there can be unused bits in the last byte, and
   --  these are neither initialized, nor treated specially in operations
   --  (i.e. it is allowable for these bits to be clobbered, e.g. by not).

   ---------------------------
   -- Endian Considerations --
   ---------------------------

   --  The standard does not specify the way in which bits are numbered in
   --  a packed array. There are two reasonable rules for deciding this:

   --    Store the first bit at right end (low order) word. This means
   --    that the scaled subscript can be used directly as a right shift
   --    count (if we put bit 0 at the left end, then we need an extra
   --    subtract to compute the shift count.

   --    Layout the bits so that if the packed boolean array is overlaid on
   --    a record, using unchecked conversion, then bit 0 of the array is
   --    the same as the bit numbered bit 0 in a record representation
   --    clause applying to the record. For example:

   --       type Rec is record
   --          C : Bits4;
   --          D : Bits7;
   --          E : Bits5;
   --       end record;

   --       for Rec use record
   --          C at 0 range  0  .. 3;
   --          D at 0 range  4 .. 10;
   --          E at 0 range 11 .. 15;
   --       end record;

   --       type P16 is array (0 .. 15) of Boolean;
   --       pragma Pack (P16);

   --    Now if we use unchecked conversion to convert a value of the record
   --    type to the packed array type, according to this second criterion,
   --    we would expect field D to occupy bits 4..10 of the Boolean array.

   --  Although not required, this correspondence seems a highly desirable
   --  property, and is one that GNAT decides to guarantee. For a little
   --  endian machine, we can also meet the first requirement, but for a
   --  big endian machine, it will be necessary to store the first bit of
   --  a Boolean array in the left end (most significant) bit of the word.
   --  This may cost an extra instruction on some machines, but we consider
   --  that a worthwhile price to pay for the consistency.

   --  One more important point arises in the case where we have a constrained
   --  subtype of an unconstrained array. Take the case of 20-bits. For the
   --  unconstrained representation, we would use an array of bytes:

   --     Little-endian case
   --       8-7-6-5-4-3-2-1  16-15-14-13-12-11-10-9  x-x-x-x-20-19-18-17

   --     Big-endian case
   --       1-2-3-4-5-6-7-8  9-10-11-12-13-14-15-16  17-18-19-20-x-x-x-x

   --   For the constrained case, we use a 20-bit modular value, but in
   --   general this value may well be stored in 32 bits. Let's look at
   --   what it looks like:

   --     Little-endian case

   --       x-x-x-x-x-x-x-x-x-x-x-x-20-19-18-17-...-10-9-8-7-6-5-4-3-2-1

   --         which stored in memory looks like

   --       8-7-...-2-1  16-15-...-10-9  x-x-x-x-20-19-18-17  x-x-x-x-x-x-x

   --   An important rule is that the constrained and unconstrained cases
   --   must have the same bit representation in memory, since we will often
   --   convert from one to the other (e.g. when calling a procedure whose
   --   formal is unconstrained). As we see, that criterion is met for the
   --   little-endian case above. Now let's look at the big-endian case:

   --     Big-endian case

   --       x-x-x-x-x-x-x-x-x-x-x-x-1-2-3-4-5-6-7-8-9-10-...-17-18-19-20

   --         which stored in memory looks like

   --       x-x-x-x-x-x-x-x  x-x-x-x-1-2-3-4  5-6-...11-12  13-14-...-19-20

   --   That won't do, the representation value in memory is NOT the same in
   --   the constrained and unconstrained case. The solution is to store the
   --   modular value left-justified:

   --       1-2-3-4-5-6-7-8-9-10-...-17-18-19-20-x-x-x-x-x-x-x-x-x-x-x

   --         which stored in memory looks like

   --       1-2-...-7-8  9-10-...15-16  17-18-19-20-x-x-x-x  x-x-x-x-x-x-x-x

   --   and now, we do indeed have the same representation. The special flag
   --   Is_Left_Justified_Modular is set in the modular type used as the
   --   packed array type in the big-endian case to ensure that this required
   --   left justification occurs.

   -----------------
   -- Subprograms --
   -----------------

   procedure Create_Packed_Array_Type (Typ  : Entity_Id);
   --  Typ is a array type or subtype to which pragma Pack applies. If the
   --  Packed_Array_Type field of Typ is already set, then the call has no
   --  effect, otherwise a suitable type or subtype is created and stored
   --  in the Packed_Array_Type field of Typ. This created type is an Itype
   --  so that Gigi will simply elaborate and freeze the type on first use
   --  (which is typically the definition of the corresponding array type).
   --
   --  Note: although this routine is included in the expander package for
   --  packed types, it is actually called unconditionally from Freeze,
   --  whether or not expansion (and code generation) is enabled. We do this
   --  since we want gigi to be able to properly compute type charactersitics
   --  (for the Data Decomposition Annex of ASIS, and possible other future
   --  uses) even if code generation is not active. Strictly this means that
   --  this procedure is not part of the expander, but it seems appropriate
   --  to keep it together with the other expansion routines that have to do
   --  with packed array types.

   procedure Expand_Packed_Boolean_Operator (N : Node_Id);
   --  N is an N_Op_And, N_Op_Or or N_Op_Xor node whose operand type is a
   --  packed boolean array. This routine expands the appropriate operations
   --  to carry out the logical operation on the packed arrays. It handles
   --  both the modular and array representation cases.

   procedure Expand_Packed_Element_Reference (N : Node_Id);
   --  N is an N_Indexed_Component node whose prefix is a packed array. In
   --  the bit packed case, this routine can only be used for the expression
   --  evaluation case not the assignment case, since the result is not a
   --  variable. See Expand_Bit_Packed_Element_Set for how he assignment case
   --  is handled in the bit packed case. For the enumeration case, the result
   --  of this call is always a variable, so the call can be used for both the
   --  expression evaluation and assignment cases.

   procedure Expand_Bit_Packed_Element_Set (N : Node_Id);
   --  N is an N_Assignment_Statement node whose name is an indexed
   --  component of a bit-packed array. This procedure rewrites the entire
   --  assignment statement with appropriate code to set the referenced
   --  bits of the packed array type object. Note that this procedure is
   --  used only for the bit-packed case, not for the enumeration case.

   procedure Expand_Packed_Eq (N : Node_Id);
   --  N is an N_Op_Eq node where the operands are packed arrays whose
   --  representation is an array-of-bytes type (the case where a modular
   --  type is used for the representation does not require any special
   --  handling, because in the modular case, unused bits are zeroes.

   procedure Expand_Packed_Not (N : Node_Id);
   --  N is an N_Op_Not node where the operand is packed array of Boolean
   --  in standard representation (i.e. component size is one bit). This
   --  procedure expands the corresponding not operation. Note that the
   --  non-standard representation case is handled by using a loop through
   --  elements generated by the normal non-packed circuitry.

   function Involves_Packed_Array_Reference (N : Node_Id) return Boolean;
   --  N is the node for a name. This function returns true if the name
   --  involves a packed array reference. A node involves a packed array
   --  reference if it is itself an indexed compoment referring to a bit-
   --  packed array, or it is a selected component whose prefix involves
   --  a packed array reference.

   procedure Expand_Packed_Address_Reference (N : Node_Id);
   --  The node N is an attribute reference for the 'Address reference, where
   --  the prefix involves a packed array reference. This routine expands the
   --  necessary code for performing the address reference in this case.

end Exp_Pakd;
