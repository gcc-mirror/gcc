------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               T T Y P E S                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

--  This package contains constants describing target properties

with Types;    use Types;
with Get_Targ; use Get_Targ;

package Ttypes is

   ------------------------------
   -- Host/Target Dependencies --
   ------------------------------

   --  It is vital to maintain a clear distinction between properties of
   --  types on the host and types on the target, since in the general
   --  case of a cross-compiler these will be different.

   --  This package and its companion Ttypef provide definitions of values
   --  that describe the properties of the target types. All instances of
   --  target dependencies, including the definitions of such packages as
   --  Standard and System depend directly or indirectly on the definitions
   --  in the Ttypes and Ttypef packages.

   --  In the source of the compiler, references to attributes such as
   --  Integer'Size will give information regarding the host types (i.e.
   --  the types within the compiler itself). Such references are therefore
   --  almost always suspicious (it is hard for example to see that the
   --  code in the compiler should even be using type Integer very much,
   --  and certainly this code should not depend on the size of Integer).

   --  On the other hand, it is perfectly reasonable for the compiler to
   --  require access to the size of type Integer for the target machine,
   --  e.g. in constructing the internal representation of package Standard.
   --  For this purpose, instead of referencing the attribute Integer'Size,
   --  a reference to Ttypes.Standard_Integer_Size will provide the needed
   --  value for the target type.

   --  Two approaches are used for handling target dependent values in the
   --  standard library packages. Package Standard is handled specially,
   --  being constructed internally (by package Stand). Target dependent
   --  values needed in Stand are obtained by direct reference to Ttypes
   --  and Ttypef.

   --  For package System, the required constant values are obtained by
   --  referencing appropriate attributes. Ada 95 already defines most of
   --  the required attributes, and GNAT specific attributes have been
   --  defined to cover the remaining cases (such as Storage_Unit). The
   --  evaluation of these attributes obtains the required target dependent
   --  values from Ttypes and Ttypef. The additional attributes that have
   --  been added to GNAT (Address_Size, Storage_Unit, Word_Size, Max_Priority,
   --  and Max_Interrupt_Priority) are for almost all purposes redundant with
   --  respect to the corresponding references to System constants. For example
   --  in a program, System.Address_Size and Standard'Address_Size yield the
   --  same value. The critical use of the attribute is in writing the System
   --  declaration of Address_Size which of course cannot refer to itself. By
   --  this means we achieve complete target independence in the source code
   --  of package System, i.e. there is only one copy of the source of System
   --  for all targets.

   --  Note that during compilation there are two versions of package System
   --  around. The version that is directly with'ed by compiler packages
   --  contains host-dependent definitions, which is what is needed in that
   --  case (for example, System.Storage_Unit referenced in the source of the
   --  compiler refers to the storage unit of the host, not the target). This
   --  means that, like attribute references, any references to constants in
   --  package System in the compiler code are suspicious, since it is strange
   --  for the compiler to have such host dependencies. If the compiler needs
   --  to access the target dependent values of such quantities as Storage_Unit
   --  then it should reference the constants in this package (Ttypes), rather
   --  than referencing System.Storage_Unit, or Standard'Storage_Unit, both of
   --  which would yield the host value.

   ---------------------------------------------------
   -- Target-Dependent Values for Types in Standard --
   ---------------------------------------------------

   --  Note: GNAT always supplies all the following integer and float types,
   --  but depending on the machine, some of the types may be identical. For
   --  example, on some machines, Short_Float may be the same as Float, and
   --  Long_Long_Float may be the same as Long_Float.

   Standard_Short_Short_Integer_Size  : constant Pos := Get_Char_Size;
   Standard_Short_Short_Integer_Width : constant Pos :=
                                          Width_From_Size
                                           (Standard_Short_Short_Integer_Size);

   Standard_Short_Integer_Size        : constant Pos := Get_Short_Size;
   Standard_Short_Integer_Width       : constant Pos :=
                                          Width_From_Size
                                            (Standard_Short_Integer_Size);

   Standard_Integer_Size              : constant Pos := Get_Int_Size;
   Standard_Integer_Width             : constant Pos :=
                                          Width_From_Size
                                            (Standard_Integer_Size);

   Standard_Long_Integer_Size         : constant Pos := Get_Long_Size;
   Standard_Long_Integer_Width        : constant Pos :=
                                          Width_From_Size
                                            (Standard_Long_Integer_Size);

   Standard_Long_Long_Integer_Size    : constant Pos := Get_Long_Long_Size;
   Standard_Long_Long_Integer_Width   : constant Pos :=
                                          Width_From_Size
                                            (Standard_Long_Long_Integer_Size);

   Standard_Short_Float_Size          : constant Pos := Get_Float_Size;
   Standard_Short_Float_Digits        : constant Pos :=
                                          Digits_From_Size
                                            (Standard_Short_Float_Size);

   Standard_Float_Size                : constant Pos := Get_Float_Size;
   Standard_Float_Digits              : constant Pos :=
                                          Digits_From_Size
                                            (Standard_Float_Size);

   Standard_Long_Float_Size           : constant Pos := Get_Double_Size;
   Standard_Long_Float_Digits         : constant Pos :=
                                          Digits_From_Size
                                            (Standard_Long_Float_Size);

   Standard_Long_Long_Float_Size      : constant Pos := Get_Long_Double_Size;
   Standard_Long_Long_Float_Digits    : constant Pos :=
                                          Digits_From_Size
                                            (Standard_Long_Long_Float_Size);

   Standard_Character_Size            : constant Pos := Get_Char_Size;

   Standard_Wide_Character_Size       : constant Pos := 16;
   Standard_Wide_Wide_Character_Size  : constant Pos := 32;
   --  Standard wide character sizes

   --  Note: there is no specific control over the representation of
   --  enumeration types. The convention used is that if an enumeration
   --  type has fewer than 2**(Character'Size) elements, then the size
   --  used is Character'Size, otherwise Integer'Size is used.

   --  Similarly, the size of fixed-point types depends on the size of the
   --  corresponding integer type, which is the smallest predefined integer
   --  type capable of representing the required range of values.

   -------------------------------------------------
   -- Target-Dependent Values for Types in System --
   -------------------------------------------------

   System_Address_Size : constant Pos := Get_Pointer_Size;
   --  System.Address'Size (also size of all thin pointers)

   System_Max_Binary_Modulus_Power : constant Pos :=
                                       Standard_Long_Long_Integer_Size;

   System_Max_Nonbinary_Modulus_Power : constant Pos := Standard_Integer_Size;

   System_Storage_Unit : constant Pos := Get_Bits_Per_Unit;
   System_Word_Size    : constant Pos := Get_Bits_Per_Word;

   System_Tick_Nanoseconds : constant Pos := 1_000_000_000;
   --  Value of System.Tick in nanoseconds. At the moment, this is a fixed
   --  constant (with value of 1.0 seconds), but later we should add this
   --  value to the GCC configuration file so that its value can be made
   --  configuration dependent.

   -----------------------------------------------------
   -- Target-Dependent Values for Types in Interfaces --
   -----------------------------------------------------

   Interfaces_Wchar_T_Size : constant Pos := Get_Wchar_T_Size;

   ----------------------------------------
   -- Other Target-Dependent Definitions --
   ----------------------------------------

   Maximum_Alignment : constant Pos := Get_Maximum_Alignment;
   --  The maximum alignment, in storage units, that an object or type may
   --  require on the target machine.

   System_Allocator_Alignment : constant Pos :=
                                  Get_System_Allocator_Alignment;
   --  The alignment in storage units of addresses returned by malloc

   Max_Unaligned_Field : constant Pos := Get_Max_Unaligned_Field;
   --  The maximum supported size in bits for a field that is not aligned
   --  on a storage unit boundary.

   Bytes_Big_Endian : Boolean := Get_Bytes_BE /= 0;
   --  Important note: for Ada purposes, the important setting is the bytes
   --  endianness (Bytes_Big_Endian), not the bits value (Bits_Big_Endian).
   --  This is because Ada bit addressing must be compatible with the byte
   --  ordering (otherwise we would end up with non-contiguous fields). It
   --  is rare for the two to be different, but if they are, Bits_Big_Endian
   --  is relevant only for the generation of instructions with bit numbers,
   --  and thus relevant only to the back end. Note that this is a variable
   --  rather than a constant, since it can be modified (flipped) by -gnatd8.

   Target_Strict_Alignment : Boolean := Get_Strict_Alignment /= 0;
   --  True if instructions will fail if data is misaligned

   Target_Double_Float_Alignment : Nat := Get_Double_Float_Alignment;
   --  The default alignment of "double" floating-point types, i.e. floating
   --  point types whose size is equal to 64 bits, or 0 if this alignment is
   --  not specifically capped.

   Target_Double_Scalar_Alignment : Nat := Get_Double_Scalar_Alignment;
   --  The default alignment of "double" or larger scalar types, i.e. scalar
   --  types whose size is greater or equal to 64 bits, or 0 if this alignment
   --  is not specifically capped.

end Ttypes;
