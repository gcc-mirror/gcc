------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            I N T E R F A C E S . P A C K E D _ D E C I M A L             --
--                                                                          --
--                                 S p e c                                  --
--            (Version for IBM Mainframe Packed Decimal Format)             --
--                                                                          --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --

--  This unit defines the packed decimal format used by GNAT in response to
--  a specication of Machine_Radix 10 for a decimal fixed-point type. The
--  format and operations are completely encapsulated in this unit, so all
--  that is necessary to compile using different packed decimal formats is
--  to replace this single unit.

--  Note that the compiler access the spec of this unit during compilation
--  to obtain the data length that needs allocating, so the correct version
--  of the spec must be available to the compiler, and must correspond to
--  the spec and body made available to the linker, and all units of a given
--  program must be compiled with the same version of the spec and body.
--  This consistency will be enforced automatically using the normal binder
--  consistency checking, since any unit declaring Machine_Radix 10 types or
--  containing operations on such data will implicitly with Packed_Decimal.

with System;

package Interfaces.Packed_Decimal is

   ------------------------
   -- Format Description --
   ------------------------

   --  IBM Mainframe packed decimal format uses a byte string of length one
   --  to 10 bytes, with the most significant byte first. Each byte contains
   --  two decimal digits (with the high order digit in the left nibble, and
   --  the low order four bits contain the sign, using the following code:

   --     16#A#  2#1010#   positive
   --     16#B#  2#1011#   negative
   --     16#C#  2#1100#   positive (preferred representation)
   --     16#D#  2#1101#   negative (preferred representation)
   --     16#E#  2#1110#   positive
   --     16#F#  2#1011#   positive

   --  In this package, all six sign representations are interpreted as
   --  shown above when an operand is read, when an operand is written,
   --  the preferred representations are always used. Constraint_Error
   --  is raised if any other bit pattern is found in the sign nibble,
   --  or if a digit nibble contains an invalid digit code.

   --  Some examples follow:

   --     05 76 3C      +5763
   --     00 01 1D        -11
   --     00 04 4E        +44 (non-standard sign)
   --     00 00 00      invalid (incorrect sign nibble)
   --     0A 01 1C      invalid (bad digit)

   ------------------
   -- Length Array --
   ------------------

   --  The following array must be declared in exactly the form shown, since
   --  the compiler accesses the associated tree to determine the size to be
   --  allocated to a machine radix 10 type, depending on the number of digits.

   subtype Byte_Length is Positive range 1 .. 10;
   --  Range of possible byte lengths

   Packed_Size : constant array (1 .. 18) of Byte_Length :=
      (01 => 01,    -- Length in bytes for digits 1
       02 => 02,    -- Length in bytes for digits 2
       03 => 02,    -- Length in bytes for digits 2
       04 => 03,    -- Length in bytes for digits 2
       05 => 03,    -- Length in bytes for digits 2
       06 => 04,    -- Length in bytes for digits 2
       07 => 04,    -- Length in bytes for digits 2
       08 => 05,    -- Length in bytes for digits 2
       09 => 05,    -- Length in bytes for digits 2
       10 => 06,    -- Length in bytes for digits 2
       11 => 06,    -- Length in bytes for digits 2
       12 => 07,    -- Length in bytes for digits 2
       13 => 07,    -- Length in bytes for digits 2
       14 => 08,    -- Length in bytes for digits 2
       15 => 08,    -- Length in bytes for digits 2
       16 => 09,    -- Length in bytes for digits 2
       17 => 09,    -- Length in bytes for digits 2
       18 => 10);   -- Length in bytes for digits 2

   -------------------------
   -- Conversion Routines --
   -------------------------

   subtype D32 is Positive range 1 .. 9;
   --  Used to represent number of digits in a packed decimal value that
   --  can be represented in a 32-bit binary signed integer form.

   subtype D64 is Positive range 10 .. 18;
   --  Used to represent number of digits in a packed decimal value that
   --  requires a 64-bit signed binary integer for representing all values.

   function Packed_To_Int32 (P : System.Address; D : D32) return Integer_32;
   --  The argument P is the address of a packed decimal value and D is the
   --  number of digits (in the range 1 .. 9, as implied by the subtype).
   --  The returned result is the corresponding signed binary value. The
   --  exception Constraint_Error is raised if the input is invalid.

   function Packed_To_Int64 (P : System.Address; D : D64) return Integer_64;
   --  The argument P is the address of a packed decimal value and D is the
   --  number of digits (in the range 10 .. 18, as implied by the subtype).
   --  The returned result is the corresponding signed binary value. The
   --  exception Constraint_Error is raised if the input is invalid.

   procedure Int32_To_Packed (V : Integer_32; P : System.Address; D : D32);
   --  The argument V is a signed binary integer, which is converted to
   --  packed decimal format and stored using P, the address of a packed
   --  decimal item of D digits (D is in the range 1-9). Constraint_Error
   --  is raised if V is out of range of this number of digits.

   procedure Int64_To_Packed (V : Integer_64; P : System.Address; D : D64);
   --  The argument V is a signed binary integer, which is converted to
   --  packed decimal format and stored using P, the address of a packed
   --  decimal item of D digits (D is in the range 10-18). Constraint_Error
   --  is raised if V is out of range of this number of digits.

end Interfaces.Packed_Decimal;
