------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                       S Y S T E M . B I T _ O P S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  Operations on packed bit strings

pragma Compiler_Unit;

with System;

package System.Bit_Ops is

   --  Note: in all the following routines, the System.Address parameters
   --  represent the address of the first byte of an array used to represent
   --  a packed array (of type System.Unsigned_Types.Packed_Bytes{1,2,4})
   --  The length in bits is passed as a separate parameter. Note that all
   --  addresses must be of byte aligned arrays.

   procedure Bit_And
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "and" of given bit string with result being placed in Result.
   --  The and operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

   function Bit_Eq
     (Left  : System.Address;
      Llen  : Natural;
      Right : System.Address;
      Rlen  : Natural) return Boolean;
   --  Left and Right are the addresses of two bit packed arrays with Llen
   --  and Rlen being the respective length in bits. The routine compares the
   --  two bit strings for equality, being careful not to include the unused
   --  bits in the final byte. Note that the result is always False if Rlen
   --  is not equal to Llen.

   procedure Bit_Not
     (Opnd   : System.Address;
      Len    : Natural;
      Result : System.Address);
   --  Bitwise "not" of given bit string with result being placed in Result.
   --  The not operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Result and
   --  Opnd always have the same length in bits (Len).

   procedure Bit_Or
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "or" of given bit string with result being placed in Result.
   --  The or operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

   procedure Bit_Xor
     (Left   : System.Address;
      Llen   : Natural;
      Right  : System.Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "xor" of given bit string with result being placed in Result.
   --  The xor operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

end System.Bit_Ops;
