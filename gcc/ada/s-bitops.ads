------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B I T _ O P S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999, Free Software Foundation, Inc.         --
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
------------------------------------------------------------------------------

--  Operations on packed bit strings

with System;

package System.Bit_Ops is

   --  Note: in all the following routines, the System.Address parameters
   --  represent the address of the first byte of an array used to represent
   --  a packed array (of type System.Unsigned_Types.Packed_Bytes{1,2,4})
   --  The length in bits is passed as a separate parameter.

   procedure Bit_And
     (Left   : System.Address;
      Llen   : Natural;
      Right  : Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "and" of given bit string with result being placed in Result.
   --  The or operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

   function Bit_Eq
     (Left  : System.Address;
      Llen  : Natural;
      Right : System.Address;
      Rlen  : Natural)
      return  Boolean;
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
      Right  : Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "or" of given bit string with result being placed in Result.
   --  The or operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

   procedure Bit_Xor
     (Left   : System.Address;
      Llen   : Natural;
      Right  : Address;
      Rlen   : Natural;
      Result : System.Address);
   --  Bitwise "xor" of given bit string with result being placed in Result.
   --  The or operation is allowed to destroy unused bits in the last byte,
   --  i.e. to leave them set in an undefined manner. Note that Left, Right
   --  and Result always have the same length in bits (Len).

end System.Bit_Ops;
