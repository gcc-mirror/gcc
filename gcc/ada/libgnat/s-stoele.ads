------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the implementation dependent sections of this file.      --
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

--  This package provides the ``Storage_Elements`` package as defined by the
--  Ada ARM 13.7.1 to allow for arithmetic operations on memory addresses.

--  Warning: declarations in this package are ambiguous with respect to the
--  extra declarations that can be introduced into System using Extend_System.
--  It is a good idea to avoid use clauses for this package.

package System.Storage_Elements with
  Always_Terminates
is
   pragma Pure;
   --  Note that we take advantage of the implementation permission to make
   --  this unit Pure instead of Preelaborable; see RM 13.7.1(15). In Ada 2005,
   --  this is Pure in any case (AI-362).

   pragma No_Elaboration_Code_All;
   --  Allow the use of that restriction in units that WITH this unit

   type Storage_Offset is range -Memory_Size / 2 .. Memory_Size / 2 - 1;

   subtype Storage_Count is Storage_Offset range 0 .. Storage_Offset'Last;
   --  *Storage_Count* is a subtype of *Storage_Offset* with only the null or
   --  positive values.

   type Storage_Element is mod 2 ** Storage_Unit;
   for Storage_Element'Size use Storage_Unit;
   --  The type *Storage_Element* represents a byte

   pragma Universal_Aliasing (Storage_Element);
   --  This type is used by the expander to implement aggregate copy

   type Storage_Array is
     array (Storage_Offset range <>) of aliased Storage_Element;
   for Storage_Array'Component_Size use Storage_Unit;
   --  The type *Storage_Array* represents a part of the memory

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address;
   function "+" (Left : Storage_Offset; Right : Address) return Address;
   pragma Import (Intrinsic, "+");
   --   Returns the sum of ``Left`` and ``Right``

   function "-" (Left : Address; Right : Storage_Offset) return Address;
   function "-" (Left, Right : Address) return Storage_Offset;
   pragma Import (Intrinsic, "-");
   --  Returns the difference between ``Left`` and ``Right``

   function "mod"
     (Left  : Address;
      Right : Storage_Offset) return Storage_Offset;
   pragma Import (Intrinsic, "mod");
   --  Returns the modulus between ``Left`` and ``Right``

   --  Conversion to/from integers

   type Integer_Address is mod Memory_Size;
   --  Finally, *Integer_Address* is a non-private modular
   --  type that represents an address.

   function To_Address (Value : Integer_Address) return Address;
   pragma Import (Intrinsic, To_Address);
   --  Converts the *Integer_Address* ``Value`` to an *Address*

   function To_Integer (Value : Address) return Integer_Address;
   pragma Import (Intrinsic, To_Integer);
   --  Converts the *Address* ``Value`` to an *Integer_Address*

end System.Storage_Elements;
